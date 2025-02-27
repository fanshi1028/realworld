{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module StateMachine (stateMachine) where

import Client (apisClient, getTagsClient, getTagsStreamClient, loginClient, registerClient)
import Control.Lens ((%~))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Authentication.HasAuth (AuthOf (..), LoginOf (UserLogin))
import Data.Domain.Transform (transform)
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import Data.Field.Slug (titleToSlug)
import Data.Functor.Classes (Eq1, Ord1)
import Data.Generics.Product (HasField' (field'), getField)
import qualified Data.Semigroup as SG (Last (Last, getLast))
import Data.Set (delete, insert, isSubsetOf)
import qualified Data.Set as S (empty, foldl', insert, map, member)
import Data.Storage.Map.HasStorage (IdOf (..), toArticleId, toUserId)
import Data.Storage.Map.HasCreate (CreateOf (..))
import Data.Authentication.HasToken (TokenOf (UserToken))
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out (Out))
import Data.Util.Validation (WithValidation)
import Gen.Naive ()
import Orphans.Ord.Data.Field.Tag ()
import Orphans.Ord.Data.Storage.Map.HasStorage ()
import Relude.Extra (un, (^.))
import Servant (Headers (Headers), type (:<|>) ((:<|>)))
import Servant.Client (ClientEnv, ClientError)
import Servant.Client.Streaming (ClientM, withClientM)
import Servant.Types.SourceT (runSourceT)
import StateMachine.Gen (generator, shrinker)
import StateMachine.Types
  ( AuthCommand (..),
    AuthResponse (..),
    Command (..),
    Model (..),
    Response (..),
    StreamMode (HasStreaming, NoStreaming),
    UserCommand (..),
    UserResponse (..),
    VisitorCommand (..),
    VisitorResponse (..),
  )
import StateMachine.Util (deleteByRef, findByRef, findByRef2, findByRef2All, findByRefAll)
import Test.StateMachine
  ( Concrete,
    GenSym,
    Logic (Boolean, Bot, Forall, Not, Top),
    StateMachine (StateMachine),
    Symbolic,
    concrete,
    genSym,
    noCleanup,
    notMember,
    reference,
    (.&&),
    (.//),
    (.==),
  )
import Test.StateMachine.Logic (member)
import Validation (Validation (Failure, Success))

initModel :: Model r
initModel = Model mempty mempty mempty mempty mempty S.empty S.empty S.empty S.empty S.empty mempty S.empty

transition :: (Eq1 r, Ord1 r) => Model r -> Command r -> Response r -> Model r
transition m cm res = case (cm, res) of
  (_, FailResponse _) -> m
  (AuthCommand mref cm', AuthResponse res') -> case (mref, cm', res') of
    -- FIXME: Register when login?
    (_, Register cr, Registered ref em pw t) ->
      m & field' @"users" %~ ((ref, cr) :)
        & field' @"emails" %~ ((ref, em) :)
        & field' @"credentials" %~ ((em, pw) :)
        & field' @"tokens" %~ ((t, em) :)
    -- FIXME: How about double login with different identity?
    (_, Login _ _, LoggedIn) -> m
    _failed -> m
  (VisitorCommand _ _, _) -> m
  (UserCommand (Just t) cm', UserResponse res') -> fromMaybe m $ do
    em <- findByRef t $ tokens m
    ref <- findByRef2 em $ emails m
    pw <- findByRef em $ credentials m
    pure $ case (cm', res') of
      (GetCurrentUser, _) -> m
      -- FIXME: JWT suck as session token, update profile will invalidate the login session
      (UpdateUser ur, UpdatedUser t' m_uid m_em m_pw) ->
        let em' = fromMaybe em m_em
            ref' = fromMaybe ref m_uid
            pw' = fromMaybe pw m_pw
         in m
              & field' @"users"
                %~ fmap
                  ( \orig@(u, cr) ->
                      let cr' =
                            cr
                              & field' @"email" %~ case getField @"email" ur of
                                Nothing -> Prelude.id
                                Just (SG.Last new_em) -> const new_em
                              & field' @"username" %~ case getField @"username" ur of
                                Nothing -> Prelude.id
                                Just (SG.Last new_un) -> const new_un
                       in if u == ref then (ref', cr') else orig
                  )
              & field' @"tokens" %~ ((t', em') :) . deleteByRef t
              & field' @"emails" %~ ((ref', em') :) . deleteByRef ref
              & field' @"credentials" %~ ((em', pw') :) . deleteByRef em
              & field' @"userFollowUser"
                %~ S.map
                  ( \case
                      us@(u1, u2)
                        | u1 == ref -> (ref', u2)
                        | u2 == ref -> (u1, ref')
                        | otherwise -> us
                  )
              & field' @"userFavoriteArticle" %~ S.map (\ua@(u, a) -> if u == ref then (ref', a) else ua)
              & field' @"userCreateComment" %~ S.map (\uc@(u, c) -> if u == ref then (ref', c) else uc)
              & field' @"userCreateArticle" %~ S.map (\ua@(u, a) -> if u == ref then (ref', a) else ua)
      (FollowUser ref', FollowedUser) -> m & field' @"userFollowUser" %~ insert (ref, ref')
      (UnfollowUser ref', UnfollowedUser) -> m & field' @"userFollowUser" %~ delete (ref, ref')
      (CreateArticle cr, CreatedArticle ref') ->
        m
          & field' @"articles" %~ ((ref', cr) :)
          & field' @"userCreateArticle" %~ insert (ref, ref')
          & field' @"tags" %~ (<> (cr ^. field' @"tagList" & fromList))
      (UpdateArticle ref' au, UpdatedArticle m_aid) ->
        case m_aid of
          Nothing -> m
          Just aid ->
            m
              & field' @"userCreateArticle" %~ insert (ref, aid) . delete (ref, ref')
              & field' @"userFavoriteArticle" %~ S.map (\orig@(u, o_aid) -> if o_aid == ref' then (u, aid) else orig)
              & field' @"articleHasComment" %~ S.map (\orig@(o_aid, c) -> if o_aid == ref' then (aid, c) else orig)
              & field' @"articles" %~ case findByRef ref' (articles m) of
                Nothing -> error "impossible"
                Just a ->
                  let a' =
                        a & field' @"title" %~ case getField @"title" au of
                          Nothing -> Prelude.id
                          Just (SG.Last new_tt) -> const new_tt
                   in ((aid, a') :) . deleteByRef ref'
      (DeleteArticle ref', DeletedArticle) ->
        m
          & field' @"articles" %~ deleteByRef ref'
          & field' @"userCreateArticle" %~ delete (ref, ref')
          & field' @"userFavoriteArticle" %~ S.foldl' (\acc orig@(_, o_aid) -> if o_aid == ref' then acc else S.insert orig acc) S.empty
          & field' @"articleHasComment" %~ S.foldl' (\acc orig@(o_aid, _) -> if o_aid == ref' then acc else S.insert orig acc) S.empty
      (AddCommentToArticle ref' c, AddedCommentToArticle ref'') ->
        m
          & field' @"comments" %~ ((ref'', c) :)
          & field' @"articleHasComment" %~ insert (ref', ref'')
      (DeleteComment ref' ref'', DeletedComment) ->
        m
          & field' @"comments" %~ deleteByRef ref''
          & field' @"articleHasComment" %~ delete (ref', ref'')
      (FavoriteArticle ref', FavoritedArticle) -> m & field' @"userFavoriteArticle" %~ insert (ref, ref')
      (UnfavoriteArticle ref', UnfavoritedArticle) -> m & field' @"userFavoriteArticle" %~ delete (ref, ref')
      (FeedArticles _, _) -> m
      _failed -> m
  _failed -> m

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition _ _ = Top

postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition m cmd res =
  let m' = transition m cmd res

      tokenToUser tokenRef mm = findByRef tokenRef (tokens mm) >>= (`findByRef2` emails m) >>= (`findByRef` users mm)
      validToken = Boolean . isJust <<$>> tokenToUser
      findByRef' ref = Boolean . isJust . findByRef ref

      usersL = length . users
      emailsL = length . emails
      credentialsL = length . credentials
      articlesL = length . articles
      commentsL = length . comments
      followL = length . userFollowUser
      favoriteL = length . userFavoriteArticle
      hasCommentL = length . articleHasComment
      createArticleL = length . userCreateArticle
      tokensL = length . tokens
      tagsL = length . tags

      allL = [usersL, emailsL, credentialsL, articlesL, commentsL, followL, favoriteL, hasCommentL, createArticleL, tokensL, tagsL]
      authInv = [articlesL, commentsL, followL, favoriteL, hasCommentL, createArticleL, tagsL]
      followInv = [emailsL, credentialsL, usersL, articlesL, commentsL, favoriteL, hasCommentL, createArticleL, tokensL, tagsL]
      articleInv = [emailsL, credentialsL, usersL, followL, tokensL]
      commentInv = [emailsL, credentialsL, usersL, articlesL, followL, favoriteL, createArticleL, tokensL, tagsL]
      favoriteInv = [emailsL, credentialsL, usersL, articlesL, commentsL, followL, hasCommentL, createArticleL, tokensL, tagsL]
   in case (cmd, res) of
        (_, FailResponse _) -> m .== m' .// "same model"
        (AuthCommand _m_ref ac, AuthResponse ar) ->
          Boolean (and $ on (==) <$> authInv ?? m ?? m') .// "auth command invariant"
            .&& case (ac, ar) of
              (Register cr, Registered ref em pw _t) ->
                findByRef ref (users m) .== Nothing .// "before: the user not existed"
                  .&& findByRef ref (users m') .== Just cr .// "after: the user exists"
                  .&& findByRef ref (emails m) .== Nothing .// "before: the email not existed"
                  .&& findByRef ref (emails m') .== Just em .// "after: the email exists"
                  .&& findByRef em (credentials m) .== Nothing .// "before: the credential not existed"
                  .&& findByRef em (credentials m') .== Just pw .// "after: the credential exists"
                  .&& on (-) usersL m' m .== 1 .// "after: added 1 to users"
                  .&& on (-) emailsL m' m .== 1 .// "after: added 1 to emails"
                  .&& on (-) credentialsL m' m .== 1 .// "after: added 1 to credentials"
                  .&& on (-) tokensL m' m .== 1 .// "after: new token, added 1 to tokensL"
              (Login _ _, LoggedIn) -> Top
              _ -> error "auth postcondition error"
        (VisitorCommand _ vc, VisitorResponse vr) ->
          m .== m' .// "same model" .&& case (vc, vr) of
            (GetProfile ref', GotProfile) -> Forall [m, m'] (findByRef' ref' . users) .// "the user exist"
            (GetArticle ref', GotArticle) -> Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
            (ListArticles {}, ListedArticles) -> Top
            (GetTags _, GotTags) -> Top
            (GetComments ref' _, GotComments) -> Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
            _ -> error "visitor postcondition error"
        (UserCommand (Just ref) uc, UserResponse ur) -> fromMaybe (Bot .// "the token is valid") $ do
          em <- findByRef ref $ tokens m
          ref'' <- findByRef2 em $ emails m
          pure $
            validToken ref m .// "the token is valid" .&& case (uc, ur) of
              (GetCurrentUser, GotCurrentUser) -> m .== m' .// "same model"
              (UpdateUser _, UpdatedUser t m_uid m_em m_pw) ->
                let em' = fromMaybe em m_em
                    uid = fromMaybe ref'' m_uid
                 in member (ref'', em) (emails m) .// "before: old email in emails"
                      .&& member (uid, em') (emails m') .// "after: maybe updated user name or email in emails"
                      .&& member (ref, em) (tokens m) .// "before: old token and email in tokens"
                      .&& member (t, em') (tokens m') .// "after: maybe updated token or email in tokens"
                      .&& Boolean (and $ on (==) <$> allL ?? m ?? m') .// "update user invariant"
                      .&& maybe Top (\pw -> member (em', pw) (credentials m')) m_pw .// "after: maybe updated credential in credentials"
              (FollowUser ref', FollowedUser) ->
                Forall [m, m'] (findByRef' ref' . users) .// "the user exists"
                  .&& member (ref'', ref') (userFollowUser m') .// "after: followed the user"
                  .&& Boolean (and $ on (==) <$> followInv ?? m ?? m') .// "follow invariant"
              (UnfollowUser ref', UnfollowedUser) ->
                Forall [m, m'] (findByRef' ref' . users) .// "the user exists"
                  .&& notMember (ref'', ref') (userFollowUser m') .// "after: unfollowed the user"
                  .&& Boolean (and $ on (==) <$> followInv ?? m ?? m') .// "follow invariant"
              (CreateArticle ArticleCreate {tagList}, CreatedArticle ref') ->
                findByRef' ref' (articles m') .// "after: the article exists"
                  .&& Not (findByRef' ref' $ articles m) .// "before: the article not existed"
                  .&& member (ref'', ref') (userCreateArticle m') .// "after: user created the article"
                  .&& notMember (ref'', ref') (userCreateArticle m) .// "before: user didn't created the article"
                  .&& on (-) articlesL m' m .== 1 .// "after: added 1 to articles"
                  .&& on (-) createArticleL m' m .== 1 .// "after: added 1 to userCreateArticle"
                  .&& Boolean (fromList tagList `isSubsetOf` tags m') .// "after: have tags"
                  .&& Boolean (and $ on (==) <$> commentsL : favoriteL : hasCommentL : articleInv ?? m ?? m') .// "article invariant"
              (UpdateArticle ref' _, UpdatedArticle m_ref) ->
                let new_aid = fromMaybe ref' m_ref
                 in findByRef' ref' (articles m) .// "before: the article exists"
                      .&& findByRef' new_aid (articles m') .// "after: the article(maybe with new id) exists"
                      .&& member (ref'', ref') (userCreateArticle m) .// "before: user creates the article"
                      .&& member (ref'', new_aid) (userCreateArticle m') .// "after: user creates the article(maybe with new id)"
                      .&& findByRef2All ref' (userFavoriteArticle m) .== findByRef2All new_aid (userFavoriteArticle m') .// "has same favorite"
                      .&& findByRefAll ref' (articleHasComment m) .== findByRefAll new_aid (articleHasComment m') .// "has same comments"
                      .&& Boolean (and $ on (==) <$> allL ?? m ?? m') .// "article invariant"
              (DeleteArticle ref', DeletedArticle) ->
                findByRef' ref' (articles m) .// "before: the article existed"
                  .&& Not (findByRef' ref' $ articles m') .// "after: the article not exists"
                  .&& member (ref'', ref') (userCreateArticle m) .// "before: user created the article"
                  .&& notMember (ref'', ref') (userCreateArticle m') .// "after: user not created the article"
                  .&& on (-) articlesL m m' .== 1 .// "after: removed 1 from article"
                  .&& on (-) createArticleL m m' .== 1 .// "after: removed 1 from userCreateArticle"
                  .&& Boolean (and $ on (==) <$> articleInv ?? m ?? m') .// "article invariant"
              (AddCommentToArticle ref' _, AddedCommentToArticle ref''') ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& findByRef' ref''' (comments m') .// "after: the comment exists"
                  .&& Not (findByRef' ref''' $ comments m) .// "before: the comment not existed"
                  .&& notMember (ref', ref''') (articleHasComment m) .// "before: article didn't has the comment"
                  .&& member (ref', ref''') (articleHasComment m') .// "after: article has the comment"
                  .&& on (-) commentsL m' m .== 1 .// "after: added 1 to comments"
                  .&& on (-) hasCommentL m' m .== 1 .// "after: added 1 to articleHasComment"
                  .&& Boolean (and $ on (==) <$> commentInv ?? m ?? m') .// "comment invariant"
              (DeleteComment ref' ref''', DeletedComment) ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& findByRef' ref''' (comments m) .// "before: the comment exists"
                  .&& Not (findByRef' ref''' $ comments m') .// "after: the comment not existed"
                  .&& member (ref', ref''') (articleHasComment m) .// "before: article has the comment"
                  .&& notMember (ref', ref''') (articleHasComment m') .// "after: article didn't has the comment"
                  .&& on (-) commentsL m m' .== 1 .// "after: remove 1 from comments"
                  .&& on (-) hasCommentL m m' .== 1 .// "after: remove 1 from aritcleHasComment"
                  .&& Boolean (and $ on (==) <$> commentInv ?? m ?? m') .// "comment invariant"
              (FavoriteArticle ref', FavoritedArticle) ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& member (ref'', ref') (userFavoriteArticle m') .// "after: user favorites the article"
                  .&& Boolean (and $ on (==) <$> favoriteInv ?? m ?? m') .// "favorite invariant"
              (UnfavoriteArticle ref', UnfavoritedArticle) ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& notMember (ref'', ref') (userFavoriteArticle m') .// "after: user unfavorites the article"
                  .&& Boolean (and $ on (==) <$> favoriteInv ?? m ?? m') .// "favorite invariant"
              (FeedArticles _, FeededArticles) -> m .== m' .// "same model"
              _ -> error "user postcondition error"
        _ -> error "postcondition error"

mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock m =
  let -- HACK
      validate :: forall a. (FromJSON (WithValidation a), ToJSON a) => a -> Either String a
      validate a =
        eitherDecode @(WithValidation a) (encode a) >>= \case
          Failure err -> Left $ show err
          Success a' -> pure a'
   in \case
        AuthCommand _m_ref ac -> maybe (pure $ FailResponse "") (AuthResponse <$>) $
          case ac of
            Register cr -> do
              _ <- rightToMaybe $ validate cr
              guard $ all ((/= toUserId cr) . UserId . getField @"username" . snd) $ users m
              guard $ all ((/= getField @"email" cr) . getField @"email" . snd) $ users m
              pure $ Registered <$> genSym <*> genSym <*> genSym <*> genSym
            Login em pw -> do
              guard $ (em, pw) `elem` credentials m
              pure $ pure LoggedIn
        VisitorCommand _ vc -> maybe (pure $ FailResponse "") (VisitorResponse <$>) $ case vc of
          GetProfile _ -> pure $ pure GotProfile
          GetArticle ref -> do
            _ <- findByRef ref $ articles m
            pure $ pure GotArticle
          ListArticles {} -> pure $ pure ListedArticles
          GetTags _ -> pure $ pure GotTags
          GetComments ref _ -> do
            _ <- findByRef ref $ articles m
            pure $ pure GotComments
        UserCommand m_ref uc -> maybe (pure $ FailResponse "") (UserResponse <$>) $ do
          uRef <- m_ref >>= (`findByRef` tokens m) >>= (`findByRef2` emails m)
          o_uc <- uRef `findByRef` users m
          case uc of
            GetCurrentUser -> pure $ pure GotCurrentUser
            UpdateUser u -> do
              let validate' v = case v of
                    Nothing -> pure Nothing
                    Just (SG.Last a) -> case validate a of
                      Left _ -> Nothing
                      Right _ -> pure $ pure a
                  maybeGenSym Nothing = pure Nothing
                  maybeGenSym (Just _) = Just <$> genSym
                  o_em = getField @"email" o_uc
                  o_un = getField @"username" o_uc
              _ <- validate' $ getField @"bio" u
              _ <- validate' $ getField @"image" u
              m_uid <- validate' $ getField @"username" u
              m_em <- validate' $ getField @"email" u
              m_pw <- validate' $ getField @"password" u
              case m_uid of
                Just new_un
                  | o_un == new_un -> pure ()
                  | otherwise -> guard $ all (\(_, getField @"username" -> un') -> un' /= new_un) $ users m
                Nothing -> pure ()
              case m_em of
                Just new_em
                  | o_em == new_em -> pure ()
                  | otherwise -> guard $ all (\(_, getField @"email" -> em) -> em /= new_em) $ users m
                Nothing -> pure ()
              pure $
                UpdatedUser <$> genSym
                  <*> maybeGenSym m_uid
                  <*> maybeGenSym m_em
                  <*> maybeGenSym m_pw
            FollowUser ref -> do
              _ <- findByRef ref $ users m
              pure $ pure FollowedUser
            UnfollowUser ref -> do
              _ <- findByRef ref $ users m
              pure $ pure UnfollowedUser
            CreateArticle ca -> do
              _ <- rightToMaybe $ validate ca
              guard $ all (((/=) `on` toArticleId) ca . snd) $ articles m
              pure $ CreatedArticle <$> genSym
            UpdateArticle ref ar -> do
              let validate' v = case v of
                    Nothing -> pure Nothing
                    Just (SG.Last a) -> case validate a of
                      Left _ -> Nothing
                      Right _ -> pure $ pure a
                  maybeGenSym a = maybe (pure Nothing) (const $ Just <$> genSym) a
              m_tt <- validate' $ getField @"title" ar
              _ <- validate' $ getField @"description" ar
              _ <- validate' $ getField @"body" ar
              orig_tt <- getField @"title" <$> findByRef ref (articles m)
              m_tt >>= \case
                new_tt
                  | orig_tt == new_tt -> pure ()
                  | otherwise -> guard $ all (\(_, getField @"title" -> tt) -> tt /= new_tt) $ articles m
              guard $ (uRef, ref) `S.member` userCreateArticle m
              pure $ UpdatedArticle <$> maybeGenSym m_tt
            DeleteArticle ref -> do
              _ <- findByRef ref $ articles m
              guard $ (uRef, ref) `S.member` userCreateArticle m
              pure $ pure DeletedArticle
            AddCommentToArticle ref _ -> do
              _ <- findByRef ref $ articles m
              pure $ AddedCommentToArticle <$> genSym
            DeleteComment ref ref' -> do
              _ <- findByRef ref $ articles m
              guard $ (ref, ref') `S.member` articleHasComment m
              guard $ (uRef, ref') `S.member` userCreateComment m
              pure $ pure DeletedComment
            FavoriteArticle ref -> do
              _ <- findByRef ref $ articles m
              pure $ pure FavoritedArticle
            UnfavoriteArticle ref -> do
              _ <- findByRef ref $ articles m
              pure $ pure UnfavoritedArticle
            FeedArticles _ -> pure $ pure FeededArticles

semantics :: Command Concrete -> ReaderT ClientEnv IO (Response Concrete)
semantics =
  let run' :: forall a. ClientM a -> ReaderT ClientEnv IO (Either ClientError a)
      run' req = ask >>= liftIO . flip (withClientM req) pure

      runNoContent req res = run' req >>= either (pure . FailResponse . show) (const $ pure res)

      run req f =
        run' req >>= \case
          Left ce -> pure $ FailResponse $ show ce
          Right (Out r) -> pure $ f r

      runStream req f =
        ask >>= \env -> liftIO $
          withClientM req env $ \case
            Left ce -> pure $ FailResponse $ show ce
            Right stream ->
              runExceptT (runSourceT stream) <&> \case
                Left ce -> FailResponse $ fromString ce
                Right r -> f r

      runByCases noStreamClient hasStreamClient = \case
        NoStreaming -> run noStreamClient
        HasStreaming -> runStream hasStreamClient
   in \case
        AuthCommand _m_ref ac ->
          case ac of
            Register cr ->
              run (registerClient $ In $ pure cr) $ \(UserAuthWithToken u t) ->
                AuthResponse $
                  Registered
                    (reference $ toUserId u)
                    (reference $ getField @"email" cr)
                    (reference $ getField @"password" cr)
                    $ reference t
            Login em pw ->
              run' (loginClient $ In $ pure $ UserLogin (concrete em) $ concrete pw) >>= \case
                Left ce -> pure $ FailResponse $ show ce
                Right (Headers (Out _) _) -> pure $ AuthResponse LoggedIn
        VisitorCommand m_ref vc ->
          let (getProfile :<|> (listArticles :<|> withArticle)) :<|> _ = apisClient $ maybe (UserToken "") concrete m_ref
           in case vc of
                GetProfile ref -> run (getProfile $ pure $ concrete ref) $ const $ VisitorResponse GotProfile
                GetArticle ref ->
                  let getArticle :<|> _ = withArticle $ pure $ concrete ref
                   in run getArticle $ const $ VisitorResponse GotArticle
                -- FIXME: gen query param ?
                ListArticles (fmap pure -> mTags) ((pure . un . concrete <$>) -> mAuthor) ((pure . un . concrete <$>) -> mFav) streamMode ->
                  let listArticlesNoStream :<|> listArticlesStream = listArticles mTags mAuthor mFav Nothing Nothing
                   in runByCases listArticlesNoStream listArticlesStream streamMode $ const $ VisitorResponse ListedArticles
                GetTags streamMode -> runByCases getTagsClient getTagsStreamClient streamMode $ const $ VisitorResponse GotTags
                GetComments ref streamMode ->
                  let _ :<|> (getComments :<|> getCommentsStream) = withArticle $ pure $ concrete ref
                   in runByCases getComments getCommentsStream streamMode $ const $ VisitorResponse GotComments
        UserCommand m_ref uc ->
          case m_ref of
            Nothing -> pure $ FailResponse ""
            Just ref ->
              let _ :<|> (getCurrentUser :<|> updateUser) :<|> withUser :<|> feedArticles :<|> createArticle :<|> withArticle = apisClient $ concrete ref
               in case uc of
                    GetCurrentUser -> run getCurrentUser $ const $ UserResponse GotCurrentUser
                    UpdateUser ur -> run (updateUser $ In $ pure ur) $ \(UserAuthWithToken _ t) ->
                      UserResponse $
                        let m_uid = getField @"username" ur <&> reference . UserId . SG.getLast
                            m_em = getField @"email" ur <&> reference . SG.getLast
                            m_pw = getField @"password" ur <&> reference . SG.getLast
                         in UpdatedUser (reference t) m_uid m_em m_pw
                    FollowUser ref' ->
                      let follow :<|> _ = withUser $ pure $ concrete ref'
                       in run follow $ const $ UserResponse FollowedUser
                    UnfollowUser ref' ->
                      let _ :<|> unfollow = withUser $ pure $ concrete ref'
                       in run unfollow $ const $ UserResponse UnfollowedUser
                    CreateArticle ar -> run (createArticle $ In $ pure ar) $ UserResponse . CreatedArticle . reference . transform . getField @"article"
                    UpdateArticle ref' u ->
                      let (updateArticle :<|> _) :<|> _ = withArticle $ pure $ concrete ref'
                          m_new_aid = reference . ArticleId . titleToSlug . SG.getLast <$> getField @"title" u
                       in run (updateArticle $ In $ pure u) $ const $ UserResponse $ UpdatedArticle m_new_aid
                    DeleteArticle ref' ->
                      let (_ :<|> deleteArticle) :<|> _ = withArticle $ pure $ concrete ref'
                       in runNoContent deleteArticle $ UserResponse DeletedArticle
                    AddCommentToArticle ref' cr ->
                      let _ :<|> (_ :<|> addComment) :<|> _ = withArticle $ pure $ concrete ref'
                       in run (addComment $ In $ pure cr) $ UserResponse . AddedCommentToArticle . reference . getField @"id"
                    DeleteComment ref' ref'' ->
                      let _ :<|> (deleteComment :<|> _) :<|> _ = withArticle $ pure $ concrete ref'
                       in runNoContent (deleteComment $ pure $ concrete ref'') $ UserResponse DeletedComment
                    FavoriteArticle ref' ->
                      let _ :<|> _ :<|> (favoriteArticle :<|> _) = withArticle $ pure $ concrete ref'
                       in run favoriteArticle $ const $ UserResponse FavoritedArticle
                    UnfavoriteArticle ref' ->
                      let _ :<|> _ :<|> (_ :<|> unfavoriteArticle) = withArticle $ pure $ concrete ref'
                       in run unfavoriteArticle $ const $ UserResponse UnfavoritedArticle
                    FeedArticles streamMode ->
                      let feedArticlesNoStream :<|> feedArticlesStream = feedArticles Nothing Nothing
                       in runByCases feedArticlesNoStream feedArticlesStream streamMode $ const $ UserResponse FeededArticles

stateMachine :: StateMachine Model Command (ReaderT ClientEnv IO) Response
stateMachine = StateMachine initModel transition precondition postcondition Nothing generator shrinker semantics mock noCleanup

-- prop :: IO Application -> Manager -> (Int -> BaseUrl) -> Property
-- prop new mgr mkUrl =
--   forAllParallelCommands sm Nothing $ \cmds -> monadic
--     ( ioProperty
--         . \prop -> testWithApplication new $ \port -> usingReaderT (mkClientEnv mgr $ mkUrl port) prop
--     )
--     $ do
--       prettyParallelCommands cmds =<< runParallelCommandsNTimes 30 sm cmds
