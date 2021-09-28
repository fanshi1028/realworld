{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module StateMachine where

import Control.Lens ((%~))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Functor.Classes (Eq1, Ord1)
import Data.Generics.Product (field, getField)
import qualified Data.Semigroup as SG (Last (Last, getLast))
import Data.Set (delete, insert)
import qualified Data.Set as S (empty, map, member)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Field (titleToSlug)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import Domain.Util.Representation (transform)
import Domain.Util.Validation (WithValidation)
import Gen.Naive ()
import HTTP (Api)
import Network.HTTP.Client (Manager)
import Network.Wai.Handler.Warp (withApplication)
import Orphans ()
import Servant (Application, type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl, ClientEnv, ClientError, ClientM, client, mkClientEnv, runClientM)
import StateMachine.Gen (generator, shrinker)
import StateMachine.Types
  ( AuthCommand (..),
    AuthResponse (..),
    Command (..),
    Model (..),
    Response (..),
    UserCommand (..),
    UserResponse (..),
    VisitorCommand (..),
    VisitorResponse (..),
  )
import StateMachine.Util (deleteByRef, findByRef, findByRef2)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.Monadic (monadic)
import Test.StateMachine
  ( Concrete,
    GenSym,
    Logic (Boolean, Bot, Forall, Not, Top),
    Reason (Ok),
    StateMachine (StateMachine),
    Symbolic,
    checkCommandNames,
    concrete,
    forAllCommands,
    forAllParallelCommands,
    genSym,
    noCleanup,
    notMember,
    prettyCommands,
    prettyParallelCommands,
    reference,
    runCommands,
    runParallelCommandsNTimes,
    (.&&),
    (.//),
    (.==),
    (.=>),
  )
import Test.StateMachine.Logic (member)
import Test.Tasty.QuickCheck ((===))
import Validation (Validation (Failure, Success))

initModel :: Model r
initModel = Model mempty mempty mempty mempty mempty S.empty S.empty S.empty S.empty S.empty mempty

transition :: (Eq1 r, Ord1 r) => Model r -> Command r -> Response r -> Model r
transition m cm res = case (cm, res) of
  (_, FailResponse _) -> m
  (AuthCommand mref cm', AuthResponse res') -> case (mref, cm', res') of
    -- FIXME: Register when login?
    (_, Register cr, Registered ref em pw t) ->
      m & field @"users" %~ ((ref, cr) :)
        & field @"emails" %~ ((ref, em) :)
        & field @"credentials" %~ ((em, pw) :)
        & field @"tokens" %~ ((t, em) :)
    -- FIXME: How about double login with different identity?
    (_, Login _ _, LoggedIn) -> m
    (Just t, Logout, LoggedOut) -> m & field @"tokens" %~ deleteByRef t
    _failed -> m
  (VisitorCommand _ _, _) -> m
  (UserCommand (Just t) cm', UserResponse res') -> fromMaybe m $ do
    em <- findByRef t $ tokens m
    ref <- findByRef2 em $ emails m
    pw <- findByRef em $ credentials m
    pure $ case (cm', res') of
      (GetCurrentUser, _) -> m
      -- FIXME: JWT suck as session token, update profile will invalidate the login session
      (UpdateUser _, UpdatedUser t' m_uid m_em m_pw) ->
        let em' = fromMaybe em m_em
            ref' = fromMaybe ref m_uid
            pw' = fromMaybe pw m_pw
         in m
              & field @"tokens" %~ ((t', em') :) . deleteByRef t
              & field @"emails" %~ ((ref', em') :) . deleteByRef ref
              & field @"credentials" %~ ((em', pw') :) . deleteByRef em
              & field @"userFollowUser" %~ S.map (\us@(u1, u2) -> (if u1 == ref then (ref', u2) else if u2 == ref then (u1, ref') else us))
              & field @"userFavoriteArticle" %~ S.map (\ua@(u, a) -> (if u == ref then (ref', a) else ua))
              & field @"userFavoriteArticle" %~ S.map (\ua@(u, a) -> (if u == ref then (ref', a) else ua))
              & field @"userCreateComment" %~ S.map (\uc@(u, c) -> (if u == ref then (ref', c) else uc))
              & field @"userCreateArticle" %~ S.map (\ua@(u, a) -> (if u == ref then (ref', a) else ua))
      (FollowUser ref', FollowedUser) -> m & field @"userFollowUser" %~ insert (ref, ref')
      (UnfollowUser ref', UnfollowedUser) -> m & field @"userFollowUser" %~ delete (ref, ref')
      (CreateArticle cr, CreatedArticle ref') ->
        m
          & field @"articles" %~ ((ref', cr) :)
          & field @"userCreateArticle" %~ insert (ref, ref')
      (UpdateArticle ref' _, UpdatedArticle m_aid) ->
        case m_aid of
          Nothing -> m
          Just aid ->
            m
              & field @"userCreateArticle" %~ insert (ref, aid) . delete (ref, ref')
              & field @"userFavoriteArticle" %~ insert (ref, aid) . delete (ref, ref')
      (DeleteArticle ref', DeletedArticle) ->
        m
          & field @"articles" %~ deleteByRef ref'
          & field @"userCreateArticle" %~ delete (ref, ref')
      (AddCommentToArticle ref' c, AddedCommentToArticle ref'') ->
        m
          & field @"comments" %~ ((ref'', c) :)
          & field @"articleHasComment" %~ insert (ref', ref'')
      (DeleteComment ref' ref'', DeletedComment) ->
        m
          & field @"comments" %~ deleteByRef ref''
          & field @"articleHasComment" %~ delete (ref', ref'')
      (FavoriteArticle ref', FavoritedArticle) -> m & field @"userFavoriteArticle" %~ insert (ref, ref')
      (UnfavoriteArticle ref', UnfavoritedArticle) -> m & field @"userFavoriteArticle" %~ delete (ref, ref')
      (FeedArticles, _) -> m
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

      allL = [usersL, emailsL, credentialsL, articlesL, commentsL, followL, favoriteL, hasCommentL, createArticleL, tokensL]
      authInv = [articlesL, commentsL, followL, favoriteL, hasCommentL, createArticleL]
      followInv = [emailsL, credentialsL, usersL, articlesL, commentsL, favoriteL, hasCommentL, createArticleL, tokensL]
      articleInv = [emailsL, credentialsL, usersL, commentsL, followL, favoriteL, hasCommentL, tokensL]
      commentInv = [emailsL, credentialsL, usersL, articlesL, followL, favoriteL, createArticleL, tokensL]
      favoriteInv = [emailsL, credentialsL, usersL, articlesL, commentsL, followL, hasCommentL, createArticleL, tokensL]
   in case (cmd, res) of
        (_, FailResponse _) -> m .== m' .// "same model"
        (AuthCommand m_ref ac, AuthResponse ar) ->
          Boolean (and $ on (==) <$> authInv <*> pure m <*> pure m') .// "auth command invariant"
            .&& case (ac, ar) of
              (Register cr, Registered ref em pw t) ->
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
              -- NOTE: no one care, not log out in spec
              (Logout, LoggedOut) ->
                on (.==) usersL m' m .// "same users" .&& case m_ref of
                  Just ref ->
                    validToken ref m .// "before: the token is valid"
                      .&& findByRef ref (tokens m') .== Nothing .// "after: the token is invalidated"
                      .&& on (-) tokensL m m' .== 1 .// "after: removed 1 from tokens"
                      .&& on (.==) emailsL m' m .// "same emails"
                      .&& on (.==) credentialsL m' m .// "same credentials"
                  Nothing -> Bot .// "logged in"
              _ -> error "auth postcondition error"
        (VisitorCommand _ vc, VisitorResponse vr) ->
          m .== m' .// "same model" .&& case (vc, vr) of
            (GetProfile ref', GotProfile) -> Forall [m, m'] (findByRef' ref' . users) .// "the user exist"
            (GetArticle ref', GotArticle) -> Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
            (ListArticles, ListedArticles) -> Top
            (GetTags, GotTags) -> Top
            (GetComments ref', GotComments) -> Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
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
                      .&& Boolean (and $ on (==) <$> allL <*> pure m <*> pure m') .// "update user invariant"
                      .&& maybe Top (\pw -> member (em', pw) (credentials m')) m_pw .// "after: maybe updated credential in credentials"
              (FollowUser ref', FollowedUser) ->
                Forall [m, m'] (findByRef' ref' . users) .// "the user exists"
                  .&& member (ref'', ref') (userFollowUser m') .// "after: followed the user"
                  .&& Boolean (and $ on (==) <$> followInv <*> pure m <*> pure m') .// "follow invariant"
              (UnfollowUser ref', UnfollowedUser) ->
                Forall [m, m'] (findByRef' ref' . users) .// "the user exists"
                  .&& notMember (ref'', ref') (userFollowUser m') .// "after: unfollowed the user"
                  .&& Boolean (and $ on (==) <$> followInv <*> pure m <*> pure m') .// "follow invariant"
              (CreateArticle _, CreatedArticle ref') ->
                findByRef' ref' (articles m') .// "after: the article exists"
                  .&& Not (findByRef' ref' $ articles m) .// "before: the article not existed"
                  .&& member (ref'', ref') (userCreateArticle m') .// "after: user created the article"
                  .&& notMember (ref'', ref') (userCreateArticle m) .// "before: user didn't created the article"
                  .&& on (-) articlesL m' m .== 1 .// "after: added 1 to articles"
                  .&& on (-) createArticleL m' m .== 1 .// "after: added 1 to userCreateArticle"
                  .&& Boolean (and $ on (==) <$> articleInv <*> pure m <*> pure m') .// "article invariant"
              (UpdateArticle ref' _, UpdatedArticle m_ref) ->
                let new_aid = fromMaybe ref' m_ref
                 in findByRef' ref' (articles m) .// "before: the article exists"
                      .&& findByRef' new_aid (articles m') .// "after: the article(maybe with new id) exists"
                      .&& member (ref'', ref') (userCreateArticle m) .// "before: user creates the article"
                      .&& member (ref'', new_aid) (userCreateArticle m') .// "after: user creates the article(maybe with new id)"
                      .&& member (ref'', ref') (userFavoriteArticle m) .// "before: user favorite the article"
                      .&& member (ref'', new_aid) (userFavoriteArticle m') .// "after: user favorite the article(maybe with new id)"
                      .&& Boolean (and $ on (==) <$> allL <*> pure m <*> pure m') .// "article invariant"
              (DeleteArticle ref', DeletedArticle) ->
                findByRef' ref' (articles m) .// "before: the article existed"
                  .&& Not (findByRef' ref' $ articles m') .// "after: the article not exists"
                  .&& member (ref'', ref') (userCreateArticle m) .// "before: user created the article"
                  .&& notMember (ref'', ref') (userCreateArticle m') .// "after: user not created the article"
                  .&& on (-) articlesL m m' .== 1 .// "after: removed 1 from article"
                  .&& on (-) createArticleL m m' .== 1 .// "after: removed 1 from userCreateArticle"
                  .&& Boolean (and $ on (==) <$> articleInv <*> pure m <*> pure m') .// "article invariant"
              (AddCommentToArticle ref' _, AddedCommentToArticle ref''') ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& findByRef' ref''' (comments m') .// "after: the comment exists"
                  .&& Not (findByRef' ref''' $ comments m) .// "before: the comment not existed"
                  .&& notMember (ref', ref''') (articleHasComment m) .// "before: article didn't has the comment"
                  .&& member (ref', ref''') (articleHasComment m') .// "after: aritlce has the comment"
                  .&& on (-) commentsL m' m .== 1 .// "after: added 1 to comments"
                  .&& on (-) hasCommentL m' m .== 1 .// "after: added 1 to aritlceHasComment"
                  .&& Boolean (and $ on (==) <$> commentInv <*> pure m <*> pure m') .// "comment invariant"
              (DeleteComment ref' ref''', DeletedComment) ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& findByRef' ref''' (comments m) .// "before: the comment exists"
                  .&& Not (findByRef' ref''' $ comments m') .// "after: the comment not existed"
                  .&& member (ref', ref''') (articleHasComment m) .// "before: aritlce has the comment"
                  .&& notMember (ref', ref''') (articleHasComment m') .// "after: article didn't has the comment"
                  .&& on (-) commentsL m m' .== 1 .// "after: remove 1 from comments"
                  .&& on (-) hasCommentL m m' .== 1 .// "after: remove 1 from aritlceHasComment"
                  .&& Boolean (and $ on (==) <$> commentInv <*> pure m <*> pure m') .// "comment invariant"
              (FavoriteArticle ref', FavoritedArticle) ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& member (ref'', ref') (userFavoriteArticle m') .// "after: user favorites the article"
                  .&& Boolean (and $ on (==) <$> favoriteInv <*> pure m <*> pure m') .// "favorite invariant"
              (UnfavoriteArticle ref', UnfavoritedArticle) ->
                Forall [m, m'] (findByRef' ref' . articles) .// "the article exists"
                  .&& notMember (ref'', ref') (userFavoriteArticle m') .// "after: user unfavorites the article"
                  .&& Boolean (and $ on (==) <$> favoriteInv <*> pure m <*> pure m') .// "favorite invariant"
              (FeedArticles, FeededArticles) -> m .== m' .// "same model"
              _ -> error "user postcondition error"
        _ -> error "postcondition error"

-- invariant = Nothing

-- mock m c = traverse (const genSym) transition m c
-- FIXME: handle fail case
mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock m =
  let -- HACK
      validate :: forall a. (FromJSON (WithValidation a), ToJSON a) => a -> Either String a
      validate a =
        eitherDecode @(WithValidation a) (encode a) >>= \case
          Failure err -> Left $ show err
          Success a' -> pure a'
   in \case
        AuthCommand m_ref ac ->
          case ac of
            Register cr -> either (pure . FailResponse . fromString) (AuthResponse <$>) $ do
              _ <- validate cr
              guard $ all ((/= transform cr) . UserId . getField @"username" . snd) $ users m
              guard $ all ((/= getField @"email" cr) . getField @"email" . snd) $ users m
              pure $ Registered <$> genSym <*> genSym <*> genSym <*> genSym
            Login em pw ->
              pure $
                if elem (em, pw) $ credentials m
                  then AuthResponse LoggedIn
                  else FailResponse ""
            Logout -> pure $ AuthResponse LoggedOut
        VisitorCommand _ vc -> maybe (pure $ FailResponse "") (VisitorResponse <$>) $ case vc of
          GetProfile _ -> pure $ pure GotProfile
          GetArticle ref -> do
            _ <- findByRef ref $ articles m
            pure $ pure GotArticle
          ListArticles -> pure $ pure ListedArticles
          GetTags -> pure $ pure GotTags
          GetComments ref -> do
            _ <- findByRef ref $ articles m
            pure $ pure GotComments
        UserCommand m_ref uc -> maybe (pure $ FailResponse "") (UserResponse <$>) $ do
          uRef <- m_ref >>= (`findByRef` tokens m) >>= (`findByRef2` emails m)
          _ <- uRef `findByRef` users m
          case uc of
            GetCurrentUser -> pure $ pure GotCurrentUser
            UpdateUser (UserUpdate u) -> do
              let validate' v = case v of
                    Nothing -> pure Nothing
                    Just (SG.Last a) -> case validate a of
                      Left _ -> Nothing
                      Right _ -> pure $ pure a
                  maybeGenSym a = maybe (pure Nothing) (const $ Just <$> genSym) a
              _ <- validate' $ getField @"bio" u
              _ <- validate' $ getField @"image" u
              m_uid <- validate' $ getField @"username" u
              m_em <- validate' $ getField @"email" u
              m_pw <- validate' $ getField @"password" u
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
              _ <- either (const Nothing) Just $ validate ca
              guard $ all ((/= transform ca) . ArticleId . titleToSlug . getField @"title" . snd) $ articles m
              pure $ CreatedArticle <$> genSym
            UpdateArticle ref (ArticleUpdate ar) -> do
              let validate' v = case v of
                    Nothing -> pure Nothing
                    Just (SG.Last a) -> case validate a of
                      Left _ -> Nothing
                      Right _ -> pure $ pure a
                  maybeGenSym a = maybe (pure Nothing) (const $ Just <$> genSym) a
              m_tt <- validate' $ getField @"title" ar
              _ <- validate' $ getField @"description" ar
              _ <- validate' $ getField @"body" ar
              _ <- findByRef ref $ articles m
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
            FeedArticles -> pure $ pure FeededArticles

semantics :: Command Concrete -> ReaderT ClientEnv IO (Response Concrete)
semantics =
  let run' :: forall a. ClientM a -> ReaderT ClientEnv IO (Either ClientError a)
      run' req = ask >>= liftIO . runClientM req
      run req f =
        run' req >>= \case
          Left ce -> pure $ FailResponse $ show ce
          Right (Out r) -> pure $ f r
      runNoContent req res = run' req >>= either (pure . FailResponse . show) (const $ pure res)
      (apis :<|> login :<|> register) :<|> _healthcheck = client $ Proxy @Api
   in \case
        AuthCommand m_ref ac ->
          case ac of
            Register cr ->
              run (register $ In $ pure cr) $ \(UserAuthWithToken u t) ->
                AuthResponse $
                  Registered
                    (reference $ transform u)
                    (reference $ getField @"email" cr)
                    (reference $ getField @"password" cr)
                    $ reference t
            Login em pw -> run (login $ In $ pure $ UserLogin (concrete em) $ concrete pw) $ const $ AuthResponse LoggedIn
            -- FIXME: No logout api
            Logout -> undefined
        VisitorCommand m_ref vc ->
          let (getProfile :<|> (listArticles :<|> withArticle) :<|> getTags) :<|> _ = apis $ maybe (UserToken "") concrete m_ref
           in case vc of
                GetProfile ref -> run (getProfile $ pure $ concrete ref) $ const $ VisitorResponse GotProfile
                GetArticle ref ->
                  let getArticle :<|> _ = withArticle $ pure $ concrete ref
                   in run getArticle $ const $ VisitorResponse GotArticle
                -- FIXME: gen query param ?
                ListArticles -> run (listArticles Nothing Nothing Nothing Nothing Nothing) $ const $ VisitorResponse ListedArticles
                GetTags -> run getTags $ const $ VisitorResponse GotTags
                GetComments ref ->
                  let _ :<|> getComments = withArticle $ pure $ concrete ref
                   in run getComments $ const $ VisitorResponse GotComments
        UserCommand m_ref uc ->
          case m_ref of
            Nothing -> pure $ FailResponse ""
            Just ref ->
              let _ :<|> (getCurrentUser :<|> updateUser) :<|> withUser :<|> createArticle :<|> feedArticles :<|> withArticle = apis $ concrete ref
               in case uc of
                    GetCurrentUser -> run getCurrentUser $ const $ UserResponse GotCurrentUser
                    UpdateUser u@(UserUpdate ur) -> run (updateUser $ In $ pure u) $ \(UserAuthWithToken _ t) ->
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
                    UpdateArticle ref' au@(ArticleUpdate u) ->
                      let (updateArticle :<|> _) :<|> _ = withArticle $ pure $ concrete ref'
                          m_new_aid = ArticleId . titleToSlug . SG.getLast <$> getField @"title" u
                          m_new_aid' = m_new_aid >>= bool Nothing (reference <$> m_new_aid) . (== concrete ref')
                       in run (updateArticle $ In $ pure au) $ const $ UserResponse $ UpdatedArticle m_new_aid'
                    DeleteArticle ref' ->
                      let (_ :<|> deleteArticle) :<|> _ = withArticle $ pure $ concrete ref'
                       in runNoContent deleteArticle $ UserResponse DeletedArticle
                    AddCommentToArticle ref' cr ->
                      let _ :<|> (_ :<|> addComment) :<|> _ = withArticle $ pure $ concrete ref'
                       in run (addComment $ In $ pure cr) $ UserResponse . AddedCommentToArticle . reference . transform
                    DeleteComment ref' ref'' ->
                      let _ :<|> (deleteComment :<|> _) :<|> _ = withArticle $ pure $ concrete ref'
                       in runNoContent (deleteComment $ pure $ concrete ref'') $ UserResponse DeletedComment
                    FavoriteArticle ref' ->
                      let _ :<|> _ :<|> (favoriteArticle :<|> _) = withArticle $ pure $ concrete ref'
                       in run favoriteArticle $ const $ UserResponse FavoritedArticle
                    UnfavoriteArticle ref' ->
                      let _ :<|> _ :<|> (_ :<|> unfavoriteArticle) = withArticle $ pure $ concrete ref'
                       in run unfavoriteArticle $ const $ UserResponse UnfavoritedArticle
                    FeedArticles -> run (feedArticles Nothing Nothing) $ const $ UserResponse FeededArticles

sm :: StateMachine Model Command (ReaderT ClientEnv IO) Response
sm = StateMachine initModel transition precondition postcondition Nothing generator shrinker semantics mock noCleanup

prop1 :: IO Application -> Manager -> (Int -> BaseUrl) -> Property
prop1 new mgr mkUrl =
  forAllCommands sm Nothing $ \cmds -> monadic
    ( ioProperty
        . \prop -> liftIO $ withApplication new $ \port -> usingReaderT (mkClientEnv mgr $ mkUrl port) prop
    )
    $ do
      (hist, _, res) <- runCommands sm cmds
      prettyCommands sm hist $ checkCommandNames cmds $ res === Ok
      pure ()

prop2 :: IO Application -> Manager -> (Int -> BaseUrl) -> Property
prop2 new mgr mkUrl =
  forAllParallelCommands sm Nothing $ \cmds -> monadic
    ( ioProperty
        . \prop -> liftIO $ withApplication new $ \port -> usingReaderT (mkClientEnv mgr $ mkUrl port) prop
    )
    $ do
      prettyParallelCommands cmds =<< runParallelCommandsNTimes 30 sm cmds
