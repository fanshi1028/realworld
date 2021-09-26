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

-- |
module StateMachine where

import Control.Lens ((%~))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Functor.Classes (Eq1, Ord1)
import Data.Generics.Product (field, getField)
import Data.Set (delete, insert)
import qualified Data.Set as S
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
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
  )
import Test.StateMachine.Logic (member)
import Test.Tasty.QuickCheck ((===))
import Validation (Validation (Failure, Success))
import Domain.Util.Field (titleToSlug)

initModel :: Model r
initModel = Model mempty mempty mempty mempty S.empty S.empty S.empty S.empty S.empty S.empty mempty

transition :: (Eq1 r, Ord1 r) => Model r -> Command r -> Response r -> Model r
transition m cm res = case (cm, res) of
  (_, FailResponse _) -> m
  (AuthCommand mref cm', AuthResponse res') -> case (mref, cm', res') of
    -- FIXME: Register when login?
    -- (_, Register _, Registered ref u) -> m & field @"users" %~ ((ref, u) :)
    (_, Register cr, Registered ref login) ->
      m & field @"users" %~ ((ref, cr) :)
        & field @"logins" %~ ((ref, login) :)
    -- FIXME: How about double login with different identity?
    (_, Login ref, LoggedIn token) -> case findByRef2 ref $ logins m of
      Nothing -> error "impossible"
      Just ref' -> m & field @"tokens" %~ ((token, ref') :)
    (Just t, Logout, LoggedOut) -> m & field @"tokens" %~ deleteByRef t
    _failed -> m
  (VisitorCommand _ _, _) -> m
  (UserCommand mref cm', UserResponse res') -> case (cm', res', mref >>= \t -> findByRef t $ tokens m) of
    (GetCurrentUser, _, _) -> m
    -- FIXME: JWT suck as session token, update profile will invalidate the login session
    (UpdateUser _, UpdatedUser, Just ref) -> m
    (FollowUser ref, FollowedUser, Just ref') -> m & field @"userFollowUser" %~ insert (ref', ref)
    (UnfollowUser ref, UnfollowedUser, Just ref') -> m & field @"userFollowUser" %~ delete (ref', ref)
    (CreateArticle cr, CreatedArticle ref, Just ref') ->
      m
        & field @"articles" %~ ((ref, cr) :)
        & field @"userCreateArticle" %~ insert (ref', ref)
    (UpdateArticle ref _, UpdatedArticle, Just _) -> m
    (DeleteArticle ref, DeletedArticle, Just ref') ->
      m
        & field @"articles" %~ deleteByRef ref
        & field @"userCreateArticle" %~ delete (ref', ref)
    (AddCommentToArticle ref c, AddedCommentToArticle ref', Just _) ->
      m
        & field @"comments" %~ ((ref', c) :)
        & field @"articleHasComment" %~ insert (ref, ref')
    (DeleteComment ref ref', DeletedComment, Just _) ->
      m
        & field @"comments" %~ deleteByRef ref'
        & field @"articleHasComment" %~ delete (ref, ref')
    (FavoriteArticle ref, FavoritedArticle, Just ref') -> m & field @"userFavoriteArticle" %~ insert (ref', ref)
    (UnfavoriteArticle ref, UnfavoritedArticle, Just ref') -> m & field @"userFavoriteArticle" %~ delete (ref', ref)
    (FeedArticles, _, _) -> m
    _failed -> m
  _failed -> m

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition _ _ = Top

postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition m cmd res =
  let m' = transition m cmd res

      tokenToUser tokenRef mm = findByRef tokenRef (tokens mm) >>= flip findByRef (users mm)
      validToken = Boolean . isJust <<$>> tokenToUser
      findByRef' ref = Boolean . isJust . findByRef ref

      usersL = length . users
      loginsL = length . logins
      articlesL = length . articles
      commentsL = length . comments
      followL = length . userFollowUser
      favoriteL = length . userFavoriteArticle
      tagL = length . articleTaggedByTag
      hasCommentL = length . articleHasComment
      createArticleL = length . userCreateArticle
      tokensL = length . tokens

      allL = [usersL, loginsL, articlesL, commentsL, followL, favoriteL, tagL, hasCommentL, createArticleL, tokensL]
      authInv = [articlesL, commentsL, followL, favoriteL, tagL, hasCommentL, createArticleL]
      followInv = [loginsL, usersL, articlesL, commentsL, favoriteL, tagL, hasCommentL, createArticleL, tokensL]
      articleInv = [loginsL, usersL, commentsL, followL, favoriteL, tagL, hasCommentL, tokensL]
      commentInv = [loginsL, usersL, articlesL, followL, favoriteL, tagL, createArticleL, tokensL]
      favoriteInv = [loginsL, usersL, articlesL, commentsL, followL, tagL, hasCommentL, createArticleL, tokensL]
   in case (cmd, res) of
        (_, FailResponse _) -> m .== m'
        (AuthCommand m_ref ac, AuthResponse ar) ->
          Boolean (and $ on (==) <$> authInv <*> pure m <*> pure m') .// "authInv"
            .&& case (ac, ar) of
              (Register cr, Registered ref login) ->
                findByRef ref (users m) .== Nothing .// ""
                  .&& findByRef ref (users m') .== Just cr .// ""
                  .&& findByRef ref (logins m') .== Just login .// ""
                  .&& on (-) usersL m' m .== 1 .// ""
                  .&& on (-) loginsL m' m .== 1 .// ""
                  .&& on (.==) tokensL m' m .// ""
              (Login _, LoggedIn _) ->
                on (.==) usersL m' m .// "" .&& case m_ref of
                  Just ref ->
                    validToken ref m
                      .&& Boolean (isJust $ findByRef ref $ tokens m') .// ""
                      .&& on (-) tokensL m' m .== 1 .// ""
                      .&& on (.==) loginsL m' m .// ""
                  Nothing -> Top
              (Logout, LoggedOut) ->
                on (.==) usersL m' m .// "" .&& case m_ref of
                  Just ref ->
                    validToken ref m
                      .&& findByRef ref (tokens m') .== Nothing .// ""
                      .&& on (-) tokensL m m' .== 1 .// ""
                      .&& on (.==) loginsL m' m .// ""
                  Nothing -> Top
              _ -> error "auth postcondition error"
        (VisitorCommand _ vc, VisitorResponse vr) ->
          m .== m' .// "" .&& case (vc, vr) of
            (GetProfile ref', GotProfile) -> Forall [m, m'] (findByRef' ref' . users) .// ""
            (GetArticle ref', GotArticle) -> Forall [m, m'] (findByRef' ref' . articles) .// ""
            (ListArticles, ListedArticles) -> Top
            (GetTags, GotTags) -> Top
            (GetComments ref', GotComments) -> Forall [m, m'] (findByRef' ref' . articles) .// ""
            _ -> error "visitor postcondition error"
        (UserCommand (Just ref) uc, UserResponse ur) ->
          Forall [m, m'] (validToken ref) .// "" .&& case (uc, ur) of
            (GetCurrentUser, GotCurrentUser) -> m .== m' .// ""
            (UpdateUser _, UpdatedUser) -> Boolean (and $ on (==) <$> allL <*> pure m <*> pure m') .// ""
            (FollowUser ref', FollowedUser) ->
              Forall [m, m'] (findByRef' ref' . users) .// ""
                .&& case findByRef ref $ tokens m of
                  Nothing -> Bot .// ""
                  Just ref'' ->
                    member (ref'', ref') (userFollowUser m') .// ""
                      .&& Boolean (and $ on (==) <$> followInv <*> pure m <*> pure m') .// ""
            (UnfollowUser ref', UnfollowedUser) ->
              Forall [m, m'] (findByRef' ref' . users) .// ""
                .&& case findByRef ref $ tokens m of
                  Nothing -> Bot .// ""
                  Just ref'' ->
                    notMember (ref'', ref') (userFollowUser m') .// ""
                      .&& Boolean (and $ on (==) <$> followInv <*> pure m <*> pure m') .// ""
            (CreateArticle _, CreatedArticle ref') ->
              findByRef' ref' (articles m') .// ""
                .&& Not (findByRef' ref' $ articles m) .// ""
                .&& (findByRef ref (tokens m') >>= flip findByRef (userCreateArticle m')) .== Just ref' .// ""
                .&& ( case findByRef ref $ tokens m' of
                        Nothing -> Bot .// ""
                        Just ref'' ->
                          member (ref'', ref') (userCreateArticle m') .// ""
                            .&& notMember (ref'', ref') (userCreateArticle m) .// ""
                    )
                .&& on (-) articlesL m' m .== 1 .// ""
                .&& on (-) createArticleL m' m .== 1 .// ""
                .&& Boolean (and $ on (==) <$> articleInv <*> pure m <*> pure m') .// ""
            (UpdateArticle ref' _, UpdatedArticle) ->
              Forall [m, m'] (findByRef' ref' . articles) .// ""
                -- FIXME: factor out this
                .&& (findByRef ref (tokens m) >>= flip findByRef (userCreateArticle m)) .== Just ref' .// ""
                .&& (findByRef ref (tokens m') >>= flip findByRef (userCreateArticle m')) .== Just ref' .// ""
                .&& Boolean (and $ on (==) <$> allL <*> pure m <*> pure m') .// ""
            (DeleteArticle ref', DeletedArticle) ->
              findByRef' ref' (articles m) .// ""
                .&& Not (findByRef' ref' $ articles m') .// ""
                .&& (findByRef ref (tokens m) >>= flip findByRef (userCreateArticle m)) .== Just ref' .// ""
                -- FIXME: factor out this????
                .&& ( case findByRef ref $ tokens m of
                        Nothing -> Bot .// ""
                        Just ref'' ->
                          member (ref'', ref') (userCreateArticle m) .// ""
                            .&& notMember (ref'', ref') (userCreateArticle m') .// ""
                    )
                .&& on (-) articlesL m m' .== 1 .// ""
                .&& on (-) createArticleL m m' .== 1 .// ""
                .&& Boolean (and $ on (==) <$> articleInv <*> pure m <*> pure m') .// ""
            (AddCommentToArticle ref' _, AddedCommentToArticle ref'') ->
              Forall [m, m'] (findByRef' ref' . articles) .// ""
                .&& findByRef' ref'' (comments m') .// ""
                .&& Not (findByRef' ref'' $ comments m) .// ""
                .&& notMember (ref', ref'') (articleHasComment m) .// ""
                .&& member (ref', ref'') (articleHasComment m') .// ""
                .&& on (-) commentsL m' m .== 1 .// ""
                .&& on (-) hasCommentL m' m .== 1 .// ""
                .&& Boolean (and $ on (==) <$> commentInv <*> pure m <*> pure m') .// ""
            (DeleteComment ref' ref'', DeletedComment) ->
              Forall [m, m'] (findByRef' ref' . articles) .// ""
                .&& findByRef' ref'' (comments m) .// ""
                .&& Not (findByRef' ref'' $ comments m') .// ""
                .&& member (ref', ref'') (articleHasComment m) .// ""
                .&& notMember (ref', ref'') (articleHasComment m') .// ""
                .&& on (-) commentsL m m' .== 1 .// ""
                .&& on (-) hasCommentL m m' .== 1 .// ""
                .&& Boolean (and $ on (==) <$> commentInv <*> pure m <*> pure m') .// ""
            (FavoriteArticle ref', FavoritedArticle) ->
              Forall [m, m'] (findByRef' ref' . articles) .// ""
                .&& case findByRef ref $ tokens m of
                  Nothing -> Bot .// ""
                  Just ref'' ->
                    member (ref'', ref') (userFavoriteArticle m') .// ""
                      .&& Boolean (and $ on (==) <$> favoriteInv <*> pure m <*> pure m') .// ""
            (UnfavoriteArticle ref', UnfavoritedArticle) ->
              Forall [m, m'] (findByRef' ref' . articles) .// ""
                .&& case findByRef ref $ tokens m of
                  Nothing -> Bot .// ""
                  Just ref'' ->
                    notMember (ref'', ref') (userFavoriteArticle m') .// ""
                      .&& Boolean (and $ on (==) <$> favoriteInv <*> pure m <*> pure m') .// ""
            (FeedArticles, FeededArticles) -> m .== m' .// ""
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
              pure $ Registered <$> genSym <*> genSym
            Login ur -> case findByRef2 ur $ logins m of
              Just _ -> AuthResponse . LoggedIn <$> genSym
              Nothing -> pure $ FailResponse ""
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
          uRef <- m_ref >>= (`findByRef` tokens m)
          _ <- uRef `findByRef` users m
          case uc of
            GetCurrentUser -> pure $ pure GotCurrentUser
            -- FIXME
            UpdateUser ur -> pure $ pure UpdatedUser
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
            -- FIXME: fail case
            UpdateArticle ref _ -> do
              _ <- findByRef ref $ articles m
              guard $ (uRef, ref) `S.member` userCreateArticle m
              pure $ pure UpdatedArticle
            DeleteArticle ref -> do
              _ <- findByRef ref $ articles m
              guard $ (uRef, ref) `S.member` userCreateArticle m
              pure $ pure DeletedArticle
            AddCommentToArticle _ _ -> pure $ AddedCommentToArticle <$> genSym
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
              run (register $ In $ pure cr) $ \(UserAuthWithToken u _) ->
                AuthResponse $
                  Registered
                    (reference $ transform u)
                    (reference $ UserLogin (getField @"email" cr) $ getField @"password" cr)
            Login ur ->
              run (login $ In $ pure $ concrete ur) $ \(UserAuthWithToken _ t) ->
                AuthResponse $ LoggedIn $ reference t
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
                    UpdateUser ur -> run (updateUser $ In $ pure ur) $ const $ UserResponse UpdatedUser
                    FollowUser ref' ->
                      let follow :<|> _ = withUser $ pure $ concrete ref'
                       in run follow $ const $ UserResponse FollowedUser
                    UnfollowUser ref' ->
                      let _ :<|> unfollow = withUser $ pure $ concrete ref'
                       in run unfollow $ const $ UserResponse UnfollowedUser
                    CreateArticle ar -> run (createArticle $ In $ pure ar) $ UserResponse . CreatedArticle . reference . transform . getField @"article"
                    UpdateArticle ref' ar ->
                      let (updateArticle :<|> _) :<|> _ = withArticle $ pure $ concrete ref'
                       in run (updateArticle $ In $ pure ar) $ const $ UserResponse UpdatedArticle
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

-- let
--     run' :: forall a. ClientM a -> IO (Either ClientError a)
--     run' = flip runClientM (mkClientEnv m url)

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
