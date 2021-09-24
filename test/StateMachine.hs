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
import Domain.User (UserR (..))
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import Domain.Util.Representation (transform)
import Domain.Util.Validation (WithValidation)
import Gen.Naive ()
import HTTP.Auth.User (AuthUserApi)
import HTTP.Public (PublicApi)
import Network.HTTP.Client (Manager)
import Orphans ()
import Servant (type (:<|>) ((:<|>)))
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
import StateMachine.Util (deleteByRef, findByRef, findByRef2, updateByRef)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.Monadic (monadic)
import Test.StateMachine
  ( Concrete,
    GenSym,
    Logic (Boolean, Bot, Forall, Not, Top),
    Reason (Ok),
    StateMachine (StateMachine),
    Symbolic,
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
import Validation (Validation (Failure, Success))

initModel :: Model r
initModel = Model mempty mempty mempty mempty S.empty S.empty S.empty S.empty S.empty S.empty mempty

transition :: (Eq1 r, Ord1 r) => Model r -> Command r -> Response r -> Model r
transition m cm res = case (cm, res) of
  (_, FailResponse _) -> m
  (AuthCommand mref cm', AuthResponse res') -> case (mref, cm', res') of
    -- FIXME: Register when login?
    -- (_, Register _, Registered ref u) -> m & field @"users" %~ ((ref, u) :)
    (_, Register _, Registered ref u login) ->
      m & field @"users" %~ ((ref, u) :)
        & field @"logins" %~ ((ref, login) :)
    -- FIXME: How about double login with different identity?
    (_, Login _, LoggedIn token ref) -> m & field @"tokens" %~ ((token, ref) :)
    (Just t, Logout, LoggedOut) -> m & field @"tokens" %~ deleteByRef t
    _failed -> m
  (VisitorCommand _ _, _) -> m
  (UserCommand mref cm', UserResponse res') -> case (cm', res', mref >>= \t -> findByRef t $ tokens m) of
    (GetCurrentUser, _, _) -> m
    -- FIXME: JWT suck as session token, update profile will invalidate the login session
    (UpdateUser _, UpdatedUser u, Just ref) -> m & field @"users" %~ updateByRef ref u
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
              (Register _, Registered ref auth login) ->
                findByRef ref (users m) .== Nothing .// ""
                  .&& findByRef ref (users m') .== Just auth .// ""
                  .&& findByRef ref (logins m') .== Just login .// ""
                  .&& on (-) usersL m' m .== 1 .// ""
                  .&& on (-) loginsL m' m .== 1 .// ""
                  .&& on (.==) tokensL m' m .// ""
              (Login _, LoggedIn _ _) ->
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
            (GetCurrentUser, GotCurrentUser _) -> m .== m' .// ""
            (UpdateUser _, UpdatedUser _) -> Boolean (and $ on (==) <$> allL <*> pure m <*> pure m') .// ""
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
              pure $ Registered <$> genSym <*> genSym <*> genSym
            Login ur -> case findByRef2 ur $ logins m of
              Just _ -> AuthResponse <$> (LoggedIn <$> genSym <*> genSym)
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
          auth <- uRef `findByRef` users m
          case uc of
            GetCurrentUser -> pure $ pure $ GotCurrentUser auth
            -- FIXME
            UpdateUser ur -> pure $ pure $ UpdatedUser undefined
            FollowUser ref -> do
              _ <- findByRef ref $ users m
              pure $ pure FollowedUser
            UnfollowUser ref -> do
              _ <- findByRef ref $ users m
              pure $ pure UnfollowedUser
            -- FIXME: fail case
            CreateArticle _ -> pure $ CreatedArticle <$> genSym
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
      run' a = ask >>= liftIO . runClientM a
      handleRes f = \case
        Left ce -> pure $ FailResponse $ show ce
        Right (Out a) -> pure $ f a
      run req f = run' req >>= handleRes f
   in \case
        AuthCommand m_ref ac ->
          let login :<|> register = client $ Proxy @AuthUserApi
           in case ac of
                Register cr ->
                  run (register $ In $ pure cr) $ \(UserAuthWithToken u _) ->
                    AuthResponse $
                      Registered
                        (reference $ transform u)
                        (reference u)
                        (reference $ UserLogin (getField @"email" cr) $ getField @"password" cr)
                Login ur ->
                  run (login $ In $ pure $ concrete ur) $ \(UserAuthWithToken u t) ->
                    AuthResponse $ LoggedIn (reference t) $ reference $ transform u
                -- FIXME: No logout api
                Logout -> undefined
        VisitorCommand m_ref vc ->
          let getProfile :<|> (listArticles :<|> withArticle) :<|> getTags = client $ Proxy @PublicApi
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

-- UserCommand m_ref uc ->
--   let a = client $ Proxy @AuthedApi
--    in case uc of
--         GetCurrentUser -> _
--         (UpdateUser ur) -> _
--         (FollowUser ref) -> _
--         (UnfollowUser ref) -> _
--         (CreateArticle ar) -> _
--         (UpdateArticle ref ar) -> _
--         (DeleteArticle ref) -> _
--         (AddCommentToArticle ref cr) -> _
--         (DeleteComment ref ref') -> _
--         (FavoriteArticle ref) -> _
--         (UnfavoriteArticle ref) -> _
--         FeedArticles -> _

sm :: StateMachine Model Command (ReaderT ClientEnv IO) Response
sm = StateMachine initModel transition precondition postcondition Nothing generator shrinker semantics mock noCleanup

-- let
--     run' :: forall a. ClientM a -> IO (Either ClientError a)
--     run' = flip runClientM (mkClientEnv m url)

prop1 :: Manager -> BaseUrl -> Property
prop1 mgr url =
  forAllCommands sm Nothing $ \cmds -> monadic (ioProperty . usingReaderT (mkClientEnv mgr url)) $ do
    (hist, _, res) <- runCommands sm cmds
    -- prettyCommands sm hist (res === Ok)
    pure ()

prop2 :: Manager -> BaseUrl -> Property
prop2 mgr url =
  forAllParallelCommands sm Nothing $ \cmds -> monadic (ioProperty . usingReaderT (mkClientEnv mgr url)) $ do
    prettyParallelCommands cmds =<< runParallelCommandsNTimes 30 sm cmds
