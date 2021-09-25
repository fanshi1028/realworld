-- |
module StateMachine.Gen where

import Gen.Realistic (arbitraryRealistic, shrinkRealistic)
import Orphans ()
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
import Test.QuickCheck (Gen, elements, frequency)

generator :: Model r -> Maybe (Gen (Command r))
generator m =
  let genRef = elements . map fst
      genUsers = genRef $ users m
      genArticles = genRef $ articles m
      genComments = genRef $ comments m
      genLogins = elements . map snd $ logins m
      genTokens =
        if null $ tokens m
          then pure Nothing
          else
            frequency
              [ (5, Just <$> genRef (tokens m)),
                (1, pure Nothing)
              ]
      genVisitCommand =
        frequency
          [ (if null $ users m then 0 else 1, GetProfile <$> genUsers),
            (1, pure ListArticles),
            (1, pure GetTags),
            (if null $ articles m then 0 else 1, GetArticle <$> genArticles),
            (if null $ articles m then 0 else 1, GetComments <$> genArticles)
          ]
      genAuthCommand =
        frequency
          [ (if null $ users m then 3 else 1, Register <$> arbitraryRealistic),
            (if null $ logins m then 0 else 1, Login <$> genLogins)
            -- (1, pure Logout)
          ]
      genUserCommand =
        frequency
          [ (1, pure GetCurrentUser),
            -- FIXME
            -- (1, UpdateUser <$> arbitraryRealistic),
            (if null $ users m then 0 else 1, UnfollowUser <$> genUsers),
            (if null $ users m then 0 else 1, FollowUser <$> genUsers),
            (if null $ articles m then 8 else 2, CreateArticle <$> arbitraryRealistic),
            -- FIXME
            -- (1, UpdateArticle <$> genArticles <*> arbitraryRealistic)
            (if null $ articles m then 0 else 1, DeleteArticle <$> genArticles),
            ( if null $ articles m
                then 0
                else
                  if null $ comments m
                    then 8
                    else 3,
              AddCommentToArticle <$> genArticles <*> arbitraryRealistic
            ),
            ( if null (comments m) || null (articles m) then 0 else 1, DeleteComment <$> genArticles <*> genComments),
            (if null $ articles m then 0 else 1, FavoriteArticle <$> genArticles),
            (if null $ articles m then 0 else 1, UnfavoriteArticle <$> genArticles),
            (1, pure FeedArticles)
          ]
   in pure $
        frequency
          [ (if null (users m) || null (tokens m) then 4 else 1, AuthCommand <$> genTokens <*> genAuthCommand),
            (if null $ tokens m then 1 else 2, VisitorCommand <$> genTokens <*> genVisitCommand),
            (1, UserCommand <$> genTokens <*> genUserCommand)
          ]

shrinker :: Model r -> Command r -> [Command r]
shrinker _ =
  \case
    AuthCommand m_ref ac ->
      AuthCommand m_ref <$> case ac of
        Register ur -> Register <$> shrinkRealistic ur
        Login ur -> []
        Logout -> []
    VisitorCommand _ _ -> []
    UserCommand m_ref uc ->
      UserCommand m_ref <$> case uc of
        GetCurrentUser -> []
        -- FIXME
        -- UpdateUser ur -> UpdateUser <$> shrinkRealistic ur
        UnfollowUser _ -> []
        CreateArticle ar -> CreateArticle <$> shrinkRealistic ar
        -- FIXME
        -- UpdateArticle ref ar -> UpdateArticle ref  <$> shrinkRealistic ar
        DeleteArticle _ -> []
        AddCommentToArticle ref cr -> AddCommentToArticle ref <$> shrinkRealistic cr
        DeleteComment _ _ -> []
        FavoriteArticle _ -> []
        UnfavoriteArticle _ -> []
        FeedArticles -> []
