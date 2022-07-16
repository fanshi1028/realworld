module StateMachine.Gen where

import Gen.Realistic (arbitraryRealistic, shrinkRealistic)
import StateMachine.Types
  ( AuthCommand (..),
    Command (..),
    Model (..),
    UserCommand (..),
    VisitorCommand (..),
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements, frequency, oneof)

generator :: Model r -> Maybe (Gen (Command r))
generator m =
  let genMaybe g = oneof [pure Nothing, Just <$> g]
      genRef = elements . map fst
      genUsers = genRef $ users m
      genArticles = genRef $ articles m
      genComments = genRef $ comments m
      genCredentials = elements $ credentials m
      genTag = elements $ toList $ tags m
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
            ( 1,
              ListArticles
                <$> (if null $ toList $ tags m then pure Nothing else genMaybe genTag)
                  <*> (if null $ toList $ users m then pure Nothing else genMaybe genUsers)
                  <*> (if null $ toList $ users m then pure Nothing else genMaybe genUsers)
                  <*> arbitrary
            ),
            (1, GetTags <$> arbitrary),
            (if null $ articles m then 0 else 1, GetArticle <$> genArticles),
            (if null $ articles m then 0 else 1, GetComments <$> genArticles <*> arbitrary)
          ]
      genAuthCommand =
        frequency
          [ (if null $ users m then 3 else 1, Register <$> arbitraryRealistic),
            (if null $ credentials m then 0 else 1, uncurry Login <$> genCredentials)
          ]
      genUserCommand =
        frequency
          [ (1, pure GetCurrentUser),
            (1, UpdateUser <$> arbitraryRealistic),
            (if null $ users m then 0 else 1, UnfollowUser <$> genUsers),
            (if null $ users m then 0 else 1, FollowUser <$> genUsers),
            (if null $ articles m then 8 else 2, CreateArticle <$> arbitraryRealistic),
            (if null $ articles m then 0 else 1, UpdateArticle <$> genArticles <*> arbitraryRealistic),
            (if null $ articles m then 0 else 1, DeleteArticle <$> genArticles),
            ( if null $ articles m
                then 0
                else if null $ comments m then 8 else 3,
              AddCommentToArticle <$> genArticles <*> arbitraryRealistic
            ),
            (if null (comments m) || null (articles m) then 0 else 1, DeleteComment <$> genArticles <*> genComments),
            (if null $ articles m then 0 else 1, FavoriteArticle <$> genArticles),
            (if null $ articles m then 0 else 1, UnfavoriteArticle <$> genArticles),
            (1, FeedArticles <$> arbitrary)
          ]
   in pure $
        frequency
          [ (if null (users m) || null (tokens m) then 3 else 1, AuthCommand <$> genTokens <*> genAuthCommand),
            (1, VisitorCommand <$> genTokens <*> genVisitCommand),
            (2, UserCommand <$> genTokens <*> genUserCommand)
          ]

shrinker :: Model r -> Command r -> [Command r]
shrinker _ =
  \case
    AuthCommand m_ref ac ->
      AuthCommand m_ref <$> case ac of
        Register ur -> Register <$> shrinkRealistic ur
        Login _ _ -> []
    VisitorCommand _ _ -> []
    UserCommand m_ref uc ->
      UserCommand m_ref <$> case uc of
        GetCurrentUser -> []
        UpdateUser ur -> UpdateUser <$> shrinkRealistic ur
        FollowUser _ -> []
        UnfollowUser _ -> []
        CreateArticle ar -> CreateArticle <$> shrinkRealistic ar
        UpdateArticle ref ar -> UpdateArticle ref <$> shrinkRealistic ar
        DeleteArticle _ -> []
        AddCommentToArticle ref cr -> AddCommentToArticle ref <$> shrinkRealistic cr
        DeleteComment _ _ -> []
        FavoriteArticle _ -> []
        UnfavoriteArticle _ -> []
        FeedArticles _ -> []
