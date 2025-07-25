module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)

-- ^ A <verb> describes how something has changed via an expectation. An expectation is a requirement that the code should respect. The Standard Commits format provides a set of predefined verbs to ensure consistency and clarity in commit messages, and other verbs SHOULD be avoided.
data Verb
  = Add -- ^ Adds an expectation Introduces new content to the repository with the expectation that it SHALL behave as intended.
  | Remove
  | Refactor
  | Fix
  | Undo
  | Release deriving (Show, Eq)

data Importance = PossiblyBreaking | Breaking | Critical
data Scope = Executable | BackendLibrary | Testing | Building | Documentation | ContinousIntegration | ContinuousDelivery
data Reason = Introduction | Preliminary | Efficiency | Reliability | Compatibility | Temporary | Experiment | Security | Upgrade | UserExperience | Policy | Styling

-- <verb><importance?>(<scope?>)[<reason?>]: <summary>
--
-- <body?>
--
-- <footer?>
data CommitMsg = CommitMsg {
    verb :: Verb
} deriving (Show, Eq)



parseVerb :: Parser Verb
parseVerb =
  -- ? For sure this can be abstracted futher with map or smth
  choice
    [ try (string "refactor" *> pure Refactor),
      try (string "release" *> pure Release),
      try (string "remove" *> pure Remove),
      try (string "add" *> pure Add),
      try (string "undo" *> pure Undo),
      try (string "fix" *> pure Fix),
      try (string "rem" *> pure Remove),
      try (string "ref" *> pure Refactor)
    ]
-- ^ Parses the importance of a commit message, which can be one of the
-- following: `?`, `!`, `!!`, or nothing.
-- If it is not specified, it defaults to `Nothing`.

parseImportance :: Parser (Maybe Importance)
parseImportance =
  do
    -- ? For sure this can be abstracted futher with map or smth
    choice
      [ string "?" *> pure (Just PossiblyBreaking),
        string "!" *> pure (Just Breaking),
        string "!!" *> pure (Just Critical)
      ]
    <|> pure Nothing

-- (lib) || (lib/<lib>) || Nothing
parseScope :: Parser (Maybe Scope)
parseScope =
  optionMaybe $ do
    _ <- char '('
    scopeKind <-
      choice
        [ try (string "exe" *> pure Executable),
          try (string "lib" *> pure BackendLibrary),
          try (string "test" *> pure Testing),
          try (string "build" *> pure Building),
          try (string "docs" *> pure Documentation),
          try (string "ci" *> pure ContinousIntegration),
          try (string "cd" *> pure ContinuousDelivery)
        ]
    pure verb

parseStandardCommitMessage :: String -> IO ()
parseStandardCommitMessage commitMsg = do
    case parse parseVerb "CommitMsg" commitMsg of
        Left err -> putStrLn $ "Error: " ++ show err
        Right msg -> print msg
