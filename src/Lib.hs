module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)

-- ^ A <verb> describes how something has changed via an expectation. An expectation is a requirement that the code should respect. The Standard Commits format provides a set of predefined verbs to ensure consistency and clarity in commit messages, and other verbs SHOULD be avoided.

data Verb
  = -- | Adds an expectation Introduces new content to the repository with the expectation that it SHALL behave as intended.
    Add
  | Remove
  | Refactor
  | Fix
  | Undo
  | Release
  deriving (Show, Eq)
-- ^ Implies the change MUST NOT be particularly relevant for maintainers or users. This field is a marker that is intended to be applied only to specific commits that maintainers/users should pay attention to.

data Importance
  = -- | `?` (question) Changes something exposed externally, but not an API. It SHOULD NOT be breaking for projects that depend on the underlying repository.
    PossiblyBreaking
  | -- | `!` (exclamation) Changes something exposed externally, and MAY break projects that depend on the underlying repository.
    Breaking
  | -- | `!!` (loud exclamation) This change is critical ─ previous versions have severe issues that must be addressed. Projects depending on the underlying repository SHOULD update immediately. The <footer> MUST specify the last safe commit (Last-safe-commit).
    Critical
  deriving
    ( Show,
      Eq
    )

data Scope
  = -- | `exe` (executable) The change affects the executable part of the project.
    Executable (Maybe String)
  | BackendLibrary (Maybe String)
  | Testing (Maybe String)
  | Building (Maybe String)
  | Documentation (Maybe String)
  | ContinousIntegration (Maybe String)
  | ContinuousDelivery (Maybe String)
  deriving
    (Show, Eq)

data Reason = Introduction | Preliminary | Efficiency | Reliability | Compatibility | Temporary | Experiment | Security | Upgrade | UserExperience | Policy | Styling

-- <verb><importance?>(<scope?>)[<reason?>]: <summary>
--
-- <body?>
--
-- <footer?>
data StandardCommit = StandardCommit
  { verb :: Verb,
    importance :: Maybe Importance,
    summary :: String,
    scope :: Maybe Scope
  }
  deriving (Show, Eq)

type CommitMsg = String

parseVerb :: Parser Verb
parseVerb = do
  verb <-
    choice
      [ string "add" *> pure Add,
        string "remove" *> pure Remove,
        string "refactor" *> pure Refactor,
        string "fix" *> pure Fix,
        string "undo" *> pure Undo,
        string "release" *> pure Release
      ]
  pure verb
-- ^ Parses the importance of a commit message, which can be one of the
-- following: `?`, `!`, `!!`, or nothing.
-- If it is not specified, it defaults to `Nothing`.

parseImportance :: Parser (Maybe Importance)
parseImportance =
  do
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
        [ string "exe" *> pure Executable,
          string "lib" *> pure BackendLibrary,
          string "test" *> pure Testing,
          string "build" *> pure Building,
          string "docs" *> pure Documentation,
          string "ci" *> pure ContinousIntegration,
          string "cd" *> pure ContinuousDelivery
        ]
    mSub <- optionMaybe (char '/' *> many1 (noneOf ")[]:"))
    _ <- char ')'
    pure $ scopeKind mSub

parseCommitMessage :: Parser StandardCommit
parseCommitMessage = do
  verb <- parseVerb
  importance <- parseImportance
  scope <- parseScope
  --   Handle newline after first line
  pure $ StandardCommit verb importance "" scope

-- | Parses a commit message in the Standard Commits format.
parseStandardCommitMessage :: CommitMsg -> Either ParseError StandardCommit
parseStandardCommitMessage commitMsg =
  case parse parseCommitMessage "CommitMsg" commitMsg of
    Left err -> Left err
    Right msg -> Right msg