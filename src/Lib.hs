module Lib where

import Control.Applicative
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

-- ^ A <verb> describes how something has changed via an expectation. An expectation is a requirement that the code should respect. The Standard Commits format provides a set of predefined verbs to ensure consistency and clarity in commit messages, and other verbs SHOULD be avoided.
-- ^ Implies the change MUST NOT be particularly relevant for maintainers or users. This field is a marker that is intended to be applied only to specific commits that maintainers/users should pay attention to.

data Verb
  = -- | Adds an expectation Introduces new content to the repository with the expectation that it SHALL behave as intended.
    Add
  | Remove
  | Refactor
  | Fix
  | Undo
  | Release
  deriving (Show, Eq)

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

type ScopeContext = Maybe String

data Scope
  = -- | `exe` (executable) The change affects the executable part of the project.
    Executable (ScopeContext)
  | BackendLibrary (ScopeContext)
  | Testing (ScopeContext)
  | Building (ScopeContext)
  | Documentation (ScopeContext)
  | ContinousIntegration (ScopeContext)
  | ContinuousDelivery (ScopeContext)
  deriving
    (Show, Eq)

data Reason = Introduction | Preliminary | Efficiency | Reliability | Compatibility | Temporary | Experiment | Security | Upgrade | UserExperience | Policy | Styling deriving (Show, Eq)

-- <verb><importance?>(<scope?>)[<reason?>]: <summary>
--
-- <body?>
--
-- <footer?>
data StandardCommit = StandardCommit
  { verb :: Verb,
    importance :: Maybe Importance,
    summary :: String,
    scope :: Maybe Scope,
    reason :: Maybe Reason
  }
  deriving (Show, Eq)

type CommitMsg = String

-- | Parses the verb of a commit message, accepting both full and shorthand forms.
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
      string "ref" *> pure Refactor
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
    Text.Parsec.<|> pure Nothing

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
    moduleName <- optionMaybe (char '/' *> many1 (noneOf ")[]:"))
    _ <- char ')'
    pure $ scopeKind moduleName

reasonAbbreviations :: [(String, Reason)]
reasonAbbreviations =
  [ ("int", Introduction),
    ("pre", Preliminary),
    ("eff", Efficiency),
    ("rel", Reliability),
    ("cmp", Compatibility),
    ("tmp", Temporary),
    ("exp", Experiment),
    ("sec", Security),
    ("upg", Upgrade),
    ("ux", UserExperience),
    ("pol", Policy),
    ("sty", Styling)
  ]

-- Use `choice` to create parser that would match any Reason string into actual Reason datatype.
-- ? Is it possible to have Read instance on Reason datatype and use it instead defining the reasonAbbreviations variable?
reasonParser :: Parser Reason
reasonParser = choice [try (string abbr *> pure r) | (abbr, r) <- reasonAbbreviations]

-- | Parse Reason from a string, which is expected to be between square brackets, e.g. [upg] will parse to Just Upgrade.
parseReason :: Parser (Maybe Reason)
parseReason =
  optionMaybe $
    between (char '[') (char ']') $
      reasonParser

parseSummary :: Parser String
parseSummary = manyTill anyChar (eof Control.Applicative.<|> void newline)

parseCommitMessage :: Parser StandardCommit
parseCommitMessage = do
  v <- parseVerb
  imp <- parseImportance
  sc <- parseScope
  reason <- parseReason
  _ <- char ':'
  _ <- space
  summary <- parseSummary
  pure $ StandardCommit v imp summary sc reason

-- | Parses a commit message in the Standard Commits format.
parseStandardCommitMessage :: CommitMsg -> Either ParseError StandardCommit
parseStandardCommitMessage commitMsg =
  case parse parseCommitMessage "CommitMsg" commitMsg of
    Left err -> Left err
    Right msg -> Right msg
