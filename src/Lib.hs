module Lib where

import Control.Applicative
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

-- import qualified Data.Map as Map

-- ^ A <verb> describes how something has changed via an expectation. An expectation is a requirement that the code should respect. The Standard Commits format provides a set of predefined verbs to ensure consistency and clarity in commit messages, and other verbs SHOULD be avoided.
-- ^ Implies the change MUST NOT be particularly relevant for maintainers or users. This field is a marker that is intended to be applied only to specific commits that maintainers/users should pay attention to.
-- | Spec: <verb> [REQUIRED]. Defines the action type of the commit message.

data Verb
  = -- | Add: Introduces new content to the repository as a new feature or enhancement.
    Add
  | -- | Remove: Removes content as a part of deprecation or cleanup.
    Remove
  | -- | Refactor: Restructures existing code without changing its external behavior.
    Refactor
  | -- | Fix: Corrects errors or bugs in the code.
    Fix
  | -- | Undo: Reverses previous changes.
    Undo
  | -- | Release: Marks a commit that publishes a new version or release.
    Release
  deriving (Show, Eq)

-- | Spec: <importance> [OPTIONAL]. Specifies the significance of the commit message change.
data Importance
  = -- | PossiblyBreaking: Non-breaking change indicated by '?'.
    PossiblyBreaking
  | -- | Breaking: Indicates a potentially breaking change using '!'.
    Breaking
  | -- | Critical: Indicates a critical change using '!!' that requires immediate update.
    Critical
  deriving
    ( Show,
      Eq
    )

type ScopeContext = Maybe String

-- | Spec: <scope> [OPTIONAL]. Specifies the target area of the changes, such as executable, library, testing, build, documentation, CI, or CD.
data Scope
  = -- | Executable: Change affects the executable component of the project.
    Executable (ScopeContext)
  | BackendLibrary (ScopeContext)
  | Testing (ScopeContext)
  | Building (ScopeContext)
  | Documentation (ScopeContext)
  | ContinousIntegration (ScopeContext)
  | ContinuousDelivery (ScopeContext)
  deriving
    (Show, Eq)

-- | Spec: <reason> [OPTIONAL]. Represents the underlying motivation for the commit (e.g., bug fix, improvement, or stylistic change).
data Reason
  = -- | Introduction: Introduces a new feature or functionality.
    Introduction
  | -- | Preliminary: Indicates preliminary work paving the way for future changes.
    Preliminary
  | -- | Efficiency: Improves performance or resource usage.
    Efficiency
  | -- | Reliability: Increases system reliability.
    Reliability
  | -- | Compatibility: Enhances compatibility with other systems.
    Compatibility
  | -- | Temporary: Represents a temporary fix or workaround.
    Temporary
  | -- | Experiment: Experimental changes.
    Experiment
  | -- | Security: Enhances security measures.
    Security
  | -- | Upgrade: Upgrades an aspect of the system.
    Upgrade
  | -- | UserExperience: Improves the user experience.
    UserExperience
  | -- | Policy: Aligns code with updated policies.
    Policy
  | -- | Styling: Modifies styling or formatting.
    Styling
  deriving (Show, Eq)

-- | Spec: Standard Commit. A complete parsed commit message including <verb>, <importance>, <scope>, <reason>, <summary>, <body>, and <footer>.
data StandardCommit = StandardCommit Verb (Maybe Importance) (Maybe Scope) (Maybe Reason) CommitMsg (Maybe String) (Maybe [(String, String)])
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

parseBody :: Parser (Maybe String)
-- fmap applies function inside Functor.
-- There we define parser and we transform it's output with use of lambda
parseBody = fmap (\body -> if null body then Nothing else Just body) (manyTill anyChar (eof Control.Applicative.<|> void newline))

-- Starts with uppercase letter
-- Contains tag each newline -> `<key>: <value>`
-- Must be separated from body by a blank line
-- MUST not repeat info from structured fragment
-- Organized in short, clear paragraph
-- -- `Breaking:` - described breaking changes
-- -- `Fixes: #N` - closes referenced issues
-- -- `Co-Authored-by:` - attributes co-ownership
--
-- Example:
--
-- add!(lib/type-check)[rel]: enforce type checking in function calls

-- Previously, the semantic analyzer allowed mismatched parameter types
-- in function calls, leading to runtime errors. This fix implements
-- strict type validation during the semantic analysis phase.

-- Breaking: The `validateCall` function now returns `TypeMismatchError`
--   instead of returning a boolean, requiring updates in error handling.
-- Fixes: #247
-- Co-authored-by: Foo Bar <foo.bar@compiler.dev>
parseFooter :: Parser (Maybe [(String, String)])
parseFooter = return Nothing

parseCommitMessage :: Parser StandardCommit
parseCommitMessage = StandardCommit <$> parseVerb <*> parseImportance <*> parseScope <*> parseReason <* char ':' <* space <*> parseSummary <*> parseBody <*> parseFooter

-- | Parses a commit message in the Standard Commits format.
parseStandardCommitMessage :: CommitMsg -> Either ParseError StandardCommit
parseStandardCommitMessage commitMsg =
  case parse parseCommitMessage "CommitMsg" commitMsg of
    Left err -> Left err
    Right msg -> Right msg
