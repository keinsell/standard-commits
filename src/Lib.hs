module Lib where

import Control.Applicative
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.Functor (($>))
import Data.List (dropWhileEnd, intercalate, unfoldr)
import Data.Maybe (isJust, isNothing)
import Text.Parsec
import Text.Parsec.String (Parser)

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
      [ string "?" $> Just PossiblyBreaking,
        string "!" $> Just Breaking,
        string "!!" $> Just Critical
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
parseBody = optionMaybe $ manyTill anyChar (try blankLine Control.Applicative.<|> eof)
  where
    blankLine = try (string "\n\n" $> ())

-- Starts with uppercase letter
-- Contains tag each newline -> `<key>: <value>`
-- Must be separated from body by a blank line
-- MUST not repeat info from structured fragment
-- Organized in short, clear paragraph
-- -- `Breaking:` - described breaking changes
-- -- `Fixes: #N` - closes referenced issues
-- -- `Co-Authored-by:` - attributes co-ownership
parseFooter :: Parser (Maybe [(String, String)])
parseFooter = do
  lines <- filter (not . null) <$> sepEndBy (Text.Parsec.many $ satisfy (`notElem` ['\r', '\n'])) endOfLine
  eof
  let entries = unfoldr groupEntry lines
  return $ if null entries then Nothing else Just entries
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    isValidKey k = not (null k) && isUpper (head k) && all (\c -> isAlphaNum c || c == '-') (tail k)
    parseKeyValue s = case break (== ':') s of
      (keyPart, ':' : valuePart) ->
        let key = trim keyPart
            value = dropWhile isSpace valuePart
         in if isValidKey key then Just (key, value) else Nothing
      _ -> Nothing
    groupEntry [] = Nothing
    groupEntry (l : ls) = case parseKeyValue l of
      Just (key, value) ->
        let (cont, rest) = span (isNothing . parseKeyValue) ls
            fullValue = intercalate "\n" (value : cont)
         in Just ((key, fullValue), rest)
      Nothing -> groupEntry ls

parseCommitMessage :: Parser StandardCommit
parseCommitMessage = StandardCommit <$> parseVerb <*> parseImportance <*> parseScope <*> parseReason <* char ':' <* space <*> parseSummary <*> parseBody <*> parseFooter

-- | Parses a commit message in the Standard Commits format.
parseStandardCommitMessage :: CommitMsg -> Either ParseError StandardCommit
parseStandardCommitMessage commitMsg =
  case parse parseCommitMessage "CommitMsg" commitMsg of
    Left err -> Left err
    Right msg -> Right msg
