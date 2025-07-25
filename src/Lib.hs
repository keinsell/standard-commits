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
parseVerb = do
    verb <- choice [
        string "add" *> pure Add,
        string "remove" *> pure Remove,
        string "refactor" *> pure Refactor,
        string "fix" *> pure Fix,
        string "undo" *> pure Undo,
        string "release" *> pure Release
        ]
    pure verb

parseStandardCommitMessage :: String -> IO ()
parseStandardCommitMessage commitMsg = do
    case parse parseVerb "CommitMsg" commitMsg of
        Left err -> putStrLn $ "Error: " ++ show err
        Right msg -> print msg
