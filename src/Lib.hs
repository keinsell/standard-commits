module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)

data Verb = Add |  Remove | Refactor | Fix | Undo | Release
data Importance = PossiblyBreaking | Breaking | Critical
data Scope = Executable | BackendLibrary | Testing | Building | Documentation | ContinousIntegration | ContinousDelivery
data Reason = Introduction | Preliminary | Efficiency | Reliability | Compatibility | Temporary | Experiment | Security | Upgrade | UserExperience | Policy | Styling

data CommitMsg = CommitMsg {
    verb :: Verb,
    importance :: Maybe Importance,
    scope :: Maybe Scope,
    reason :: Maybe Reason,
    summary :: Maybe String
}


parseStandardCommitMessage :: String -> IO ()
parseStandardCommitMessage commitMsg = do
    putStrLn commitMsg
