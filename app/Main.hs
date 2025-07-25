module Main (main) where

import Lib

main :: IO ()
main = parseStandardCommitMessage "add!(lib/type-check)[rel]: enforce type checking in function calls"
