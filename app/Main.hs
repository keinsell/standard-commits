module Main (main) where

import Lib

commitMessage = "add!(lib/type-check)[rel]: enforce type checking in function calls\nHelloWorld!"

main :: IO ()
main =
  case parseStandardCommitMessage commitMessage of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> do
      putStrLn $ "Commit message: " ++ commitMessage
      putStrLn "--------------"
      putStrLn $ show msg
