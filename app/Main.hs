module Main(main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Console.Pretty (Color(..), Style(..), color, style)
import System.Exit (exitFailure)
import Run

-- Take in multi-line input until empty line
multiline :: IO String
multiline = do
  s <- getLine
  case s of
    "" -> return s
    _ -> fmap (s ++) multiline

checkArgs :: [String] -> IO ()
checkArgs [] = return ()
checkArgs (arg : args)
  | arg `elem` flags = checkArgs args
  | otherwise = do
    putStrLn $ color Red ("Invalid option " ++ show arg) ++ "\n"
               ++ "Possible options: " ++ intercalate ", " flags
    exitFailure
    where
      flags = ["-lex", "-parse", "-code", "-step", "-trace"]

asciiLogo :: String
asciiLogo = "       _____  _\n" ++
            "      |  ___|(_) _ __\n" ++
            "      | |_   | || '_ \\\n" ++ 
            "      |  _|  | || | | |\n" ++
            color Blue "~~~~~~" ++ "|_|" ++ color Blue "~~~~" ++ "|_||_| |_|" ++ color Blue "~~~~~~" ++ "\n" ++
            color Blue "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

main :: IO ()
main =
  do
    args <- getArgs
    checkArgs args
    putStrLn asciiLogo
    putStrLn $ style Bold "Please enter the F program here" ++ " (end with an empty line)" ++ style Bold ":"
    input <- multiline
    putStrLn $
      runFin
        Options
          { lexOpt = "-lex" `elem` args,
            parseOpt = "-parse" `elem` args,
            codeOpt = "-code" `elem` args,
            stepOpt = "-step" `elem` args,
            traceOpt = "-trace" `elem` args
          }
        input