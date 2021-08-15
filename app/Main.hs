module Main(main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Console.Pretty (Color(..), Style(..), color, style)
import System.Exit (exitFailure)
import Run
import Control.Exception (catch, IOException)

-- Take in multi-line input until empty line
multiline :: IO String
multiline = do
  s <- getLine
  case s of
    "" -> return s
    _ -> ((s ++ "\n") ++) <$> multiline

-- Check if the command line arguments are valid. Otherwise give help information. Return the file path if only one is provided.
checkArgs :: [String] -> IO (Maybe String)
checkArgs [] = return Nothing
checkArgs (arg@('-' : _) : args)
  | arg `elem` flags = checkArgs args
  | otherwise = do
    putStrLn $ color Red ("Invalid option " ++ show arg) ++ "\n"
               ++ "Possible options: " ++ intercalate ", " flags
    exitFailure
    where
      flags = ["-lex", "-parse", "-code", "-step", "-trace"]
checkArgs (path : args) = do
  otherPath <- checkArgs args
  case otherPath of
    Just _ -> do
      putStrLn $ color Red "Invalid arguments: " ++ "Only one file path is allowed."
      exitFailure
    Nothing -> return (Just path)

asciiLogo :: String
asciiLogo = "       _____  _\n" ++
            "      |  ___|(_) _ __\n" ++
            "      | |_   | || '_ \\\n" ++ 
            "      |  _|  | || | | |\n" ++
            color Blue "~~~~~~" ++ "|_|" ++ color Blue "~~~~" ++ "|_||_| |_|" ++ color Blue "~~~~~~" ++ "\n" ++
            color Blue "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

main :: IO ()
main = do
  args <- getArgs
  maybePath <- checkArgs args
  input <- case maybePath of
    Just path -> catch (readFile path)
        (\e -> do 
          let _ = (e :: IOException)
          putStrLn $ color Red "Error:" ++ " Couldn't open file " ++ show path
          exitFailure)
    Nothing -> do
      putStrLn asciiLogo
      putStrLn $ style Bold "Please enter the F program here" ++ " (end with an empty line)" ++ style Bold ":"
      multiline
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