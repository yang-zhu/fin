module FInterface where

import qualified Data.Sequence as Seq
import Lexer (tokenize)
import Parser (parseProgram)
import FCompiler (translateProgram)
import MF hiding (Value (..))
import qualified MF

import Data.Text (Text)
import qualified Data.Text as T

data Value = Bool Bool | Integer Integer deriving (Eq, Show)

toValue :: MF.Value -> Value
toValue (MF.BoolValue value) = Bool value
toValue (MF.IntValue value) = Integer value

fromValue :: Value -> MF.Value
fromValue (Bool value) = MF.BoolValue value
fromValue (Integer value) = MF.IntValue value

-- roughly copied from Run.hs
emulate :: String -> Either String [MachineState]
emulate s = case tokenize s of
  Right tokens -> case parseProgram tokens of
    Right program -> case runMF $ translateProgram program of
      Right machinestates -> Right machinestates
      Left (err, _) -> Left $ "Runtime error: " ++ err
    Left err -> Left $ "Syntax error: " ++ err
  Left err -> Left $ "Lexical error: " ++ err
result :: [MachineState] -> MF.Value
result machinestates =
  let MachineState {stack, heap} = last machinestates
      HeapAddr hCell = head stack
      VAL res = value hCell heap
   in res

eval :: Text -> Either String Value
eval = fmap toValue . fmap result . emulate . T.unpack

evaluationStepCount :: Text -> Either String Int
evaluationStepCount = fmap length . emulate . T.unpack
