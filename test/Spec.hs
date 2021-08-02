import Control.Monad (when)
import Data.Sequence (index)
import FCompiler (translateProgram)
import Lexer (tokenize)
import MF (HeapCell (VAL), MachineState (..), StackCell (HeapAddr), Value (..), runMF)
import Parser (parseProgram)
import Run(test)

main :: IO ()
main = do
  when (test "main = 1 + 2;" /= IntValue 3) $ error "failed"
