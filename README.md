# Fin - An F Interpreter <img src="fin_logo.jpeg" width=90px>

**Fin** is an implementation of the simple functional language F.

F supports function definitions, basic arithmetic computations, recursive let expressions and lazy evaluation. However, it is untyped and does not support higher-order functions, lambda abstractions or local function definitions.

The implementation consists of four parts.
- [`Lexer.hs`](src/Lexer.hs) tokenizes the input program into tokens. A variable name can contain letters, numbers and underscores, but it has to start with a letter.
- [`Parser.hs`](src/Parser.hs) implements a recursive descent parser using the following LL(1) grammar:
```
AtomicExpression ::= Variable | Literal | ( Expression0 )
Expression8 ::= AtomicExpression {AtomicExpression}
Expression7 ::= [-] Expression8
Expression6 ::= Expression7 RestExpression6
RestExpression6 ::= / Expression7 | {* Expression7}
Expression5 ::= Expression6 RestExpression5
RestExpression5 ::= - Expression6 | {+ Expression6}
Expression4 ::= Expression5 [== Expression5] | Expression5 [< Expression5]
Expression3 := [not] Expression4
Expression2 := Expression3 [& Expression2]
Expression1 := Expression2 [| Expression1]
Expression0 := if Expression0 then Expression0 else Expression0
             | let LocalDefinitions in Expression0
             | Expression1
LocalDefinitions := LocalDefinition {; LocalDefinition}
LocalDefinition := Variable = Expression0

Definition := Variable {Variable} = Expression0
Program := Definition ; {Definition ;} 
```
- [`FCompiler.hs`](src/FCompiler.hs) compiles the program and generates the initial machine state to be executed.
- [`MF.hs`](src/MF.hs) defines the abstract machine MF for F. It executes the MF instructions and delivers the result.

## How to build Fin

The project requires [stack](https://docs.haskellstack.org/en/stable/README/).
After stack is successfully installed, build the project as follows:
```
stack build
```

## How to run Fin

The project can be run as follows:
```
stack run -- [file path] [flags]
```
The F program can either be given as a file or as input on the terminal. Only one file path can be provided and only one program is allowed in the file. Regarding giving the program on the terminal, it can be multiple lines and is terminated with an empty line. The F program will be parsed, compiled and run automatically.

The possible flags are: `-lex`, `-parse`, `-code`, `-step` and `-trace`.

- `-lex`: Output the tokens.
- `-parse`: Output the abstract syntax tree.
- `-code`: Output the compiled MF instructions.
- `-step`: Output the number of execution steps.
- `-trace`: Output the trace of the MF execution.

## Example
The following F program computes the next prime number after 90.
```
main = nextPrime 90;
nextPrime x = if isPrime x then x else nextPrime (x + 1);
isPrime x = not hasFactorBelow x x;
hasFactorBelow x y = if 2 < y then divides (y - 1) x | hasFactorBelow x (y - 1) else false;
divides x y = let remainder = y - (y / x) * x in remainder == 0; 
```

Fin tells us
```
>>> Result: 97
```

----
Group 6: Shiyi Gou, Simon Hehnen, Zhu Yang, Jing Yuan
<br>
Tutors: Philipp Zander, Kilian Rückschloß