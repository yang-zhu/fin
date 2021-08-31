# Fin - An F Interpreter <img src="static/fin_logo.jpeg" width=90px>

**Fin** is an implementation of the simple functional language F.

F supports function definitions, basic arithmetic computations, recursive let expressions and lazy evaluation. However, it is untyped and does not support higher-order functions, lambda abstractions or local function definitions.

The implementation consists of four parts.
- [`Lexer.hs`](frontend/src/Lexer.hs) tokenizes the input program into tokens. A variable name can contain letters, numbers and underscores, but it has to start with a letter.
- [`Parser.hs`](frontend/src/Parser.hs) implements a recursive descent parser using the following LL(1) grammar:
```
LocalDefinitions ::= LocalDefinition {; LocalDefinition}
LocalDefinition ::= Variable {Variable} = Expression1
AtomicExpression ::= Variable | Literal | ( Expression1 )
                   | \ {Variable} . Expression1
                   | if Expression1 then Expression1 else Expression1
                   | let LocalDefinitions in Expression1
Expression8 ::= AtomicExpression {AtomicExpression}
Expression7 ::= [-] Expression8
Expression6 ::= Expression7 RestExpression6
RestExpression6 ::= / Expression7 | {* Expression7}
Expression5 ::= Expression6 RestExpression5
RestExpression5 ::= - Expression6 | {+ Expression6}
Expression4 ::= Expression5 [== Expression5] | Expression5 [< Expression5]
Expression3 ::= [not] Expression4
Expression2 ::= Expression3 [& Expression2]
Expression1 ::= Expression2 [| Expression1]

Definition ::= Variable {Variable} = Expression1
Program ::= Definition ; {Definition ;} 
```
- [`FCompiler.hs`](frontend/src/FCompiler.hs) compiles the program and generates the initial machine state to be executed.
- [`MF.hs`](frontend/src/MF.hs) defines the abstract machine MF for F. It executes the MF instructions and delivers the result.

## How to build Fin

1. Follow step 1 and 2 of https://github.com/obsidiansystems/obelisk/#installing-obelisk. There is no need to perform step 3.
2. Execute

```shell
mkdir result
ln -s $(nix-build -A exe --no-out-link)/* result/
cp -r config result
(cd result && ./backend)
```

as described on https://github.com/obsidiansystems/obelisk#locally.

## How to develop Fin

[Install Obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk). The following options are available.

- Execute `ob run` for a ghcid window. The web server is updated automatically. `-- $>` code comments are supported.
- Execute `ob repl` for a ghci prompt. Load a module using `:l Run`.

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