{-# OPTIONS_GHC
  -Wno-unused-top-binds
#-}

{-# language
  QuasiQuotes,
  OverloadedStrings,
  LambdaCase
#-}

import FInterface

import Test.Hspec
import Test.Hspec.Runner
  (runSpec, defaultConfig, evaluateSummary, configFormatter)
import Test.Hspec.Formatters (progress)
import Data.Text (Text)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "functions" $ do
    it "can have one arguments" $
      eval "main = id 0; id a = a;" `shouldBe` Right (Integer 0)
    it "can have two arguments" $
      eval ("main = k1 0 1;" <> k1) `shouldBe` Right (Integer 1)
    it "can have three arguments" $
      eval "main = f 0 1 2; f a b c = c;" `shouldBe` Right (Integer 2)
    it "are lazy" $
      eval ("main = k1 bottom 0;" <> k1 <> bottom)
      `shouldBe`
      Right (Integer 0)
    it "are lazy with strict functions in arguments" $
      eval ("main = k1 (bottom + bottom) 0;" <> k1 <> bottom)
      `shouldBe`
      Right (Integer 0)
    it "definitions can contain functions" $
      eval ("main = f 0 1 2; f a b c = k1 a b;" <> k1)
      `shouldBe`
      Right (Integer 1)
    it "arguments can contain functions" $
      eval ("main = k1 0 (k1 1 2);" <> k1) `shouldBe` Right (Integer 2)
    it "definitions can contain primitives" $
      eval "main = square 3; square a = a * a;"
      `shouldBe`
      Right (Integer 9)
    it "arguments can contain primitives" $
      eval ("main = k1 0 (1 + 2);" <> k1) `shouldBe` Right (Integer 3)
    it "need not be used" $
      eval ("main = 0;" <> bottom) `shouldBe` Right (Integer 0)
  describe "not" $ do
    it "works on variables" $
      eval "main = not a; a = false;" `shouldBe` Right (Bool True)
    it "works as a first disjunct" $
      eval "main = not true | true;" `shouldBe` Right (Bool True)
    it "works as a second disjunct" $ -- grammar dependent
      eval "main = true | not true;" `shouldBe` Right (Bool True)
    it "works as a first conjunct" $
      eval "main = not false & false;" `shouldBe` Right (Bool False)
    it "works as a second conjunct" $ -- grammar dependent
      eval "main = false & not false;" `shouldBe` Right (Bool False)
    it "works as a first comparison operand" $
      eval "main = not false == false;" `shouldBe` Right (Bool False)
    -- it "works as a second comparison operand" $
    --   eval "main = false == not false;" `shouldBe` Right (Bool False)
  describe "unary -" $ do
    it "works on variables" $
      eval "main = -a; a = 0;" `shouldBe` Right (Integer 0)
    it "works as a first summand" $ -- grammar dependent
      eval "main = -2 + 4;" `shouldBe` Right (Integer 2)
    it "works as a second summand" $ -- grammar dependent
      eval "main = 4 + -2;" `shouldBe` Right (Integer 2)
    it "works as a first factor" $
      eval "main = -2 * 4;" `shouldBe` Right (Integer (-8))
    -- it "does not work as a second factor" $
    --   eval "main = 4 * -2;" `shouldSatisfy` isLeft
    it "works as a first comparison operand" $
      eval "main = -2 < 4;" `shouldBe` Right (Bool True)
    it "works as a second comparison operand" $
      eval "main = 4 < -2;" `shouldBe` Right (Bool False)
  describe "|" $ do
    it "works" $
      (
        eval "main = false | false;",
        eval "main = false | true;",
        eval "main = true | false;",
        eval "main = true | true;"
      )
      `shouldBe`
      (
        Right (Bool False),
        Right (Bool True),
        Right (Bool True),
        Right (Bool True)
      )
  describe "&" $ do
    it "works" $
      (
        eval "main = false & false;",
        eval "main = false & true;",
        eval "main = true & false;",
        eval "main = true & true;"
      )
      `shouldBe`
      (
        Right (Bool False),
        Right (Bool False),
        Right (Bool False),
        Right (Bool True)
      )
  describe "+" $ do
    it "works" $
      eval "main = 6 + 2;" `shouldBe` Right (Integer 8)
    it "arguments can contain functions" $
      eval "main = 6 + (id 2); id a = a;" `shouldBe` Right (Integer 8)
  describe "-" $ do
    it "works" $
      eval "main = 6 - 2;" `shouldBe` Right (Integer 4)
    it "arguments can contain functions" $
      eval "main = 6 - (id 2); id a = a;" `shouldBe` Right (Integer 4)
  describe "*" $ do
    it "works" $
      eval "main = 6 * 2;" `shouldBe` Right (Integer 12)
    it "arguments can contain functions" $
      eval "main = 6 * (id 2); id a = a;" `shouldBe` Right (Integer 12)
  describe "/" $ do
    it "works" $
      eval "main = 6 / 2;" `shouldBe` Right (Integer 3)
    it "arguments can contain functions" $
      eval "main = 6 / (id 2); id a = a;" `shouldBe` Right (Integer 3)
  describe "==" $ do
    it "works for unequal booleans" $
      eval "main = false == true;" `shouldBe` Right (Bool False)
    it "works for equal booleans" $
      eval "main = false == false;" `shouldBe` Right (Bool True)
    it "works for unequal integers" $
      eval "main = 6 == 2;" `shouldBe` Right (Bool False)
    it "works for equal integers" $
      eval "main = 6 == 6;" `shouldBe` Right (Bool True)
  describe "<" $ do
    it "works for descending integers" $
      eval "main = 6 < 2;" `shouldBe` Right (Bool False)
    it "works for ascending integers" $
      eval "main = 2 < 6;" `shouldBe` Right (Bool True)
  describe "if" $ do
    it "avoids Ziwen's mistake" $
      eval "main = if (2 == 2) then 2 else 3;" `shouldBe` Right (Integer 2)
    it "works for false" $
      eval "main = if false then 2 else 3;" `shouldBe` Right (Integer 3)
    it "works for true" $
      eval "main = if true then 2 else 3;" `shouldBe` Right (Integer 2)
    it "is lazy for false" $
      eval "main = if false then bottom else 3; bottom = bottom;"
      `shouldBe`
      Right (Integer 3)
    it "is lazy for true" $
      eval "main = if true then 2 else bottom; bottom = bottom;"
      `shouldBe`
      Right (Integer 2)
  describe "let" $ do
    it "works for one definition" $
      eval "main = let a = 0 in a;" `shouldBe` Right (Integer 0)
    it "works for two definitions" $
      eval "main = let a = 0; b = 0 in a + b;" `shouldBe` Right (Integer 0)
    it "works for four definitions" $
      eval "main = let a = 0; b = 0; c = 0; d = 0 in a + b + c;"
      `shouldBe`
      Right (Integer 0)
    it "causes previous bindings to be in scope" $
      eval "main = let a = 0; b = a in a + b;" `shouldBe` Right (Integer 0)
    it "needs not be used" $
      eval "main = let a = 0 in 0;" `shouldBe` Right (Integer 0)
    it "does not interfere with parameters" $
      eval "main = f 2; f a = let b = 3 in a + b;" `shouldBe` Right (Integer 5)
  describe "sharing" $ do
    it "does not interfere with the global environment" $
      eval "main = a + b; a = 0; b = a;" `shouldBe` Right (Integer 0)
    it "works for let bound constants" $
      do
        let4 <- evaluationStepCount "\
          \main = \
          \  let \
          \    v3 = 0; \
          \    v2 = v3 + v3; \
          \    v1 = v2 + v2; \
          \    v0 = v1 + v1 \
          \  in v0;"
        let6 <- evaluationStepCount "\
          \main = \
          \  let \
          \    v5 = 0; \
          \    v4 = v5 + v5; \
          \    v3 = v4 + v4; \
          \    v2 = v3 + v3; \
          \    v1 = v2 + v2; \
          \    v0 = v1 + v1 \
          \  in v0;"
        let8 <- evaluationStepCount "\
          \main = \
          \  let \
          \    v7 = 0; \
          \    v6 = v7 + v7; \
          \    v5 = v6 + v6; \
          \    v4 = v5 + v5; \
          \    v3 = v4 + v4; \
          \    v2 = v3 + v3; \
          \    v1 = v2 + v2; \
          \    v0 = v1 + v1 \
          \  in v0;"
        let
          difference0 = let6 - let4
          difference1 = let8 - let6
        pure
          (
            fromIntegral difference0 / fromIntegral difference1 :: Double,
            difference0 :: Int,
            difference1 :: Int,
            fromIntegral let6 / fromIntegral let4 :: Double,
            fromIntegral let8 / fromIntegral let6 :: Double
          )
      `shouldSatisfy`
      (\case
        Right (differenceQuotient, _, _, _, _) ->
           abs (differenceQuotient - 1) < 0.1
        _ -> False
      )
    it "works for constant function arguments" $
      do
        square4 <-
          evaluationStepCount
            ("main = square (square (square (square 0)));" <> square)
        square6 <-
          evaluationStepCount
            ("main = square (square (square (square (square (square 0)))));" <> square)
        square8 <-
          evaluationStepCount
            ("main = square (square (square (square (square (square (square (square 0)))))));" <> square)
        let
          difference0 = square6 - square4
          difference1 = square8 - square6
        pure
          (
            fromIntegral difference0 / fromIntegral difference1 :: Double,
            difference0 :: Int,
            difference1 :: Int,
            fromIntegral square6 / fromIntegral square4 :: Double,
            fromIntegral square8 / fromIntegral square6 :: Double
          )
      `shouldSatisfy`
      (\case
        Right (differenceQuotient, _, _, _, _) ->
           abs (differenceQuotient - 1) < 0.1
        _ -> False
      )
    -- it "works for function function arguments" $
    --   do
    --     twice4 <-
    --       evaluationStepCount
    --         ("main = twice (twice (twice (twice id))) 0;" <> twice)
    --     twice6 <-
    --       evaluationStepCount
    --         ("main = twice (twice (twice (twice (twice (twice id))))) 0;" <> twice)
    --     twice8 <-
    --       evaluationStepCount
    --         ("main = twice (twice (twice (twice (twice (twice (twice (twice id))))))) 0;" <> twice)
    --     let
    --       difference0 = twice6 - twice4
    --       difference1 = twice8 - twice6
    --     pure
    --       (
    --         fromIntegral difference0 / fromIntegral difference1 :: Double,
    --         difference0 :: Int,
    --         difference1 :: Int,
    --         fromIntegral twice6 / fromIntegral twice4 :: Double,
    --         fromIntegral twice8 / fromIntegral twice6 :: Double
    --       )
    --   `shouldSatisfy`
    --   (\case
    --     Right (differenceQuotient, _, _, _, _) ->
    --        abs (differenceQuotient - 1) < 0.1
    --     _ -> False
    --   )
  describe "the implementation" $ do
    it "can compute factorial" $
      eval "bool x = x == true | x == false;\
           \f x = if bool x | x < 1 \
           \      then 1 \
           \      else x * f (x - 1);\
           \main = f 6;"
      `shouldBe` Right (Integer 720)
    it "can compute the next prime number" $
      eval "main = nextPrime 90; \
           \nextPrime x = if isPrime x \
           \              then x \
           \              else nextPrime (x + 1); \
           \isPrime x = not hasFactorBelow x x; \
           \hasFactorBelow x y = if 2 < y \
           \                     then divides (y - 1) x | hasFactorBelow x (y - 1) \
           \                     else false; \
           \divides x y = \
           \  let \
           \    remainder = y - (y / x) * x \
           \  in remainder == 0;"
      `shouldBe` Right (Integer 97)
    it "can compute gcd" $
      eval "main = gcd 75 125; \
           \gcd x y = if y == 0 \
           \          then x \
           \          else gcd y (mod x y); \
           \mod x y = x - (x / y) * y;"
        `shouldBe` Right (Integer 25)
    it "can approximate square root" $
      eval "main = sqrt 1024; \
           \sqrt x = newton x 1; \
           \newton x old_guess = \
           \  let \
           \    new_guess = (old_guess + x / old_guess) / 2 \
           \  in if abs (new_guess - old_guess) < 1 \
           \     then new_guess \
           \     else newton x new_guess; \
           \abs x = if 0 < x then x else -x;"
      `shouldBe` Right (Integer 32)
    it "can define fix with let" $
      eval "main = fix summation 100; \
            \summation recursion n = if n == 0 \
            \                        then n \
            \                        else n + recursion (n - 1); \
            \fix f x = let g = f g in g x;"
      `shouldBe` Right (Integer 5050)
  -- describe "higher order functions" $ do
  --   it "work" $
  --     eval "main = f 5; f = id; id a = a;" `shouldBe` Right (Integer 5)
  --   it "can compute the fifth prime number" $ do
  --     eval prime4Code `shouldBe` Right (Integer 11)

prime4Code :: Text
prime4Code =
  [r|
    main =
      drop 4 (foldr primesHelp nil (enumFrom 2)) k0 (-1);

    primes = foldr primesHelp nil (enumFrom 2);
    primesHelp head tail =
      cons head (filter (dividesNot head) tail);
    divides x y =
      let remainder = y - (y / x) * x
      in remainder == 0;

    dividesNot x y = not (divides x y);

    enumFrom lower = cons lower (enumFrom (lower+1));

    enumFromTo lower upper =
      if upper < lower
      then nil
      else cons lower (enumFromTo (lower+1) upper);

    filter predicate =
      foldr (filterHelp predicate) nil;
    filterHelp predicate head tail =
      if predicate head
      then cons head tail
      else tail;

    sum = foldr add 0;
    add a b = a + b;

    cons head tail consResult nilResult =
      consResult head tail;

    nil consResult nilResult = nilResult;

    k0 a b = a;
    k1 a b = b;

    foldr f seed list = list (foldrHelp f seed) seed;
    foldrHelp f seed head tail =
      f head (foldr f seed tail);

    drop n list =
      if n == 0
      then list
      else list (dropHelp n) nil;
    dropHelp n head tail = drop (n-1) tail;
  |]

k1 :: Text
k1 = " k1 a b = b;"

bottom :: Text
bottom = " bottom = bottom;"

square :: Text
square = " square n = n * n;"

-- twice :: Text
-- twice = " twice f x = f (f x); id x = x;"

main :: IO ()
main = hspec spec

hspecProgress :: Spec -> IO ()
hspecProgress spec =
  evaluateSummary
    =<< runSpec spec (defaultConfig {configFormatter = Just progress})

-- > hspecProgress spec
