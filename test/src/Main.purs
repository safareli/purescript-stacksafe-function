module Test.Main where

import Prelude

import StackSafe.Function (Func(..), run)
import Control.Monad.Eff (Eff)
import Test.Assert (ASSERT, assert')
import Test.Laws (laws)
import Test.StrongCheck.Laws (SC)

main :: SC (assert :: ASSERT) Unit
main = do
  testId
  testAssociativity
  laws
  stackSafety


plus10 :: Func Int Int
plus10 = Func (_ + 10)

plus20 :: Func Int Int
plus20 = Func (_ + 20)

plus40 :: Func Int Int
plus40 = Func (_ + 40)

plus80 :: Func Int Int
plus80 = Func (_ + 80)

testId :: forall eff. Eff (assert :: ASSERT |eff) Unit
testId = do
  assert' "80 + 0" ((plus80 >>> id) `run` 0 == 80 + 0)
  assert' "10 + 0" ((plus10 >>> id) `run` 0 == 10 + 0)
  assert' "10 + 0" ((plus10 >>> id) `run` 0 == 10 + 0)
  assert' "0 + 80" ((id >>> plus80) `run` 0 == 0 + 80)
  assert' "0 + 10" ((id >>> plus10) `run` 0 == 0 + 10)
  assert' "0 + 10" ((id >>> plus10) `run` 0 == 0 + 10)

testAssociativity :: forall eff. Eff (assert :: ASSERT |eff) Unit
testAssociativity = do
  assert' "(10 + 80) + 40" (((plus10 >>> plus80) >>> plus40) `run` 0 == (10 + 80) + 40)
  assert' "(20 + 10) + 40" (((plus20 >>> plus10) >>> plus40) `run` 0 == (20 + 10) + 40)
  assert' "(40 + 10) + 20" (((plus40 >>> plus10) >>> plus20) `run` 0 == (40 + 10) + 20)
  assert' "10 + (80 + 40)" ((plus10 >>> (plus80 >>> plus40)) `run` 0 == 10 + (80 + 40))
  assert' "20 + (10 + 40)" ((plus20 >>> (plus10 >>> plus40)) `run` 0 == 20 + (10 + 40))
  assert' "40 + (10 + 20)" ((plus40 >>> (plus10 >>> plus20)) `run` 0 == 40 + (10 + 20))

stackSafety :: forall eff. Eff (assert :: ASSERT |eff) Unit
stackSafety =
  assert'
    "is stack safe and correct"
    (composeGo (Func (_ + 1)) id 0 `run` 0 == 100000)

composeGo :: forall a. Func a a -> Func a a -> Int -> Func a a
composeGo f acc n = if n == 100000 then acc else composeGo f (compose acc f) (n + 1)
