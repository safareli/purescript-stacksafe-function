module Test.Main where

import Prelude
import StackSafe.Function (type (-#>), Func(..), (#$))
import Test.Assert (assert', assertEqual, assertThrows')
import Effect.Console (log)
import Effect (Effect)
import Data.Foldable (all)
import Data.Enum (class BoundedEnum, enumFromTo)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary) 
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Type.Proxy (Proxy3(..))


main :: Effect Unit
main = do
  checkIdentity
  checkAssociativity
  checkEquivalenceToFunction
  laws
  checkStackSafety

addA :: String -> String
addA = (_ <> "a")

addB :: String -> String
addB = (_ <> "b")

addC :: String -> String
addC = (_ <> "c")

addD :: String -> String
addD = (_ <> "d")

addA' :: String -#> String
addA' = Func addA

addB' :: String -#> String
addB' = Func addB

addC' :: String -#> String
addC' = Func addC

addD' :: String -#> String
addD' = Func addD

checkIdentity :: Effect Unit
checkIdentity = do
  log "checking identity"
  assertEqual { expected: identity $ ""
              , actual: identity #$ "" }
  log "checking right identity"
  assertEqual { expected: addA <<< identity $ ""
              , actual: addA' <<< identity #$ "" }
  log "checking left identity"
  assertEqual { expected: identity <<< addA $ ""
              , actual: identity <<< addA' #$ "" }

checkAssociativity :: Effect Unit
checkAssociativity = do
  log "checking associativity"
  assertEqual { expected: (addA' <<< addB') <<< addC' #$ ""
              , actual:   addA' <<< (addB' <<< addC') #$ "" }

checkEquivalenceToFunction :: Effect Unit
checkEquivalenceToFunction = do
  log "checking that -#> is equivalent to ->"
  assertEqual { expected: (addA  <<< addB ) <<< (addC  <<< addD ) $ ""
              , actual:   (addA' <<< addB') <<< (addC' <<< addD') #$ "" }

  assertEqual { expected: addA  <<< (addB  <<< addC ) <<< addD   $ ""
              , actual:   addA' <<< (addB' <<< addC') <<< addD' #$ "" }

  assertEqual { expected: ((addA  <<< addB ) <<< addC ) <<< addD   $ ""
              , actual:   ((addA' <<< addB') <<< addC') <<< addD' #$ "" }

  assertEqual { expected: addA  <<< ((addB  <<< addC ) <<< addD )  $ ""
              , actual:   addA' <<< ((addB' <<< addC') <<< addD') #$ "" }

  let addAB  = addA  <<< addB
      addAB' = addA' <<< addB'
  assertEqual { expected: addAB  <<< addAB  <<< addAB   $ ""
              , actual:   addAB' <<< addAB' <<< addAB' #$ "" }

checkStackSafety :: Effect Unit
checkStackSafety = do
  log "checking stack safety (this may take some time)"
  let depth = 1000000
  assertThrows'
    "function composition is not stack safe"
    (\_ -> composeGo (_ + 1) identity depth $ 0)
  assert'
    "composition of -#> is stack safe"
    (depth == (composeGo (Func (_ + 1)) identity depth #$ 0))
  where
    composeGo :: forall a cat. Category cat => (cat a a) -> (cat a a) -> Int -> cat a a
    composeGo f acc n = if n == 0
                        then acc
                        else composeGo f (compose acc f) (n - 1)

laws :: Effect Unit
laws =
  checkLaws "Func" do
    Control.checkSemigroupoid prx3Func
    Control.checkCategory prx3Func
  where
  prx3Func = Proxy3 :: Proxy3 TestFunc

newtype TestFunc a b = TestFunc (Func a b)

derive newtype instance semigroupoidTestFunc :: Semigroupoid TestFunc
derive newtype instance categoryTestFunc :: Category TestFunc

instance eqTestFunc :: (BoundedEnum i, Eq o) => Eq (TestFunc i o) where
  eq (TestFunc (Func f)) (TestFunc (Func g)) = all (\v -> f v == g v) $ asArr $ enumFromTo bottom top
    where
      asArr :: Array ~> Array
      asArr = identity

instance coarbitraryTestFunc :: (Arbitrary a, Coarbitrary b) => Coarbitrary (TestFunc a b) where
  coarbitrary (TestFunc (Func f)) = coarbitrary f

instance arbitraryTestFunc :: (Coarbitrary a, Arbitrary b) => Arbitrary (TestFunc a b) where
  arbitrary = arbitrary <#> Func >>> TestFunc