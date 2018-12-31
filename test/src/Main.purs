module Test.Main where

import Prelude
import StackSafe.Function
import Test.Assert
import Effect.Console (log)


main = do
  checkIdentity
  checkAssociativity
  checkEquivalenceToFunction
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
addA' = fromFunction addA

addB' :: String -#> String
addB' = fromFunction addB

addC' :: String -#> String
addC' = fromFunction addC

addD' :: String -#> String
addD' = fromFunction addD

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

checkAssociativity = do
  assertEqual { expected: (addA' <<< addB') <<< addC' #$ ""
              , actual:   addA' <<< (addB' <<< addC') #$ "" }

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

checkStackSafety = do
  log "checking stack safety (this may take some time)"
  let depth = 1000000
  assertThrows'
    "function composition is not stack safe"
    (\_ -> composeGo (_ + 1) identity depth $ 0)
  assert'
    "composition of -#> is stack safe"
    (depth == (composeGo (fromFunction (_ + 1)) identity depth #$ 0))
  where
    composeGo :: forall a cat. Category cat => (cat a a) -> (cat a a) -> Int -> cat a a
    composeGo f acc n = if n == 0
                        then acc
                        else composeGo f (compose acc f) (n - 1)
