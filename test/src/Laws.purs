module Test.Laws where

import Prelude

import StackSafe.Function (Func(..))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)
import Test.StrongCheck.Laws (SC, B(..), checkLaws)
import Test.StrongCheck.Laws.Control as Control
import Type.Proxy (Proxy3(..))
-- import Debug.Trace (spy)

laws :: ∀ eff. SC eff Unit
laws = pure unit

laws' :: ∀ eff. SC eff Unit
laws' = 
  checkLaws "Func" do
  Control.checkSemigroupoid prx3Func
  Control.checkCategory prx3Func
  where
  prx3Func = Proxy3 ∷ Proxy3 TestFunc


newtype TestFunc a b = TestFunc (Func a b)

instance semigroupoidTestFunc :: Semigroupoid (TestFunc) where
  compose (TestFunc f) (TestFunc g) = TestFunc (compose f g)

instance categoryTestFunc :: Category (TestFunc) where
  id = TestFunc id

instance eqTestFuncB :: Eq e => Eq (TestFunc B e) where
  eq (TestFunc (Func f)) (TestFunc (Func g)) 
    = e_ && l_ && g_
    where
      -- in console this values must be all true
      -- z = spy {e_, l_, g_}
      e_ = f (B EQ) == g (B EQ)
      l_ = f (B LT) == g (B LT)
      g_ = f (B GT) == g (B GT)

instance safeFuncCoarbitrary :: (Arbitrary a, Coarbitrary b) => Coarbitrary (TestFunc a b) where
  coarbitrary (TestFunc (Func f)) = coarbitrary f

instance safeFuncArbitrary :: (Coarbitrary a, Arbitrary b) => Arbitrary (TestFunc a b) where
  arbitrary = arbitrary <#> Func >>> TestFunc
