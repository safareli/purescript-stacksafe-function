module StackSafe.Function where

import Data.Newtype (class Newtype)
import Control.Semigroupoid (class Semigroupoid)
import Control.Category (class Category, id)

-- | A newtype over normal function (->) , which guarantes stack safety.
-- | It's safe to be used in FFI code, as representation of of underlying
-- | function is not changed.
newtype Func a b = Func (a -> b)

derive instance newtypeFunc :: Newtype (Func a b) _

run :: forall a b. Func a b -> a -> b
run (Func f) = f

instance semigroupoidFn :: Semigroupoid (Func) where
  compose (Func f) (Func g) = Func (functionCompose f g)

instance categoryFn :: Category (Func) where
  id = Func id

foreign import functionCompose :: forall b c d. (c -> d) -> (b -> c) -> (b -> d)

