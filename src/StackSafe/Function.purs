module StackSafe.Function 
  ( Func(..)
  , run
  , type (-#>)
  , (#$)
  )where

import Data.Newtype (class Newtype)
import Control.Semigroupoid (class Semigroupoid)
import Control.Category (class Category, identity)

-- | A newtype over normal function (->), which guarantees stack safety.
-- | It's safe for use in FFI code, as representation of underlying
-- | function is not changed.
-- |
-- | NOTE: Stack safety applies to composition only! It does not make a
-- | function, that is not stack-safe, safe.
newtype Func a b = Func (a -> b)

infixr 4 type Func as -#>

derive instance newtypeFunc :: Newtype (Func a b) _

run :: forall a b. Func a b -> a -> b
run (Func f) = f

infixr 0 run as #$

instance semigroupoidFn :: Semigroupoid Func where
  compose (Func f) (Func g) = Func (functionCompose f g)

instance categoryFn :: Category Func where
  identity = Func identity

foreign import functionCompose :: forall b c d. (c -> d) -> (b -> c) -> (b -> d)
