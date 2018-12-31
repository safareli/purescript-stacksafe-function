module StackSafe.Function where

import Control.Semigroupoid (class Semigroupoid)
import Control.Category (class Category, identity)

-- | NOTE: Stack safety applies to composition only! It does not make a
-- | function, that is not stacksafe, safe.
foreign import data Func :: Type -> Type -> Type
infixr 4 type Func as -#>

foreign import fromFunction :: forall a b. (a -> b) -> (a -#> b)
foreign import composeFunc :: forall a b c. (b -#> c) -> (a -#> b) -> (a -#> c)
foreign import toFunction :: forall a b. (a -#> b) -> (a -> b)
foreign import identityImpl :: forall a. (a -#> a)

infixr 0 toFunction as #$

instance semigroupoidFn :: Semigroupoid Func where
  compose = composeFunc

instance categoryFn :: Category Func where
  identity = identityImpl
