# purescript-stacksafe-function

[![Latest release](http://img.shields.io/github/release/safareli/purescript-stacksafe-function.svg)](https://github.com/safareli/purescript-stacksafe-function/releases)
[![Build status](https://travis-ci.org/safareli/purescript-stacksafe-function.svg?branch=master)](https://travis-ci.org/safareli/purescript-stacksafe-function)

A newtype over normal function (->), which guarantees stack safety. It's safe to be used in FFI code, as representation of underlying function is not changed.

## Installation

```
bower install purescript-stacksafe-function
```

Some context about the work:

- [medium.com/@safareli Stack safe Function compositio](https://medium.com/@safareli/stack-safe-function-composition-85d61feee37e)
- [purescript/purescript-prelude#150](https://github.com/purescript/purescript-prelude/pull/150)
- [purescript/purescript-eff#31](https://github.com/purescript/purescript-eff/pull/31)

## Documentation

Module documentation is published on Pursuit: [http://pursuit.purescript.org/packages/purescript-stacksafe-function](http://pursuit.purescript.org/packages/purescript-stacksafe-function)
