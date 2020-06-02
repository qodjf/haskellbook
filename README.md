# haskellbook
exercises of https://haskellbook.com/

* Semigroup: 半群，结合律，封闭
* Monoid: Semigroup + 幺元
* Functor: A functor is a way to apply a function over or around some structure that we don’t want to alter.
  F a -> (a -> b) -> F b
* Applicative: F a -> F (a -> b) -> F b
* Monad: M a -> (a -> M b) -> M b

Each argument (and result) in the type signature for a function must be a fully applied (and inhabitable, modulo Void, etc.) type. Each argument must have the kind *.
