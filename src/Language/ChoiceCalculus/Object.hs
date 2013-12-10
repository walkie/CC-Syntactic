{-# LANGUAGE
      FlexibleContexts,
      GADTs,
      PatternGuards,
      RankNTypes,
      TypeOperators
  #-}

-- | Generic object language encodings.
module Language.ChoiceCalculus.Object where

import Language.Syntactic hiding (Nil)


-- | A single plain value.
data One t where
  One :: a -> One (Full a)

one :: (One :<: l) => a -> ASTF l a
one = inj . One


-- | No value.
data None t where
  None :: None (Full a)

none :: (None :<: l) => ASTF l a
none = inj None


-- | List.
data List a t where
  Cons :: List a (a :-> List a t :-> Full (List a t))
  Nil  :: List a (Full (List a t))

cons :: (List a :<: l) => ASTF l a -> ASTF l (List a t) -> ASTF l (List a t)
cons h t = inj Cons :$ h :$ t

nil :: (List a :<: l) => ASTF l (List a t)
nil = inj Nil


-- | A generic rose tree representation.
data Tree a t where
  Node :: Tree a (a :-> List (Tree a t) t :-> Full (Tree a t))

node :: (Tree a :<: l) => ASTF l a -> ASTF l (List (Tree a t) t) -> ASTF l (Tree a t)
node a l = inj Node :$ a :$ l
