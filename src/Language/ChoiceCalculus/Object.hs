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
data One l where
  One :: a -> One (Full a)

one :: (One :<: l) => a -> ASTF l a
one = inj . One


-- | No value.
data None l where
  None :: None (Full a)

none :: (None :<: l) => ASTF l a
none = inj None


-- | List.
data List a l where
  Cons :: a -> List a (List a :-> Full (List a))
  Nil  :: List (Full (List a))

cons :: (List :<: l) => e -> ASTF l (List a) -> ASTF l (List a)
cons e t = inj (Cons e) :$ t


-- | A generic rose tree representation.
--data Tree2 a where
  --Node2 :: b -> Tree2 ([a] :-> a :-> Full a)
