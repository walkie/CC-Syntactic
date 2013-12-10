{-# LANGUAGE
      FlexibleContexts,
      GADTs,
      PatternGuards,
      RankNTypes,
      TypeOperators
  #-}

module Language.ChoiceCalculus.Core.Syntax where

import Language.Syntactic hiding (Nil)

type Dim = String

-- * Generic object language encodings.

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


-- | Binary choices
data Chc2 l where
  Chc2 :: Dim -> Chc2 (a :-> a :-> Full a)

chc2 :: (Chc2 :<: l) => Dim -> ASTF l a -> ASTF l a -> ASTF l a
chc2 d a b = inj (Chc2 d) :$ a :$ b

freeDims :: (Chc2 :<: l) => AST l a -> [Dim]
freeDims (s :$ a) = freeDims s ++ freeDims a
freeDims (Sym s)
  | Just (Chc2 d) <- prj s = [d]
  | otherwise              = []

type V a = ASTF (Chc2 :+: One) a

vint :: V Int
vint = chc2 "A" (chc2 "B" (one 1) (one 2)) (chc2 "B" (one 3) (one 4))
