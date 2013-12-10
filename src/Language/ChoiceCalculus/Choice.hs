{-# LANGUAGE
      FlexibleContexts,
      GADTs,
      PatternGuards,
      RankNTypes,
      TypeOperators
  #-}

-- | Various choice representations.
module Language.ChoiceCalculus.Choice where

import Data.Set (Set)
import qualified Data.Set as Set
import Language.Syntactic hiding (Nil)

import Language.ChoiceCalculus.Object

-- | Dimension names.
type Dim = String

-- | Generic binary choices.
data Chc2 t where
  Chc2 :: Dim -> Chc2 (a :-> a :-> Full a)

-- | Construct a binary choice AST node.
chc2 :: (Chc2 :<: l) => Dim -> ASTF l a -> ASTF l a -> ASTF l a
chc2 d a b = inj (Chc2 d) :$ a :$ b

freeDims :: (Chc2 :<: l) => AST l a -> Set Dim
freeDims (s :$ a) = Set.union (freeDims s) (freeDims a)
freeDims (Sym s)
  | Just (Chc2 d) <- prj s = Set.singleton d
  | otherwise              = Set.empty

type V a = ASTF (Chc2 :+: One) a

vint :: V Int
vint = chc2 "A" (chc2 "B" (one 1) (one 2)) (chc2 "B" (one 3) (one 4))

-- vtree :: ASTF (Tree Int :+: Chc2 :+: One) (Tree Int t)
-- vtree = node (one 0) (cons (node vint nil) nil)
