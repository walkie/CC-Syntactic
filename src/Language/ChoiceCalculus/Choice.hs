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
type Tag = Bool

-- | Generic binary choices.
data Chc2 d t where
  Chc2 :: d -> Chc2 d (a :-> a :-> Full a)

-- | Construct a binary choice AST node.
chc2 :: (Chc2 d :<: l) => d -> ASTF l a -> ASTF l a -> ASTF l a
chc2 d a b = inj (Chc2 d) :$ a :$ b

instance Show d => Render (Chc2 d) where
  renderArgs [l,r] (Chc2 d) = show d ++ "‹" ++ l ++ "," ++ r ++ "›"

-- | Specialized project function that fixes a type for `d`.
prjChc2 :: Project (Chc2 d) l => d -> l t -> Maybe (Chc2 d t)
prjChc2 _ = prj 

-- | Apply a selection to an AST.
select :: (Eq d, Chc2 d :<: l) => d -> Tag -> AST l a -> AST l a
select d t (s :$ l :$ r)
  | Just (Chc2 d') <- prjChc2 d s
  , d == d'         = if t then l else r
select d t (s :$ a) = select d t s :$ select d t a
select d t (Sym s)  = Sym s

-- | The set of free dimensions in the AST.
freeDims :: (Chc2 Dim :<: l) => AST l a -> Set Dim
freeDims (s :$ a) = Set.union (freeDims s) (freeDims a)
freeDims (Sym s)
  | Just (Chc2 d) <- prjChc2 "" s = Set.singleton d
  | otherwise                     = Set.empty

type V a = (Chc2 Dim :<: l, One Int :<: l) => ASTF l Int

vint :: V a
vint = chc2 "A" (chc2 "B" (one 1) (one 2)) (chc2 "B" (one 3) (one 4))

vtree :: ASTF (Tree Int :+: List (Tree Int t) :+: Chc2 Dim :+: One Int) (Tree Int t)
vtree = node (one 0) (cons (node vint nil) nil)
