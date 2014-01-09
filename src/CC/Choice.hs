{-# LANGUAGE
      FlexibleContexts,
      GADTs,
      PatternGuards,
      RankNTypes,
      TypeOperators
  #-}

-- | Various representations of localized variation points, i.e. choices.
module CC.Choice where

import Data.Set (Set)
import qualified Data.Set as Set
import Language.Syntactic hiding (Nil)

import CC.Atomic
import CC.Data


-- * Generic binary choices

-- | Generic binary choices. A choice has an associated /tag/ @t@ that
--   is used to determine how the choice is resolved during configuration.
data Chc2 t a where
  Chc2 :: t -> Chc2 t (a :-> a :-> Full a)

-- | Construct a binary choice AST node.
chc2 :: (Chc2 t :<: l) => t -> ASTF l a -> ASTF l a -> ASTF l a
chc2 = appSym . Chc2

-- | Specialized project function that fixes a particular tag type.
prjChc2 :: Project (Chc2 t) l => t -> l a -> Maybe (Chc2 t a)
prjChc2 _ = prj 

instance Show t => Render (Chc2 t) where
  renderSym _ = "Chc2"
  renderArgs [l,r] (Chc2 t) = show t ++ "‹" ++ l ++ "," ++ r ++ "›"

-- | A configuration maps generic tags to a boolean value indicating whether
--   to choose the left (@True@) or right (@False@) alternative.
type Config t = t -> Bool

-- | Apply a configuration to an expression, eliminating all choices.
configure :: (Chc2 t :<: l) => Config t -> AST l a -> AST l a
configure c (s :$ l :$ r)
    | Just (Chc2 t) <- prjChc2 (dom c) s = if c t then l else r
  where dom :: Config t -> t
        dom = undefined
configure c (s :$ a) = configure c s :$ configure c a
configure _ (Sym s)  = Sym s


-- ** Atomic tags

-- | An atomic tag, which may be either included or not in a configuration.
type Tag = String

-- | Produce a configuration from a list of selected tags.
configT :: [Tag] -> Config Tag
configT = flip elem

-- | A tagged binary choice. Resolves to its left alternative if its tag is
--   selected, otherwise its right alternative.
type ChcT = Chc2 Tag

-- | Construct a tagged binary choice AST node.
chcT :: (ChcT :<: l) => Tag -> ASTF l a -> ASTF l a -> ASTF l a
chcT = chc2

-- | Specialized project function for tagged binary choices.
prjChcT :: Project ChcT l => l a -> Maybe (ChcT a)
prjChcT = prj

-- | The set of atomic tags referred to in the AST.
tags :: (ChcT :<: l) => AST l a -> Set Tag
tags (s :$ a) = Set.union (tags s) (tags a)
tags (Sym s)
  | Just (Chc2 d) <- prjChcT s = Set.singleton d
  | otherwise                  = Set.empty

-- | Select or deselect a particular atomic tag.
chooseT :: (ChcT :<: l) => Bool -> Tag -> AST l a -> AST l a
chooseT b t (s :$ l :$ r)
  | Just (Chc2 t') <- prjChcT s
  , t == t'          = if b then l else r
chooseT b t (s :$ a) = chooseT b t s :$ chooseT b t a
chooseT _ _ (Sym s)  = Sym s

-- | Select an atomic tag by replacing all its corresponding choices by
--   their left alternatives.
selectT :: (ChcT :<: l) => Tag -> AST l a -> AST l a
selectT   = chooseT True

-- | Deselect an atomic tag by replacing all its corresponding choices by
--   their right alternatives.
deselectT :: (ChcT :<: l) => Tag -> AST l a -> AST l a
deselectT = chooseT False

-- | Configure an expression with tagged binary choices by providing a list
--   of selected tags. Omitted tags are implicitly deselected.
configureT :: (ChcT :<: l) => [Tag] -> AST l a -> AST l a
configureT = configure . configT


-- ** Formula tags

-- | Boolean tag formulas.
data Formula = FTag Tag
             | FNot Formula
             | FAnd Formula Formula
             | FOr  Formula Formula
  deriving Eq

-- | Determine whether a formula is satisfied by a particular tag configuration.
satisfied :: Config Tag -> Config Formula
satisfied c (FTag t) = c t
satisfied c (FNot f) = not (satisfied c f)
satisfied c (FAnd l r) = satisfied c l && satisfied c r
satisfied c (FOr  l r) = satisfied c l || satisfied c r

-- | Produce a formula configuration from a list of selected tags.
configF :: [Tag] -> Config Formula
configF = satisfied . configT

instance Show Formula where
  show f = case f of
      FAnd f g -> paren f ++ "∧" ++ paren g
      FOr  f g -> paren f ++ "∨" ++ paren g
      f        -> paren f
    where
      paren (FTag t) = t
      paren (FNot f) = "¬" ++ paren f
      paren f        = "(" ++ show f ++ ")"

-- | A formula choice is associated with a boolean tag formula.
--   Resolves to its left alternative if its formula is satisfied by the
--   configuration, otherwise its right alternative.
type ChcF = Chc2 Formula

-- | Construct a formula choice AST node.
chcF :: (ChcF :<: l) => Formula -> ASTF l a -> ASTF l a -> ASTF l a
chcF = chc2

-- | Specialized project function for formula choices.
prjChcF :: Project ChcF l => l a -> Maybe (ChcF a)
prjChcF = prj

-- | Configure an expression with formula choices by providing a list
--   of selected tags. Omitted tags are implicitly deselected.
configureF :: (ChcF :<: l) => [Tag] -> AST l a -> AST l a
configureF = configure . configF


-- * Examples

type V a = (ChcT :<: l, One Int :<: l) => ASTF l Int

vint :: V a
vint = chc2 "A" (chc2 "B" (one 1) (one 2)) (chc2 "B" (one 3) (one 4))

vtree :: ASTF (Tree Int :+: List (Tree Int t) :+: ChcT :+: One Int) (Tree Int t)
vtree = node (one 0) (cons (node vint nil) nil)
