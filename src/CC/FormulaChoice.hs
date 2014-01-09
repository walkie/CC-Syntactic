
module CC.FormulaChoice where

import Language.Syntactic

import CC.BinaryChoice

-- * Formula tags

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


-- * Formula choices

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
