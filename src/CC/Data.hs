{-# LANGUAGE
      FlexibleContexts,
      GADTs,
      PatternGuards,
      RankNTypes,
      TypeOperators
  #-}

-- | Syntactic constructs for some basic recursive data types.
module CC.Data where

import Language.Syntactic hiding (Nil)


-- * Lists

-- | Abstract syntax of lists.
data List a t where
  Cons :: List a (a :-> List a t :-> Full (List a t))
  Nil  :: List a (Full (List a t))

-- | Construct a Cons AST node.
cons :: (List a :<: l) => ASTF l a -> ASTF l (List a t) -> ASTF l (List a t)
cons = appSym Cons

-- | Construct a Nil AST node.
nil :: (List a :<: l) => ASTF l (List a t)
nil = inj Nil

-- | Build a List AST from a Haskell list of ASTs.
fromList :: (List a :<: l) => [ASTF l a] -> ASTF l (List a t)
fromList = foldr cons nil

instance Render (List a) where
  renderSym Cons = "(:)"
  renderSym Nil  = "[]"
  renderArgs [h,t] Cons = h ++ ":" ++ t
  renderArgs []    Nil  = "[]"


-- * Trees

-- | Abstract syntax of generic rose trees.
data Tree a t where
  Node :: Tree a (a :-> List (Tree a t) t :-> Full (Tree a t))

-- | Construct a Node AST node.
node :: (Tree a :<: l) => ASTF l a -> ASTF l (List (Tree a t) t) -> ASTF l (Tree a t)
node = appSym Node

-- | Construct a Node AST node from a value and a list of subexpressions.
-- node' :: (Tree a :<: l, List (Tree a t) :<: l) => ASTF l a -> [ASTF l (Tree a t)] -> ASTF l (Tree a t)
-- node' a l = appSym Node a (fromList l)

instance Render (Tree a) where
  renderSym _ = "Node"
