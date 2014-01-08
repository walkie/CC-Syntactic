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


-- * Atomic values


-- | An atomic plain value.
data One a t where
  One :: a -> One a (Full a)

-- | Construct a One AST node.
one :: (One a :<: l) => a -> ASTF l a
one = inj . One

instance Show a => Render (One a) where
  render (One a) = show a


-- | An empty/unit value.
data None t where
  None :: None (Full a)

-- | Construct a None AST node.
none :: (None :<: l) => ASTF l a
none = inj None

-- | Construct an AST node from a Maybe value.
fromMaybe :: (One a :<: l, None :<: l) => Maybe a -> ASTF l a
fromMaybe (Just a) = one a
fromMaybe Nothing  = none

instance Render None where
  render None = "●"


-- * Basic recursive data types

-- | List.
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
  renderArgs [h,t] Cons = h ++ ":" ++ t
  renderArgs []    Nil  = "[]"


-- | A generic rose tree.
data Tree a t where
  Node :: Tree a (a :-> List (Tree a t) t :-> Full (Tree a t))

-- | Construct a Node AST node.
node :: (Tree a :<: l) => ASTF l a -> ASTF l (List (Tree a t) t) -> ASTF l (Tree a t)
node = appSym Node

instance Render (Tree a) where
  renderArgs [a,c] Node = "(Node " ++ a ++ " " ++ c ++ ")"
