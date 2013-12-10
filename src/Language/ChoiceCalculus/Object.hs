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
data One t where
  One :: a -> One (Full a)

-- Construct a One AST node.
one :: (One :<: l) => a -> ASTF l a
one = inj . One


-- | An empty/unit value.
data None t where
  None :: None (Full a)

-- | Construct a None AST node.
none :: (None :<: l) => ASTF l a
none = inj None

-- | Construct an AST node from a Maybe value.
fromMaybe :: (One :<: l, None :<: l) => Maybe a -> ASTF l a
fromMaybe (Just a) = one a
fromMaybe Nothing  = none


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


-- | A generic rose tree.
data Tree a t where
  Node :: Tree a (a :-> List (Tree a t) t :-> Full (Tree a t))

-- | Construct a Node AST node.
node :: (Tree a :<: l) => ASTF l a -> ASTF l (List (Tree a t) t) -> ASTF l (Tree a t)
node = appSym Node
