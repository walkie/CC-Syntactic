{-# LANGUAGE
      FlexibleContexts,
      GADTs,
      TypeOperators
  #-}

-- | Syntactic constructs for basic atomic values.
module CC.Atomic where

import Language.Syntactic


-- | An atomic plain value of a particular type. Note that this differs from
--   Syntactic's built-in Literal construct since One distinguishes between
--   different types of literals in the symbol domain.
data One a t where
  One :: a -> One a (Full a)

-- | An atomic empty/unit value. Isomorphic to @One ()@.
data None t where
  None :: None (Full a)


-- | Construct a One AST node.
one :: (One a :<: l) => a -> ASTF l a
one = inj . One

-- | Construct a None AST node.
none :: (None :<: l) => ASTF l a
none = inj None

-- | Construct an AST node from a Maybe value.
fromMaybe :: (One a :<: l, None :<: l) => Maybe a -> ASTF l a
fromMaybe (Just a) = one a
fromMaybe Nothing  = none


instance Show a => Render (One a) where
  renderSym (One a) = show a

instance Render None where
  renderSym None = "●"
