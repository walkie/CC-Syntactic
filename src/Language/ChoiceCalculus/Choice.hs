
module Language.ChoiceCalculus.Choice where

import Language.Syntactic hiding (Nil)

type Dim = String

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
