-- ──────────────────────────────────────────────────────────────
--  VC/Syntax.hs  – shared abstract syntax & utilities
-- ──────────────────────────────────────────────────────────────
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module VC.Syntax
  ( Name, Prim(..), Scalar(..), Heap(..), Value(..), Expr(..)
  , HeapEnv, Subst
  -- * Smart‑constructors
  , varS, intS, primS, varV, intV, primV, tupleV, lamV
  , valE, existsE, seqE, (.=.)
  -- * Pretty printing
  , prettyExpr, prettyValue, prettyEnv, prettyResult, runPretty
  ) where

import qualified Data.Map.Strict as M
import           Data.List        (intercalate)
import           Control.Applicative (Alternative(..))

---------------------------------------------------------------------
--  Core syntax (shared by every semantics)
---------------------------------------------------------------------

type Name = String

data Prim = Add | Gt deriving (Eq, Show)

data Scalar
  = VVar Name
  | VInt Int
  | VPrim Prim
  deriving (Eq, Show)

data Heap
  = HTuple [Scalar]
  | HLam Name Expr
  deriving (Eq, Show)

data Value
  = S Scalar
  | H Heap
  deriving (Eq, Show)

data Expr
  = Val Value
  | Seq Expr Expr
  | Exists Name Expr
  | Fail
  | App Expr Expr
  | VApp Value Value
  | Choice Expr Expr
  | One Expr
  | All Expr
  | Eq Value Expr
  deriving (Eq, Show)

---------------------------------------------------------------------
--  Environments & substitutions (used by both semantics)
---------------------------------------------------------------------

type HeapEnv = M.Map Name Value

type Subst   = M.Map Name Value

---------------------------------------------------------------------
--  Smart‑constructors & tiny DSL   (unchanged)
---------------------------------------------------------------------
-- Scalars --------------------------------------------------------
varS  :: Name -> Scalar
varS  = VVar

intS  :: Int  -> Scalar
intS  = VInt

primS :: Prim -> Scalar
primS = VPrim

-- Values ---------------------------------------------------------
varV  :: Name -> Value        -- logical variable as a Value
varV  = S . varS

intV  :: Int  -> Value
intV  = S . intS

primV :: Prim -> Value
primV = S . primS

tupleV :: [Scalar] -> Value   -- ⟨ … ⟩ as a Value
tupleV = H . HTuple

lamV :: Name -> Expr -> Value -- λx.e as a Value
lamV  = (H .) . HLam

-- Expressions ----------------------------------------------------
valE :: Value -> Expr         -- just lift to Expr
valE = Val

existsE :: Name -> Expr -> Expr
existsE = Exists

seqE :: Expr -> Expr -> Expr  -- left‑to‑right sequencing
seqE = Seq

(.=.) :: Value -> Value -> Expr  -- inline equality test
v .=. w = Eq v (valE w)

infix  1 .=.
infixr 0 `seqE`

---------------------------------------------------------------------
--  Pretty printing (moved verbatim from the big‑step file)
---------------------------------------------------------------------

prettyScalar :: Scalar -> String
prettyScalar = \case
  VVar n    -> n
  VInt k    -> show k
  VPrim Add -> "add"
  VPrim Gt  -> "gt"

prettyHeap :: Heap -> String
prettyHeap = \case
  HTuple ss -> "⟨" ++ intercalate ", " (map prettyScalar ss) ++ "⟩"
  HLam x _  -> "λ" ++ x ++ ". …"

prettyValue :: Value -> String
prettyValue = \case
  S s -> prettyScalar s
  H h -> prettyHeap h

prettyEnv :: HeapEnv -> String
prettyEnv h =
  "{ " ++ intercalate ", "
          [ n ++ " = " ++ prettyValue v
          | (n,v) <- M.toList h ]
      ++ " }"

prettyResult :: (HeapEnv, Value) -> String
prettyResult (h, v) =
  prettyEnv h ++ "\n↳ " ++ prettyValue v

-- | Collect all results, fully resolving logic variables on the fly.
runPretty :: (HeapEnv -> [(HeapEnv, Value)]) -> [String]
runPretty k =
  [ prettyResult (h, resolve h v)
  | (h, v) <- k M.empty ]

-- Utilities used by runPretty -----------------------------------
resolve :: HeapEnv -> Value -> Value
resolve h (S (VVar n)) = maybe (S (VVar n)) (resolve h) (M.lookup n h)
resolve _ v            = v

---------------------------------------------------------------------
--  Pretty‑printing Expr without evaluating it (for debugging)
---------------------------------------------------------------------
prettyExpr :: Expr -> String
prettyExpr = go 0
  where
    go :: Int -> Expr -> String
    go _ (Val v)        = prettyValue v
    go _ Fail           = "fail"

    -- ∃x. e  (binds tightest of the big constructs)
    go p (Exists x e)   = paren (p > 0) $ "∃" <> x <> ". " <> go 0 e

    -- p 0  – Choice  (lowest-binding)
    go p (Choice l r)   = bin p 0 "|"  l r

    -- p 1  – Sequence
    go p (Seq   l r)    = bin p 1 ";"  l r

    -- p 2  – Equality
    go p (Eq v e)       = paren (p > 2) $
                          prettyValue v <> " = " <> go 3 e

    -- p 3  – Application
    go p (App f a)      = paren (p > 3) $
                          go 3 f <> " " <> go 4 a
    go p (VApp v1 v2)   = paren (p > 3) $
                          prettyValue v1 <> " ⋅ " <> prettyValue v2

    -- p 4  – One/All wrap an expression region
    go _ (One e)        = "one " <> paren (needsParen e) (go 4 e)
    go _ (All e)        = "all " <> paren (needsParen e) (go 4 e)

    ----------------------------------------------------------------
    -- helpers
    bin p prec op l r   =
      paren (p > prec) $ go prec l <> " " <> op <> " " <> go prec r

    needsParen Val{}    = False
    needsParen Fail     = False
    needsParen _        = True

    paren True  s = "(" <> s <> ")"
    paren False s = s

