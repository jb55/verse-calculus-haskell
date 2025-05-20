-- ──────────────────────────────────────────────────────────────
--  VC/SmallStep.hs – small‑step rewrite semantics
-- ──────────────────────────────────────────────────────────────
{-# LANGUAGE LambdaCase #-}

module VC.SmallStep
  ( rewriteOnce, normalForm, applyExpr ) where

import           VC.Syntax
import qualified Data.Map.Strict as M

---------------------------------------------------------------------
--  Substitution (identical to original Rewriter)
---------------------------------------------------------------------

applyScalar :: Subst -> Scalar -> Scalar
applyScalar s (VVar x) = case M.lookup x s of
  Just (S s') -> applyScalar s s'
  _           -> VVar x
applyScalar _ s        = s

applyHeap :: Subst -> Heap -> Heap
applyHeap s (HTuple ss) = HTuple (map (applyScalar s) ss)
applyHeap s (HLam x e)  = HLam x (applyExpr s e)

applyValue :: Subst -> Value -> Value
applyValue s (S sc) = S (applyScalar s sc)
applyValue s (H h)  = H (applyHeap s h)

applyExpr :: Subst -> Expr -> Expr
applyExpr s = \case
  Val v       -> Val (applyValue s v)
  Seq e1 e2   -> Seq (applyExpr s e1) (applyExpr s e2)
  Exists x e  -> Exists x (applyExpr s e)
  Fail        -> Fail
  App f a     -> App (applyExpr s f) (applyExpr s a)
  VApp v1 v2  -> VApp (applyValue s v1) (applyValue s v2)
  Choice l r  -> Choice (applyExpr s l) (applyExpr s r)
  One e       -> One (applyExpr s e)
  All e       -> All (applyExpr s e)
  Eq v e      -> Eq (applyValue s v) (applyExpr s e)

---------------------------------------------------------------------
--  Small‑step rewrite rules
---------------------------------------------------------------------

rewriteOnce :: Expr -> Maybe Expr
rewriteOnce = \case
    -- (a) Propagate failure
  Seq Fail _e2
    -> Just Fail
  
  Seq (Val _) e2 -> Just e2
  Seq e1 e2 -> rewriteOnce e1 >>= \e1' -> Just (Seq e1' e2)

  --Eq (S (VVar x)) (Val v) -> Just (Val (S (VVar x)))
  -- Equality: try to make progress on the RHS first
  Eq v rhs -> case rhs of
    Val v' | v == v' -> Just (Val v)        -- identical scalars, done
    Fail             -> Just Fail           -- RHS already failed
    _                -> rewriteOnce rhs >>= \rhs' -> Just (Eq v rhs') -- otherwise, step inside RHS

  App (Val (H (HLam x body))) (Val arg) ->
    Just (Exists x (Seq (Eq (S (VVar x)) (Val arg)) body))

  VApp (H (HTuple scs)) (S (VInt i))
    | i >= 0, i < length scs -> Just (Val (S (scs !! i)))
    | otherwise              -> Just Fail

  VApp (S (VPrim Add)) (H (HTuple [VInt a, VInt b])) ->
    Just (Val (S (VInt (a + b))))

  VApp (S (VPrim Gt)) (H (HTuple [VInt k1, VInt k2])) ->
    Just (if k1 > k2 then Val (S (VInt k1)) else Fail)

  VApp (H (HTuple scs)) (S (VVar idx)) ->
    let alts = zipWith (mkAlt idx) [0..] scs in Just (foldr1 Choice alts)

  One e -> case rewriteOnce e of
    Just e' -> Just (One e')
    Nothing -> Just e

  _ -> Nothing
  where
    mkAlt :: Name -> Int -> Scalar -> Expr
    mkAlt idx n s =
      let test = Eq (S (VVar idx)) (Val (S (VInt n)))
          val  = Val (S s)
       in Seq test val

---------------------------------------------------------------------
--  Drive the small‑step machine to a normal form
---------------------------------------------------------------------

normalForm :: Expr -> Expr
normalForm e = maybe e normalForm (rewriteOnce e)
