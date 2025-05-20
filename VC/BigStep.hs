
{-# LANGUAGE LambdaCase #-}

-- ──────────────────────────────────────────────────────────────
--  VC/BigStep.hs – big‑step (evaluation‑monad) semantics
-- ──────────────────────────────────────────────────────────────

module VC.BigStep
  ( Eval, runEval, eval, unify, bind, prettyEval ) where

import           VC.Syntax
import qualified Data.Map.Strict as M
import           Control.Applicative
import           Control.Monad       (MonadPlus(..))

---------------------------------------------------------------------
--  Evaluation monad (unchanged)
---------------------------------------------------------------------

newtype Eval a = E { runE :: HeapEnv -> [(HeapEnv, a)] }
  deriving Functor

runEval :: Eval a -> [(HeapEnv, a)]
runEval m = runE m M.empty

instance MonadPlus Eval where
  mzero = empty
  mplus = (<|>)

instance Applicative Eval where
  pure x = E $ \h -> [(h, x)]
  E mf <*> E mx = E $ \h -> [ (h2, f x) | (h1, f) <- mf h, (h2, x) <- mx h1 ]

instance Monad Eval where
  return = pure
  E m >>= k = E $ \h -> concat [ runE (k a) h' | (h', a) <- m h ]

instance Alternative Eval where
  empty = E $ const []
  E m <|> E n = E $ \h -> m h ++ n h

-- Helpers --------------------------------------------------------
liftMaybe :: Maybe a -> Eval a
liftMaybe = maybe mzero return

---------------------------------------------------------------------
--  Unification & variable binding (unchanged)
---------------------------------------------------------------------

unify :: Value -> Value -> Eval ()
unify (S (VVar x)) v = bind x v
unify v (S (VVar x)) = bind x v
unify (S (VPrim p1)) (S (VPrim p2)) | p1 == p2  = return ()
unify (S (VInt k1))  (S (VInt k2))  | k1 == k2  = return ()
unify (H (HTuple xs)) (H (HTuple ys)) | length xs == length ys = mapM_ (uncurry unify) (zip (map S xs) (map S ys))
unify _ _ = mzero

bind :: Name -> Value -> Eval ()
bind x v = E $ \h -> case M.lookup x h of
  Nothing -> [(M.insert x v h, ())]
  Just v' -> runE (unify v' v) h

---------------------------------------------------------------------
--  Evaluation rules (identical to original VC module)
---------------------------------------------------------------------

eval :: Expr -> Eval Value
eval = \case
  Val v       -> return v
  Fail        -> mzero
  Seq e1 e2   -> eval e1 >> eval e2
  Exists _ e  -> eval e
  Eq v e2     -> eval e2 >>= \v2 -> unify v v2 >> return v
  Choice e1 e2 -> eval e1 <|> eval e2
  One e       -> fmap head (collect e)
  All e       -> fmap (H . HTuple . map extractScalar) (collect e)
  App f a     -> eval f >>= \vf -> eval a >>= \va -> eval (VApp vf va)
  VApp (H (HLam x body)) v -> eval (Exists x (Seq (Eq (S (VVar x)) (Val v)) body))
  VApp (H (HTuple scs)) (S (VInt i))
    | i >= 0, i < length scs -> return (S (scs !! i))
    | otherwise              -> mzero
  VApp (S (VPrim Add)) (H (HTuple [VInt a, VInt b])) -> return (S (VInt (a + b)))
  VApp (S (VPrim Gt)) (H (HTuple [VInt k1, VInt k2]))
    | k1 > k2   -> return (S (VInt k1))
    | otherwise -> mzero
  _ -> error "stuck term"
  where
    collect e = E $ \h ->
      [ (h2, v:vs) | (h1, v) <- runE (eval e) h, (h2, vs) <- runE (collect e) h1 ]
      ++ [(h, [])]

-- Safely extract Scalar from Value --------------------------------
extractScalar :: Value -> Scalar
extractScalar (S s) = s
extractScalar _     = error "Expected scalar value"

prettyEval :: Expr -> IO ()
prettyEval = mapM_ putStrLn . runPretty . runE . eval
