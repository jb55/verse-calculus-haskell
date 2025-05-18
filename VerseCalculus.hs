{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module VC where

import qualified Data.Map.Strict as M
import           Data.List        (intercalate)
import Control.Applicative

import Control.Monad (MonadPlus(..), guard)
import Control.Applicative (Alternative(..))

instance MonadPlus Eval where
  mzero = empty
  mplus = (<|>)

-- | Names for both term and logical variables
type Name = String

-- | Primitive operators  (add  &  gt)
data Prim = Add | Gt deriving (Eq,Show)

-- | Scalar values  s ::= x | k | op
data Scalar
  = VVar Name          -- logical var   (x, y …)
  | VInt Int           -- integer       (k)
  | VPrim Prim         -- primitive op  (add, gt)
  deriving (Eq,Show)

-- | Heap values  h ::= ⟨s …⟩ | λx.e
data Heap
  = HTuple [Scalar]    -- tuple of scalars  ⟨s1…sn⟩
  | HLam Name Expr     -- λ-abstraction     λx.e
  deriving (Eq,Show)

-- | Values  v ::= s | h
data Value
  = S Scalar
  | H Heap
  deriving (Eq,Show)

-- | Expressions  (Fig. 1 𝑒 ::= …)                      [oai_citation:1‡9bf43d512c0d390b4f3da214878cf5505bfa6b5fb27c50f8e093b3e19371eac3.pdf](file-service://file-GFyygFG8ZqyX71yCLBbBuS)
data Expr
  = Val Value                  -- v
  | Seq Expr Expr              -- e₁ ; e₂
  | Exists Name Expr           -- ∃x.e
  | Fail                       -- fail
  | App Expr Expr              -- e₁ e₂         (desugaring later)
  | VApp Value Value           -- v₁ v₂          (core form)
  | Choice Expr Expr           -- e₁ ▽ e₂        (paper writes e₁  e₂)
  | One   Expr                 -- one{e}
  | All   Expr                 -- all{e}
  | Eq    Value Expr           -- v = e          (left of ‘;’ only)
  deriving (Eq,Show)

type HeapEnv = M.Map Name Value   -- x ↦ v   (scalar OR heap)

-- | An evaluation is a nondeterministic computation that
--   may update the heap and can fail (empty list).
newtype Eval a = E { runE :: HeapEnv -> [(HeapEnv,a)] }
  deriving Functor

instance Applicative Eval where
  pure x = E $ \h -> [(h,x)]
  E mf <*> E mx = E $ \h -> [ (h2,f x) | (h1,f) <- mf h
                                       , (h2,x) <- mx h1 ]

instance Monad Eval where
  return = pure
  E m >>= k = E $ \h -> concat [ runE (k a) h' | (h',a) <- m h ]

-- | helper
liftMaybe :: Maybe a -> Eval a
liftMaybe = maybe mzero return

instance Alternative Eval where
  empty = E $ const []
  E m <|> E n = E $ \h -> m h ++ n h

unify :: Value -> Value -> Eval ()
unify (S (VVar x)) v = bind x v        -- logical var on LHS
unify v (S (VVar x)) = bind x v
unify (S (VInt k1)) (S (VInt k2))
  | k1 == k2  = return ()
  | otherwise = mzero
unify (H (HTuple xs)) (H (HTuple ys))
  | length xs == length ys
  = mapM_ (uncurry (unify . S)) (zip (map S xs) (map S ys))
unify _ _ = mzero                       -- u-fail

bind :: Name -> Value -> Eval ()
bind x v = E $ \h -> case M.lookup x h of
                       Nothing        -> [(M.insert x v h, ())]
                       Just v' | v==v' -> [(h,())]  -- identical
                               | otherwise -> []     -- clash → fail

eval :: Expr -> Eval Value
eval = \case
  Val v       -> return v
  Fail        -> mzero
  Seq e1 e2   -> eval e1 >> eval e2      -- sequencing
  Exists x e  -> local (M.delete x) (eval e)
  Eq v e2     -> eval e2 >>= unify v >> return v
  Choice e1 e2-> eval e1 <|> eval e2     -- left-to-right
  One  e      -> head <$> eval e         -- ‘one’ is deterministic pick
  All  e      -> H . HTuple . fmap (extractInt) <$> collect e
  App  f a    -> eval f >>= \vf ->
                 eval a >>= \va ->
                 eval (VApp vf va)
  VApp (H (HLam x body)) v
              -> eval (Exists x (Seq (Eq (S (VVar x)) (Val v)) body))
  VApp (H (HTuple scs)) (S (VInt i))
              | i >= 0, i < length scs -> return (S (scs !! i))
              | otherwise              -> mzero
  VApp (S (VPrim Add)) (H (HTuple [S (VInt a), S (VInt b)]))
              -> return (S (VInt (a+b)))
  VApp (S (VPrim Gt))  (H (HTuple [S (VInt k1), S (VInt k2)]))
              | k1 > k2 -> return (S (VInt k1))
              | otherwise -> mzero
  _           -> error "stuck term"
  where
    local f (E m) = E (m . f)
    collect e     = E $ \h ->
                       [ (h2,v:vs) | (h1,v) <- runE (eval e) h
                                   , (h2,vs) <- runE (collect e) h1 ]
                       ++ [(h,[])]         -- base case
    extractInt (S (VInt k)) = S (VInt k)
    extractInt v            = v			      

demo :: Expr
demo =
  Exists "x" $
  Exists "y" $
  Exists "z" $
    Seq (Eq (S (VVar "x")) (Val . H . HTuple $ [VVar "y", VInt 3]))
      (Seq (Eq (S (VVar "x")) (Val . H . HTuple $ [VInt 2,  VVar "z"]))
           (Val . S $ VVar "y"))
