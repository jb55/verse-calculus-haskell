{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module VC where

import qualified Data.Map.Strict as M
import           Data.List        (intercalate)
import           Control.Applicative
import           Control.Monad (MonadPlus(..), guard)

-- MonadPlus instance for Eval
instance MonadPlus Eval where
  mzero = empty
  mplus = (<|>)

-- | Names for both term and logical variables
type Name = String

-- | Primitive operators  (add  &  gt)
data Prim = Add | Gt deriving (Eq, Show)

-- | Scalar values  s ::= x | k | op
data Scalar
  = VVar Name          -- logical var   (x, y …)
  | VInt Int           -- integer       (k)
  | VPrim Prim         -- primitive op  (add, gt)
  deriving (Eq, Show)

-- | Heap values  h ::= ⟨s …⟩ | λx.e
data Heap
  = HTuple [Scalar]    -- tuple of scalars  ⟨s1…sn⟩
  | HLam Name Expr     -- λ-abstraction     λx.e
  deriving (Eq, Show)

-- | Values  v ::= s | h
data Value
  = S Scalar
  | H Heap
  deriving (Eq, Show)

-- | Expressions  (Fig. 1 𝑒 ::= …)
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

type HeapEnv = M.Map Name Value

-- Evaluation Monad
newtype Eval a = E { runE :: HeapEnv -> [(HeapEnv, a)] }
  deriving Functor

instance Applicative Eval where
  pure x = E $ \h -> [(h, x)]
  E mf <*> E mx = E $ \h -> [ (h2, f x) | (h1, f) <- mf h, (h2, x) <- mx h1 ]

instance Monad Eval where
  return = pure
  E m >>= k = E $ \h -> concat [ runE (k a) h' | (h', a) <- m h ]

instance Alternative Eval where
  empty = E $ const []
  E m <|> E n = E $ \h -> m h ++ n h

-- Helper
liftMaybe :: Maybe a -> Eval a
liftMaybe = maybe mzero return

-- Unification
unify :: Value -> Value -> Eval ()
unify (S (VVar x)) v = bind x v
unify v (S (VVar x)) = bind x v
unify (S (VPrim p1)) (S (VPrim p2))
  | p1 == p2  = return ()
  | otherwise = mzero
unify (S (VInt k1)) (S (VInt k2))
  | k1 == k2  = return ()
  | otherwise = mzero
unify (H (HTuple xs)) (H (HTuple ys))
  | length xs == length ys
  = mapM_ (uncurry unify) (zip (map S xs) (map S ys))
unify _ _ = mzero

bind :: Name -> Value -> Eval ()
bind x v = E $ \h -> case M.lookup x h of
  Nothing   -> [(M.insert x v h, ())]                 -- first binding, just record it
  Just v'   -> runE (unify v' v) h                    -- merge with existing binding

resolve :: HeapEnv -> Value -> Value
resolve h (S (VVar n)) = maybe (S (VVar n)) (resolve h) (M.lookup n h)
resolve _ v            = v

-- Evaluation
eval :: Expr -> Eval Value
eval = \case
  Val v       -> return v
  Fail        -> mzero
  Seq e1 e2   -> eval e1 >> eval e2
  Exists x e  -> eval e
  Eq v e2     -> eval e2 >>= \v2 -> unify v v2 >> return v
  Choice e1 e2 -> eval e1 <|> eval e2
  One e       -> fmap head (collect e)
  All e       -> fmap (H . HTuple . map extractScalar) (collect e)
  App f a     -> eval f >>= \vf ->
                 eval a >>= \va ->
                 eval (VApp vf va)
  VApp (H (HLam x body)) v ->
    eval (Exists x (Seq (Eq (S (VVar x)) (Val v)) body))
  VApp (H (HTuple scs)) (S (VInt i))
    | i >= 0, i < length scs -> return (S (scs !! i))
    | otherwise              -> mzero
  VApp (S (VPrim Add)) (H (HTuple [VInt a, VInt b])) ->
    return (S (VInt (a + b)))
  VApp (S (VPrim Gt)) (H (HTuple [VInt k1, VInt k2]))
    | k1 > k2 -> return (S (VInt k1))
    | otherwise -> mzero
  _ -> error "stuck term"
  where
    local f (E m) = E (m . f)
    collect e = E $ \h ->
      [ (h2, v:vs) | (h1, v) <- runE (eval e) h
                   , (h2, vs) <- runE (collect e) h1 ]
      ++ [(h, [])]

-- Safely extract Scalar from Value
extractScalar :: Value -> Scalar
extractScalar (S s) = s
extractScalar _     = error "Expected scalar value"

-- ────────────────────────────────────────────────────────────────
-- Pretty printing helpers
-- ────────────────────────────────────────────────────────────────
prettyScalar :: Scalar -> String
prettyScalar = \case
  VVar n    -> n
  VInt k    -> show k
  VPrim Add -> "add"
  VPrim Gt  -> "gt"

prettyHeap :: Heap -> String
prettyHeap = \case
  HTuple ss -> "⟨" ++ intercalate ", " (map prettyScalar ss) ++ "⟩"
  HLam x e  -> "λ" ++ x ++ ". …"              -- omit body; can add later

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

-- A convenience wrapper that both *evaluates* and *resolves* logical vars
runPretty :: Expr -> [String]
runPretty e =
  [ prettyResult (h, resolve h v)
  | (h, v) <- runE (eval e) M.empty ]

-- ────────────────────────────────────────────────────────────────
-- Pretty-printing Expr without evaluating it
-- ────────────────────────────────────────────────────────────────
prettyExpr :: Expr -> String
prettyExpr = go 0
  where
    -- Higher number == binds tighter
    go :: Int -> Expr -> String
    go _ (Val v)          = prettyValue v
    go _ Fail             = "fail"

    go p (Exists x e)     = paren (p > 0) $
                              "∃" ++ x ++ ". " ++ go 0 e

    go p (Seq e1 e2)      = bin 1 ";"  e1 e2   -- precedence 1
    go p (Choice e1 e2)   = bin 1 "|"   e1 e2
    go p (Eq v e)         = paren (p > 1) $
                              prettyValue v ++ " = " ++ go 2 e

    go p (App f a)        = paren (p > 2) $
                              go 2 f ++ " " ++ go 3 a
    go p (VApp v1 v2)     = paren (p > 2) $
                              prettyValue v1 ++ " ⋅ " ++ prettyValue v2

    go p (One e)          = "one " ++ paren (needsParen e) (go 3 e)
    go p (All e)          = "all " ++ paren (needsParen e) (go 3 e)

    -- helpers ----------------------------------------------------
    bin prec op e1 e2 = paren (p > prec) $
                          go prec e1 ++ " " ++ op ++ " " ++ go prec e2
      where p = prec

    needsParen Val{}   = False
    needsParen Fail    = False
    needsParen _       = True

    paren True  s = "(" ++ s ++ ")"
    paren False s = s

printExpr :: Expr -> IO ()
printExpr = putStrLn . prettyExpr

printEval :: Expr -> IO ()
printEval = mapM_ putStrLn . runPretty

-- ────────────────────────────────────────────────────────────────
-- Smart constructors & tiny DSL
-- (drop these just after the data-type definitions)
-- ────────────────────────────────────────────────────────────────

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
lamV  = (H .) . HLam          -- handy but unused in the demo

-- Expressions ----------------------------------------------------
valE :: Value -> Expr         -- just lift to Expr
valE = Val

existsE :: Name -> Expr -> Expr
existsE = Exists

seqE :: Expr -> Expr -> Expr  -- left-to-right sequencing
seqE = Seq

(.=.) :: Value -> Value -> Expr  -- inline equality test
v .=. w = Eq v (valE w)

infix  1 .=.
infixr 0 `seqE`

-- ────────────────────────────────────────────────────────────────
-- Demo
-- ────────────────────────────────────────────────────────────────
demo :: Expr
demo =
  existsE "x" $
  existsE "y" $
  existsE "z" $
        ( varV "x" .=. tupleV [ varS "y", intS 3 ] )
     `seqE`
        ( varV "x" .=. tupleV [ intS 2  , varS "z" ] )
     `seqE`
        valE (varV "y")
