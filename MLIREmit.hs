{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module MLIREmit
  ( emitExpr          -- :: Expr -> Text
  , freshDemo         -- IO ()  -- quick sanity check
  ) where

import           VC                        -- your AST
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Text           (Text)
import           Control.Monad.State
import qualified Data.Map.Strict as M

-- ────────────────────────────────────────────────────────────
-- Simple writer + fresh-name monad
-- ────────────────────────────────────────────────────────────

type Env   = M.Map Name Text          -- Verse var  ↦  SSA name
type Supply = Int          -- monotonically increasing counter
type EmitM  = State (Supply, [Text], Env)  -- (counter, reversed lines)

fresh :: EmitM Text
fresh = do
  (n, ss, env) <- get
  put (n+1, ss, env)
  pure (T.pack ("%" <> show n))

-- read & extend the environment
withVar :: Name -> Text -> EmitM a -> EmitM a
withVar n s m = do
  (i,ls,env) <- get
  put (i,ls,M.insert n s env)
  r <- m
  modify (\(i',ls',_) -> (i',ls',env))   -- restore
  pure r

lookupVar :: Name -> EmitM Text
lookupVar n = do
  (_,_,env) <- get
  case M.lookup n env of
    Just s  -> pure s
    Nothing -> error ("unbound logical variable " ++ n)

tellLn :: Text -> EmitM ()
tellLn ln = modify (\(n,ss,env) -> (n, ln:ss, env))

runEmit :: EmitM Text -> Text
runEmit m = T.unlines . reverse . (\(_,ls,_) -> ls) $ execState m (0, [], M.empty)


-- ────────────────────────────────────────────────────────────
-- Primitive helpers
-- ────────────────────────────────────────────────────────────


-- Emit a Value, returning the SSA value name
mlirValue :: Value -> EmitM Text
mlirValue = \case
  -- plain scalar: just delegate to the scalar emitter
  S s -> mlirScalar s

  -- tuple of scalars  ⟨s1…sn⟩
  H (HTuple scs) -> do
    name        <- fresh                          -- %id for the tuple
    elemsTexts  <- mapM mlirScalar scs            -- [Text] after env lookup
    let elems   = T.intercalate ", " elemsTexts
        len     = length elemsTexts
    tellLn $ T.concat
      [ "  ", name
      , " = verse.tuple_from_values(", elems, ")"
      , " : ", tupleTy len
      ]
    pure name

  H (HLam _ _) ->
    error "λ-values not yet supported by emitter"

----------------------------------------------------------------------
-- Scalar → SSA (needs env lookup for VVar)
----------------------------------------------------------------------
mlirScalar :: Scalar -> EmitM Text
mlirScalar = \case
  VVar n    -> lookupVar n
  VInt k    -> pure (T.pack (show k) <> " : i64")
  VPrim Add -> pure "verse.add"   -- leave these as identifiers for now
  VPrim Gt  -> pure "verse.gt"

----------------------------------------------------------------------
-- Helper for tuple types:  tuple<i64,i64, …>
----------------------------------------------------------------------
tupleTy :: Int -> Text
tupleTy n = "tuple<" <> T.intercalate "," (replicate n "i64") <> ">"

-- ────────────────────────────────────────────────────────────
-- Core lowering of Expr
-- Each branch returns the *SSA value* that represents the Expr.
-- ────────────────────────────────────────────────────────────

lower :: Expr -> EmitM Text
lower = \case
  Val v -> mlirValue v

  Fail  -> do
     name <- fresh
     tellLn ("  " <> name <> " = verse.fail")
     pure name

  Seq e1 e2 -> do
     _ <- lower e1
     lower e2

  Choice l r -> do
     out <- fresh
     tellLn ("  " <> out <> " = verse.choice {")
     region l
     region r
     tellLn "  }"
     pure out


  Exists v body -> do
     res    <- fresh          -- result of verse.exist
     arg    <- fresh          -- block argument inside region
     tellLn ("  " <> res <> " = verse.exist {")
     tellLn ("    ^bb0(" <> arg <> " : any):")
     withVar v arg $ do        -- 👈  store v ↦ arg
       inner <- lower body
       tellLn ("    verse.yield " <> inner)
     tellLn "  }"
     pure res

  Eq lhs rhs -> case lhs of
    -- LHS is a logical variable x
    S (VVar x) -> do
      oldSSA <- lookupVar x          -- should be %1 the first time
      rhsSSA <- lower rhs
      resSSA <- fresh
      tellLn $ "  " <> resSSA <> " = verse.unify "
            <> oldSSA <> ", " <> rhsSSA

      setVar x resSSA                -- ★  keep x ↦ resSSA forever
      pure resSSA

    -- All other LHS forms
    _ -> do
      lhsSSA <- mlirValue lhs
      rhsSSA <- lower rhs
      resSSA <- fresh
      tellLn $ "  " <> resSSA <> " = verse.unify "
            <> lhsSSA <> ", " <> rhsSSA
      pure resSSA

  One e -> do
     res <- fresh
     tellLn ("  " <> res <> " = verse.one {")
     region e
     tellLn "  }"
     pure res

  All e -> do
     res <- fresh
     tellLn ("  " <> res <> " = verse.all {")
     region e
     tellLn "  }"
     pure res

  App f a  -> do
     vf <- lower f
     va <- lower a
     res <- fresh
     tellLn ("  " <> res <> " = verse.apply " <> vf <> ", " <> va)
     pure res

  VApp{} -> error "Emitter expects VApp to be desugared during eval"

 where
   yield v = tellLn ("    verse.yield " <> v)
   region e = do
     tellLn "    {"
     v <- lower e
     yield v
     tellLn "    }"

-- Keep a simple environment for ∃-bound vars
pushVar :: Name -> Text -> EmitM a -> EmitM a
pushVar n s = id   -- NOTE: our tiny emitter just reuses the generated SSA
                   -- name, but you could keep a Map Name Text if needed.

setVar :: Name -> Text -> EmitM ()
setVar n s = modify $ \(i,ls,env) -> (i,ls, M.insert n s env)

-- ────────────────────────────────────────────────────────────
-- Public API
-- ────────────────────────────────────────────────────────────

emitExpr :: Expr -> Text
emitExpr e = runEmit $ do
  tellLn "module {"
  res <- lower e
  tellLn ("  // entry result: " <> res)
  tellLn "}"
  pure ""

-- quick sanity check
freshDemo :: IO ()
freshDemo = T.putStrLn (emitExpr demo)
