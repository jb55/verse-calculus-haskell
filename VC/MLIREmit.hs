-- A revised emitter that addresses the most blocking correctness issues
-- pointed out in the review:   
--   • every SSA-defining op now carries a result type ("!verse.val")   
--   • block arguments inside regions use the same dialect type   
--   • tuple types are printed as !verse.tuple<…> instead of hard-wired i64s   
--   • helper constants `verseValTy` and `tupleTy` centralise the type strings   
--   • region terminators and braces keep MLIR-valid syntax   
--   • added missing import for `when` from Control.Monad (fixes GHC-88464)   
--   • no behavioural changes to the lowering logic itself
-- Drop this next to VC.hs and `import MLIREmit_fixed` instead of the old file;
-- `freshDemo` should now yield MLIR that parses with `mlir-opt --verify-diagnostics`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module VC.MLIREmit
  ( emitExpr
  , freshDemo
  ) where

import           VC.Syntax                             -- the AST definitions
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Data.Text               (Text)
import           Control.Monad.State
import           Control.Monad           (when)  -- <- needed for `when`
import qualified Data.Map.Strict         as M

-- ────────────────────────────────────────────────────────────
-- Fresh-name + writer monad
-- ────────────────────────────────────────────────────────────

type Env     = M.Map Name Text             -- logical var → SSA name

-- (int supply,  reversed IR lines,  env)
type EmitM   = State (Int, [Text], Env)

fresh :: EmitM Text
fresh = do
  (i,ls,env) <- get
  put (i+1,ls,env)
  pure ("%" <> T.pack (show i))

tellLn :: Text -> EmitM ()
tellLn ln = modify $ \(i,ls,env) -> (i, ln:ls, env)

lookupVar :: Name -> EmitM Text
lookupVar n = gets (M.lookup n . (\(_,_,e) -> e)) >>= \case
  Just s  -> pure s
  Nothing -> error ("unbound logical variable " ++ n)

withVar :: Name -> Text -> EmitM a -> EmitM a
withVar n s m = do
  (i,ls,env) <- get
  put (i,ls,M.insert n s env)
  r <- m
  modify (\(i',ls',_) -> (i',ls',env))
  pure r

setVar :: Name -> Text -> EmitM ()
setVar n s = modify $ \(i,ls,env) -> (i,ls,M.insert n s env)

runEmit :: EmitM () -> Text
runEmit m = T.unlines . reverse . (\(_,ls,_) -> ls) $ execState m (0,[],M.empty)

-- ────────────────────────────────────────────────────────────
-- Dialect type helpers
-- ────────────────────────────────────────────────────────────

verseValTy :: Text
verseValTy = "!verse.val"

tupleTy :: Int -> Text
tupleTy n = "!verse.tuple<" <> T.intercalate "," (replicate n verseValTy) <> ">"

-- ────────────────────────────────────────────────────────────
-- Value lowering helpers
-- ────────────────────────────────────────────────────────────

mlirScalar :: Scalar -> EmitM Text
mlirScalar = \case
  VVar n  -> lookupVar n
  VInt k  -> pure (T.pack (show k) <> " : i64")
  VPrim Add -> pure "verse.add"
  VPrim Gt  -> pure "verse.gt"

mlirValue :: Value -> EmitM Text
mlirValue = \case
  S s -> mlirScalar s

  H (HTuple scs) -> do
    name <- fresh
    elems <- mapM mlirScalar scs
    let joined = T.intercalate ", " elems
        len    = length elems
    tellLn $ T.concat
      [ "  ", name
      , " = verse.tuple_from_values(", joined, ")"
      , " : ", tupleTy len
      ]
    pure name

  H (HLam _ _) -> error "λ-values not supported yet in emitter"

-- ────────────────────────────────────────────────────────────
-- Core lowering of Expr → MLIR
-- ────────────────────────────────────────────────────────────

lower :: Expr -> EmitM Text
lower = \case
  Val v -> mlirValue v

  Fail -> do
    r <- fresh
    tellLn ("  " <> r <> " = verse.fail : " <> verseValTy)
    pure r

  Seq e1 e2 -> lower e1 >> lower e2

  Choice l r -> do
    out <- fresh
    tellLn ("  " <> out <> " = verse.choice {")
    region l
    region r
    tellLn ("  } : " <> verseValTy)
    pure out

  Exists n body -> do
    res <- fresh
    arg <- fresh
    tellLn ("  " <> res <> " = verse.exist {")
    tellLn ("    ^bb0(" <> arg <> " : " <> verseValTy <> "):")
    withVar n arg $ do
      inner <- lower body
      tellLn ("    verse.yield " <> inner)
    tellLn ("  } : " <> verseValTy)
    pure res

  Eq lhs rhs -> do
    lhsSSA <- case lhs of
                S (VVar x) -> lookupVar x
                _          -> mlirValue lhs
    rhsSSA <- lower rhs
    res    <- fresh
    tellLn $ T.concat
      [ "  ", res, " = verse.unify ", lhsSSA, ", ", rhsSSA
      , " : ", verseValTy
      ]
    when (matchesVar lhs) (setVar (extractName lhs) res)
    pure res
    where
      matchesVar (S (VVar _)) = True
      matchesVar _            = False
      extractName (S (VVar x)) = x
      extractName _            = error "not a variable lhs"

  One e -> genericRegionOp "verse.one" e
  All e -> genericRegionOp "verse.all" e

  App f a -> do
    vf <- lower f
    va <- lower a
    res <- fresh
    tellLn ("  " <> res <> " = verse.apply " <> vf <> ", " <> va
             <> " : " <> verseValTy)
    pure res

  VApp{} -> error "Emitter expects VApp to be desugared before lowering"
 where
   region e = do
     tellLn "    {"
     v <- lower e
     tellLn ("    verse.yield " <> v)
     tellLn "    }"

   genericRegionOp opName body = do
     res <- fresh
     tellLn ("  " <> res <> " = " <> opName <> " {")
     region body
     tellLn ("  } : " <> verseValTy)
     pure res

-- ────────────────────────────────────────────────────────────
-- Public API
-- ────────────────────────────────────────────────────────────

emitExpr :: Expr -> Text
emitExpr e = runEmit $ do
  tellLn "module {"
  _ <- lower e
  tellLn "}"

freshDemo :: IO ()
freshDemo = T.putStrLn (emitExpr demo)

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
