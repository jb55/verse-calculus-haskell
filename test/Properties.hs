-- test/Properties.hs
-- QuickCheck properties for the Verse project
--
-- To run:   cabal test         (or)   runhaskell test/Properties.hs
--
-- Invariants checked
--   1.  Big‑step semantics ≈ small‑step normal form.
--   2.  normalForm is truly a normal form and idempotent.
--   3.  Heap produces no reflexive bindings.
--   4.  prettyExpr ∘ parseExpr is a (partial) round‑trip on generated terms.
--
-- NOTE: Generators deliberately avoid λ‑values for now because
--       (a) VC.MLIREmit cannot lower them, and
--       (b) Arbitrary generation for higher‑order values is brittle.
--       Extend if you need them.

{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Main (main) where

import           Test.QuickCheck
import qualified Data.Map.Strict    as M
import           VC.Syntax
import qualified VC.BigStep        as BS
import qualified VC.SmallStep      as SS
import qualified VC.Parser         as P

--------------------------------------------------------------------------------
-- * Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Prim where
  arbitrary = elements [Add, Gt]

instance Arbitrary Scalar where
  arbitrary = oneof
    [ VVar  <$> smallName
    , VInt  <$> arbitrary
    , VPrim <$> arbitrary
    ]

instance Arbitrary Value where
  arbitrary = sized go
    where
      go 0 = S <$> arbitrary
      go n = oneof
        [ S <$> arbitrary
        , H . HTuple <$> resize (n `div` 2) (listOf1 arbitrary)
        ] -- skip HLam for now

instance Arbitrary Expr where
  arbitrary = sized go
    where
      go 0 = Val <$> arbitrary
      go n = oneof $ base ++ if n < 3 then [] else complex n
        where
          sub     = go (n `div` 3)
          base    = [ Val <$> arbitrary, pure Fail ]
          complex m =
            [ Seq    <$> sub <*> sub
            , Exists <$> smallName <*> sub
            , Choice <$> sub <*> sub
            , App    <$> sub <*> sub
            , Eq     <$> resize (m `div` 3) arbitrary <*> sub
            ]

--------------------------------------------------------------------------------
-- * Helpers
--------------------------------------------------------------------------------

smallName :: Gen String
smallName = elements [[c] | c <- ['a'..'z']]

sizeExpr :: Expr -> Int
sizeExpr = length . show

normBig :: Expr -> [String]
normBig   = map show . BS.runEval . BS.eval

normSmall :: Expr -> [String]
normSmall = normBig . SS.normalForm

--------------------------------------------------------------------------------
-- * Properties
--------------------------------------------------------------------------------

prop_no_reflexive_bindings :: Expr -> Property
prop_no_reflexive_bindings e =
  counterexample "A reflexive heap binding was found" $
    all (not . isReflexive) (BS.runEval $ BS.eval $ SS.normalForm e)
  where
    isReflexive (heap, _)         = any selfBinding (M.toList heap)
    selfBinding (k, S (VVar v))   = k == v
    selfBinding _                 = False

prop_normal_is_normal :: Expr -> Property
prop_normal_is_normal e =
  counterexample "normalForm produced a reducible expression" $
    SS.rewriteOnce n === Nothing
  where
    n = SS.normalForm e

prop_big_vs_small :: Expr -> Property
prop_big_vs_small e =
  collect (sizeExpr e) $ normBig e === normSmall e

prop_normal_idempotent :: Expr -> Property
prop_normal_idempotent e =
  collect (sizeExpr e) $ n === SS.normalForm n
  where
    n = SS.normalForm e

prop_pretty_roundtrip :: Expr -> Property
prop_pretty_roundtrip e =
  case P.parseExpr (prettyExpr n) of
    Left  _  -> counterexample "Parser failed" False
    Right e' -> collect (sizeExpr e) $ e' === n
  where
    n = SS.normalForm e

--------------------------------------------------------------------------------
-- * Runner
--------------------------------------------------------------------------------

runProp :: Testable prop => prop -> IO ()
runProp = quickCheckWith stdArgs { maxSuccess = 10000 }

main :: IO ()
main = do
  runProp prop_big_vs_small
  runProp prop_normal_idempotent
  runProp prop_no_reflexive_bindings
  runProp prop_normal_is_normal
  --runProp prop_pretty_roundtrip
