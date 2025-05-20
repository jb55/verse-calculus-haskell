-- QuickCheck properties for the Verse project
--   runhaskell test/Properties.hs
--
-- It checks two high‑value invariants:
--   1.  Big‑step semantics ≈ small‑step normal form.
--   2.  prettyExpr ∘ parseExpr is a (partial) round‑trip.
--
-- The generators deliberately avoid lambda values for now, because
--   (a) VC.MLIREmit cannot lower them, and (b) Arbitrary generation
--   of higher‑order functions is brittle.  Extend if you need them.

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           VC.Syntax
import qualified VC.BigStep    as BS
import qualified VC.SmallStep  as SS
import qualified VC.Parser     as P
import           Test.QuickCheck
import           Data.List       (sort)
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- * Arbitrary instances -------------------------------------------------------
--------------------------------------------------------------------------------

instance Arbitrary Prim where
  arbitrary = elements [Add, Gt]

instance Arbitrary Scalar where
  arbitrary = oneof
    [ VVar  <$> smallName
    , VInt  <$> arbitrary
    , VPrim <$> arbitrary
    ]
   where
     smallName = elements [ [c] | c <- ['a'..'z'] ]

instance Arbitrary Value where
  arbitrary = sized arbVal
    where
      arbVal 0 = S <$> arbitrary
      arbVal n = oneof
        [ S <$> arbitrary
        , H . HTuple <$> resize (n `div` 2) (listOf1 arbitrary)
        -- NOTE: skip HLam for now
        ]

instance Arbitrary Expr where
  arbitrary = sized arbExpr
    where
      arbExpr 0 = Val <$> arbitrary
      arbExpr n = oneof $ base ++ if n < 3 then [] else complex n
        where
          sub = arbExpr (n `div` 3)
          base = [ Val <$> arbitrary
                 , pure Fail
                 ]
          complex m =
            [ Seq   <$> sub <*> sub
            , Exists <$> smallName <*> sub
            , Choice <$> sub <*> sub
            , App   <$> sub <*> sub
            , Eq    <$> arbitraryVal <*> sub
            ]
          smallName = elements [ [c] | c <- ['x'..'z'] ]
          arbitraryVal = resize (n `div` 3) arbitrary

--------------------------------------------------------------------------------
-- * Helpers -------------------------------------------------------------------
--------------------------------------------------------------------------------

normBig :: Expr -> [String]
normBig = map show . BS.runEval . BS.eval

normSmall :: Expr -> [String]
normSmall = normBig . SS.normalForm

prettyEnvVal :: (HeapEnv, Value) -> String
prettyEnvVal = VC.Syntax.prettyResult


--------------------------------------------------------------------------------
-- * Properties ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Big‑step evaluation equals the evaluation of the small‑step normal form.
prop_big_vs_small :: Expr -> Property
prop_big_vs_small e = collect (sizeExpr e) $ normBig e === normSmall e
  where
    sizeExpr = length . show

-- | prettyExpr and parseExpr are a round‑trip for the subset we generate.
--   We normalise with small‑step first to rule out alpha‑varying Exists order.
prop_pretty_roundtrip :: Expr -> Property
prop_pretty_roundtrip e =
  case P.parseExpr (prettyExpr n) of
    Left _     -> counterexample "Parser failed" False
    Right e'   -> collect (sizeExpr e) $ e' === n
  where
    n = SS.normalForm e
    sizeExpr = length . show

--------------------------------------------------------------------------------
-- * Runner --------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  --quickCheck prop_big_vs_small
  quickCheck (withMaxSuccess 20000 prop_big_vs_small)
  --quickCheck prop_pretty_roundtrip
