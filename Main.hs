
import VC.Syntax (prettyExpr)
import VC.Parser (parseExpr)
import VC.SmallStep (normalForm)
import VC.MLIREmit (emitExpr)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  src <- getContents
  case parseExpr src of
    Left err   -> putStrLn (show err)
    Right expr -> do
      let norm = normalForm expr   -- Small-step for now
      putStrLn (prettyExpr norm)
      T.putStrLn (emitExpr norm)     -- MLIR dump
