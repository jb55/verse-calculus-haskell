{-# LANGUAGE OverloadedStrings #-}
module VC.Parser (exprP, parseExpr) where

import VC.Syntax  -- your AST
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as P
import           Text.Megaparsec            (Parsec, (<|>), (<?>))
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Applicative        (empty)
import           Control.Monad              (void)

---------------------------------------------------------------------
-- Basic parser setup
---------------------------------------------------------------------

type Parser = Parsec Void String

space :: Parser ()
space = L.space C.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment  "--"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme  :: Parser a -> Parser a
lexeme  = L.lexeme space
symbol  :: String -> Parser String
symbol  = L.symbol space
parens  :: Parser a -> Parser a
parens  = P.between (symbol "(") (symbol ")")

---------------------------------------------------------------------
-- Lexical layer — identifiers, integers, primitives
---------------------------------------------------------------------

identifier :: Parser String
identifier = lexeme ((:) <$> C.letterChar <*> P.many C.alphaNumChar) <?> "identifier"

integer :: Parser Int
integer = lexeme L.decimal <?> "integer"

scalarP :: Parser Scalar
scalarP =
      VVar  <$> identifier
  <|> VInt  <$> integer
  <|> VPrim <$> (Add <$ C.string "add" <|> Gt <$ C.string "gt")
  <?> "scalar"

---------------------------------------------------------------------
-- Values  (scalars, tuples)
---------------------------------------------------------------------

tupleP :: Parser Heap
tupleP =
  HTuple <$> P.between (symbol "(") (symbol ")")
                      (scalarP `P.sepBy1` symbol ",")

valueP :: Parser Value
valueP =
      S <$> scalarP
  <|> H <$> tupleP
  <?> "value"

---------------------------------------------------------------------
-- Expression grammar
--
--   choice   ::= seq ('|' seq)*
--   seq      ::= eq  (';' eq )*
--   eq       ::= value '=' simple | simple
--   simple   ::= value | fail | one expr | all expr | '(' expr ')'
--   quant    ::= identifier '.'
--
---------------------------------------------------------------------

exprP :: Parser Expr
exprP = do
  qs   <- P.many (P.try (identifier <* symbol "."))  -- allow back-tracking
  body <- choiceP
  pure (foldr Exists body qs)

choiceP :: Parser Expr
choiceP = do
  xs <- seqP `P.sepBy1` symbol "|"
  pure (foldr1 Choice xs)

seqP :: Parser Expr
seqP = do
  xs <- eqP `P.sepBy1` symbol ";"
  pure (foldr1 Seq xs)

eqP :: Parser Expr
eqP = P.try equality <|> simpleP
  where
    equality = do
      v <- valueP
      _ <- symbol "="
      e <- simpleP
      pure (Eq v e)

simpleP :: Parser Expr
simpleP =
      Val <$> valueP
  <|> Fail <$  symbol "fail"
  <|> One  <$> (symbol "one" *> exprP)
  <|> All  <$> (symbol "all" *> exprP)
  <|> parens exprP
  <?> "expression"

---------------------------------------------------------------------
-- Public helper
---------------------------------------------------------------------

parseExpr :: String -> Either (P.ParseErrorBundle String Void) Expr
parseExpr = P.parse (space *> exprP <* P.eof) "<input>"
