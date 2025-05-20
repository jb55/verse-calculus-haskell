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
--space = L.space C.space lineCmnt blockCmnt   -- ‘space’ = 0 … n blanks
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
--  NEW: recognise the ‘∃’ glyph that prettyExpr emits
---------------------------------------------------------------------

existsSym :: Parser ()
existsSym = void (symbol "∃")           -- U+2203
         <|> void (symbol "exists")     -- handy ASCII fallback

---------------------------------------------------------------------
-- Lexical layer — identifiers, integers, primitives
---------------------------------------------------------------------

identifier :: Parser String
identifier = (lexeme . P.try) $ do
    s <- (:) <$> C.letterChar <*> P.many C.alphaNumChar
    if s `elem` ["fail","one","all","add","gt"]
       then fail "reserved word"
       else pure s

integer :: Parser Int
integer = lexeme (L.signed space L.decimal) <?> "integer"

scalarP :: Parser Scalar
scalarP =
      VPrim <$> (Add <$ symbol "add" <|> Gt <$ symbol "gt")
  <|> VVar  <$> identifier
  <|> VInt  <$> integer
  <?> "scalar"

---------------------------------------------------------------------
-- Values  (scalars, tuples)
---------------------------------------------------------------------
tupleP :: Parser Heap
tupleP = HTuple <$> P.between open close elems
  where
    open  =  void (symbol "(")  <|> void (symbol "⟨")
    close =  void (symbol ")")  <|> void (symbol "⟩")
    elems = scalarP `P.sepBy1` symbol ","

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

exprP = do
  qs   <- P.many quantifier
  body <- choiceP
  pure (foldr Exists body qs)
  where
    quantifier :: Parser Name
    quantifier = P.try $ do
      _ <- P.optional existsSym      -- accept both “∃x.” and plain “x.”
      x <- identifier
      _ <- symbol "."
      pure x

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

-- | Smallest “atom” that can stand as a term.
atomP  :: Parser Expr
atomP =
      Fail <$  symbol "fail"
  <|> One  <$> (symbol "one" *> exprP)
  <|> All  <$> (symbol "all" *> exprP)
  <|> Val  <$> valueP
  <|> parens exprP
  <?> "atom"

-- | Left-associative, whitespace-separated application.
simpleP :: Parser Expr
simpleP = do
  ts <- P.some atomP
  pure (foldl1 App ts)
  <?> "expression"

---------------------------------------------------------------------
-- Public helper
---------------------------------------------------------------------

parseExpr :: String -> Either (P.ParseErrorBundle String Void) Expr
parseExpr = P.parse (space *> exprP <* P.eof) "<input>"
