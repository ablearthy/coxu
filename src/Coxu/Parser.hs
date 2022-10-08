{-# LANGUAGE OverloadedStrings #-}
module Coxu.Parser (Expr (..), exprP ) where

import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Coxu.Base.Ternary as BT

import Control.Monad.Combinators.Expr


type Parser = Parsec Void T.Text


sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

data Term
  = DecTerm Int
  | TernTerm BT.BTern
  | ParensExpr Expr
  deriving (Show, Eq)

data Expr
  = ExprAdd  Expr Expr
  | ExprSub  Expr Expr
  | ExprNeg  Expr
  | ExprMul  Expr Expr
  | ExprDiv  Expr Expr
  | ExprTerm Term
  deriving (Show, Eq)

bTernP :: Parser BT.BTern
bTernP = mkTern <$> (char 't' *> takeWhile1P (Just "trit") isTrit)
  where
    isTrit :: Char -> Bool
    isTrit '-' = True
    isTrit '0' = True
    isTrit '+' = True
    isTrit _   = False

    mkTern :: T.Text -> BT.BTern
    mkTern = BT.BTern . T.foldl' processChar []
    
    processChar :: [BT.Trit] -> Char -> [BT.Trit]
    processChar xs '-' = BT.N:xs
    processChar xs '+' = BT.P:xs
    processChar xs '0' = BT.Z:xs
    processChar xs _   = xs

termP :: Parser Term
termP = DecTerm <$> L.decimal
  <|> (TernTerm <$> bTernP)
  <|> (ParensExpr <$> parens exprP)

exprP :: Parser Expr
exprP = makeExprParser (ExprTerm <$> lexeme termP)
  [ [ prefix "-" ExprNeg 
    , prefix "+" id ]
  , [ binary "*" ExprMul 
    , binary "/" ExprDiv ]
  , [ binary "+" ExprAdd 
    , binary "-" ExprSub ] ]

binary :: T.Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix :: T.Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
