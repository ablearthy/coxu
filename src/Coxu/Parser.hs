{-# LANGUAGE OverloadedStrings #-}
module Coxu.Parser (Expr (..), exprP ) where

import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Coxu.Base.Ternary as BT


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

data Term
  = DecTerm Int
  | TernTerm BT.BTern
  | ParensExpr Expr
  deriving (Show, Eq)

data Expr
  = ExprAdd  Term Term
  | ExprSub  Term Term
  | ExprNeg  Term
  | ExprMul  Term Term
  | ExprDiv  Term Term
  | ExprTerm Term
  deriving (Show, Eq)

exprP :: Parser Expr
exprP = undefined
