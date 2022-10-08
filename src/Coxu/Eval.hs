module Coxu.Eval (eval) where

import Coxu.Parser
import Coxu.Base.Ternary (to10)


evalTerm :: Term -> Integer
evalTerm (DecTerm t) = t
evalTerm (TernTerm t) = to10 t
evalTerm (ParensExpr e) = eval e

eval :: Expr -> Integer
eval (ExprTerm t) = evalTerm t
eval (ExprNeg x) = negate (eval x)
eval (ExprAdd x y) = eval x + eval y
eval (ExprSub x y) = eval x - eval y
eval (ExprMul x y) = eval x * eval y
eval (ExprDiv x y) = eval x `div` eval y
