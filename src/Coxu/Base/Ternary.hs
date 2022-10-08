module Coxu.Base.Ternary where

data Trit = N | Z | P deriving (Show, Eq)

tritPlus :: Trit -> Trit -> (Trit, Trit)
tritPlus P P = (N, P)
tritPlus N P = (Z, Z)
tritPlus N N = (P, N)
tritPlus Z x = (x, Z)
tritPlus a b = tritPlus b a

tritToNum :: Num a => Trit -> a
tritToNum N = -1
tritToNum Z = 0
tritToNum P = 1

tritInverse :: Trit -> Trit
tritInverse P = N
tritInverse Z = Z
tritInverse N = P

newtype BTern = BTern [Trit] deriving (Show, Eq)


to10 :: Num a => BTern -> a
to10 (BTern trits) = to10' 0 trits
  where
    to10' acc (x:xs) = to10' (acc * 3 + tritToNum x) xs
    to10' acc [] = acc


from10 :: (Ord a, Integral a) => a -> BTern
from10 num
  | num > 0 = BTern $ from10' [] Z num
  | num == 0 = BTern [Z]
  | otherwise = case from10 (-num) of { BTern ts -> BTern $ map tritInverse ts }
  where
    from10' :: (Ord a, Integral a) => [Trit] -> Trit -> a -> [Trit]
    from10' acc Z 0 = acc
    from10' acc b 0 = b:acc
    from10' acc b n
      | n `mod` 3 == 0 = from10' (b:acc) Z (n `div` 3)
      | n `mod` 3 == 1 = from10' (fst (tritPlus P b):acc) (snd (tritPlus P b)) (n `div` 3)
      | otherwise      = from10' (fst (tritPlus N b):acc) P (n `div` 3)
