{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Coxu.Parser
import Coxu.Eval
import Coxu.Base.Ternary

import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  content <- TIO.getContents
  case parse exprP "" content of
    Left bundle -> do
      putStr $ errorBundlePretty bundle
    Right expr -> TIO.putStrLn $ showResult (eval expr)


showResult :: Integer -> T.Text
showResult n = "Base 10: " <> T.pack (show n) <> "\n"
            <> "Base 3: " <> T.pack (showBTern (from10 n))
  where
    showBTern :: BTern -> String
    showBTern (BTern ts) = map showTrit ts

    showTrit :: Trit -> Char
    showTrit N = '-'
    showTrit Z = '0'
    showTrit P = '+'
  
