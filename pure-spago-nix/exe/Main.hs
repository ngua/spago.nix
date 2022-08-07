{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Dependencies (extractDependenciesIO)
import Prettyprinter.Util qualified
import System.Environment (getArgs)
import Types (prettyNixExpr)

-- | Extracts the dependencies declared in a Dhall file, converts them to a
-- Nix expression, then prints the result to stdout
main :: IO ()
main =
  getArgs >>= \case
    fp : _ ->
      Prettyprinter.Util.putDocW 80
        . prettyNixExpr
        =<< extractDependenciesIO fp
    _ -> putStrLn "Usage: pure-spago-nix <PATH>"
