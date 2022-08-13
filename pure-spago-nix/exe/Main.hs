{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Dependencies (extractDependenciesIO)
import Options.Applicative qualified as Options
import Prettyprinter.Util qualified
import Types (
  Options (ExtractDependencies, GenerateUpstream),
  options,
  prettyNixExpr,
 )

main :: IO ()
main = run =<< Options.customExecParser prefs options
  where
    prefs :: Options.ParserPrefs
    prefs =
      Options.prefs $
        Options.showHelpOnEmpty <> Options.showHelpOnError

run :: Options -> IO ()
run = \case
  -- Extracts the dependencies declared in a Dhall config file, converts them
  -- to a Nix expression, then prints the result to stdout
  ExtractDependencies fp ->
    Prettyprinter.Util.putDocW 80 . prettyNixExpr
      =<< extractDependenciesIO fp
  -- TODO
  GenerateUpstream _ -> pure ()
