module Main where

import Prelude

import Data.BigInt as BigInt
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Hello spago.nix"
  log $ show $ BigInt.fromInt 10
