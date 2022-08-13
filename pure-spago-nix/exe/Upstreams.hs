{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- This module acts as a script to fetch a given Purescript upstream, then
-- calculate the sha256 for each entry in the Dhall configuration. The IO
-- action is basically a collection of various hacks, but since this is not
-- user-facing in any way, that's OK. This is easier than trying to write the
-- equivalent in Bash or Nix
module Upstreams (
  generateUpstreamIO,
) where

import Control.Concurrent.Async qualified as Async
import Control.Lens ((^.))
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.Char8 qualified as Lazy.Char8
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lens (unpacked)
import Data.Tuple (swap)
import Data.Void (Void)
import Dhall.Core qualified as Dhall
import Dhall.Map qualified
import Dhall.Parser qualified
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed qualified as Process.Typed

generateUpstreamIO :: Lazy.Char8.ByteString -> IO ()
generateUpstreamIO pkgset = do
  -- `http-client` could be used instead, but this is easier/more convenient
  raw <-
    checkExitCode
      =<< Process.Typed.readProcessStdout
        (Process.Typed.proc "wget" ["-qO-", mkUrl pkgset])
  dhall <-
    liftEither . bimap (userError . show) (Dhall.denote @_ @_ @Void)
      . Dhall.Parser.exprFromText "(upstream)"
      . Text.Encoding.decodeUtf8
      $ Lazy.Char8.toStrict raw
  withSha256 <- addSha256s dhall
  -- We can use `dhall-to-nix` since this does not need to run in a pure
  -- context
  nix <-
    checkExitCode
      =<< Process.Typed.readProcessStdout
        (Process.Typed.setStdin (dhallInput withSha256) "dhall-to-nix")
  Lazy.Char8.putStrLn nix
  where
    dhallInput ::
      Dhall.Expr Void Dhall.Import ->
      Process.Typed.StreamSpec 'Process.Typed.STInput ()
    dhallInput =
      Process.Typed.byteStringInput
        . Lazy.Char8.fromStrict
        . Text.Encoding.encodeUtf8
        . Dhall.pretty

    mkUrl :: Lazy.Char8.ByteString -> [Char]
    mkUrl (Lazy.Char8.unpack -> str) =
      mconcat
        [ "https://github.com/purescript/package-sets/releases/download/"
        , str
        , "/packages.dhall"
        ]

addSha256s ::
  Dhall.Expr Void Dhall.Import ->
  IO (Dhall.Expr Void Dhall.Import)
addSha256s = \case
  Dhall.RecordLit pkgs -> do
    fmap Dhall.RecordLit
      . Async.forConcurrently pkgs
      $ \pkg -> case pkg.recordFieldValue of
        Dhall.RecordLit r -> addSha256 pkg r
        expr -> throwOnExpr expr
  expr -> throwOnExpr expr

addSha256 ::
  Dhall.RecordField Void Dhall.Import ->
  Dhall.Map.Map Text (Dhall.RecordField Void Dhall.Import) ->
  IO (Dhall.RecordField Void Dhall.Import)
addSha256 pkg r = do
  repo <- extractText =<< lookupOrThrow "repo" r
  rev <- extractText =<< lookupOrThrow "version" r
  prefetched <-
    checkExitCode
      =<< Process.Typed.readProcessStdout
        ( Process.Typed.proc
            "nix-prefetch-git"
            ["--quiet", repo ^. unpacked, "--rev", rev ^. unpacked]
        )
  -- Also easier than using Aeson given the shape of the output (Aeson would
  -- parse `prefetched` as a string instead of an object)
  sha256 <-
    checkExitCode
      =<< ( Process.Typed.readProcessStdout
              . Process.Typed.setStdin
                (Process.Typed.byteStringInput prefetched)
              $ Process.Typed.proc
                "jq"
                [ "-r"
                , ".sha256"
                ]
          )
  pure $
    pkg
      { Dhall.recordFieldValue =
          Dhall.RecordLit $
            Dhall.Map.insert
              "sha256"
              ( makeTextField
                  ( Text.Encoding.decodeUtf8 $ Lazy.Char8.toStrict sha256
                  )
              )
              r
      }
  where
    extractText :: Dhall.RecordField Void Dhall.Import -> IO Text
    extractText fld = case fld.recordFieldValue of
      Dhall.TextLit (Dhall.Chunks [] t) -> pure t
      expr -> throwOnExpr expr

    makeTextField :: Text -> Dhall.RecordField Void Dhall.Import
    makeTextField = Dhall.makeRecordField . Dhall.TextLit . Dhall.Chunks []

    lookupOrThrow :: Text -> Dhall.Map.Map Text a -> IO a
    lookupOrThrow k = maybe (throwOnMissingKey k) pure . Dhall.Map.lookup k

    throwOnMissingKey :: forall (a :: Type). Text -> IO a
    throwOnMissingKey = throwUserError . ("Missing key: " <>) . (^. unpacked)

checkExitCode :: (ExitCode, Lazy.Char8.ByteString) -> IO Lazy.Char8.ByteString
checkExitCode = uncurry check . swap
  where
    check :: Lazy.Char8.ByteString -> ExitCode -> IO Lazy.Char8.ByteString
    check stdout = \case
      ExitFailure code ->
        throwError . userError $
          "Process failed with " <> show code
      ExitSuccess -> pure stdout

throwOnExpr :: forall (a :: Type). Dhall.Expr Void Dhall.Import -> IO a
throwOnExpr = throwUserError . ("Unsupported Dhall `Expr`: " <>) . show

throwUserError :: forall (a :: Type). [Char] -> IO a
throwUserError = throwError . userError
