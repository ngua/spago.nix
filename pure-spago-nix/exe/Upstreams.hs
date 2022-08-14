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
import Control.Monad.Error.Class (
  MonadError (throwError),
  liftEither,
 )
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap, first)
import Data.ByteString.Lazy.Char8 qualified as Lazy.Char8
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lens (unpacked)
import Data.Tuple (swap)
import Data.Void (Void)
import Dhall.Core qualified as Dhall
import Dhall.Map qualified
import Dhall.Parser qualified
import Prettyprinter qualified
import Prettyprinter.Util qualified
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed qualified as Process.Typed
import Types (
  NixExpr (
    NixAttrSet,
    NixFunApp,
    NixString
  ),
  prettyNixExpr,
 )

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
  nix <- toDrvs dhall
  Prettyprinter.Util.putDocW 80 $
    Prettyprinter.vsep
      [ "/* This file was generated by pure-spago-nix do NOT edit by hand! */"
      , "{ pkgs, ... }:"
      , nix
      ]
  where
    mkUrl :: Lazy.Char8.ByteString -> [Char]
    mkUrl (Lazy.Char8.unpack -> str) =
      mconcat
        [ "https://github.com/purescript/package-sets/releases/download/"
        , str
        , "/packages.dhall"
        ]

toDrvs ::
  forall (ann :: Type).
  Dhall.Expr Void Dhall.Import ->
  IO (Prettyprinter.Doc ann)
toDrvs =
  fmap (prettyNixExpr . NixAttrSet . mconcat) . \case
    Dhall.RecordLit pkgs ->
      Async.forConcurrently (Dhall.Map.toList pkgs) $ \(name, pkg) ->
        case pkg.recordFieldValue of
          Dhall.RecordLit r -> do
            repo <- extractText =<< lookupOrThrow "repo" r
            version <- extractText =<< lookupOrThrow "version" r

            prefetched <-
              checkExitCode
                =<< Process.Typed.readProcessStdout
                  ( Process.Typed.proc
                      "nix-prefetch-git"
                      [ "--quiet"
                      , repo ^. unpacked
                      , "--rev"
                      , version ^. unpacked
                      ]
                  )
            p <-
              liftEither . first userError
                . Aeson.eitherDecode @NixPrefetched
                $ prefetched

            pure $
              Map.fromList
                [
                  ( name
                  , NixFunApp "pkgs.stdenv.mkDerivation" . NixAttrSet $
                      Map.fromList
                        [ ("version", NixString version)
                        , ("phases", NixString "installPhase")
                        , ("installPhase", NixString "ln -s $src $out")
                        ,
                          ( "src"
                          , NixFunApp "pkgs.fetchGit" . NixAttrSet $
                              Map.fromList
                                [ ("url", NixString repo)
                                , ("sha256", NixString p.sha256)
                                , ("rev", NixString p.rev)
                                ]
                          )
                        ]
                  )
                ]
          expr -> throwOnExpr expr
    expr -> throwOnExpr expr
  where
    lookupOrThrow :: Text -> Dhall.Map.Map Text a -> IO a
    lookupOrThrow k = maybe (throwOnMissingKey k) pure . Dhall.Map.lookup k

    throwOnMissingKey :: forall (a :: Type). Text -> IO a
    throwOnMissingKey = throwUserError . ("Missing key: " <>) . (^. unpacked)

extractText :: Dhall.RecordField Void Dhall.Import -> IO Text
extractText fld = case fld.recordFieldValue of
  Dhall.TextLit (Dhall.Chunks [] t) -> pure t
  expr -> throwOnExpr expr

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

-- Helper for parsing JSON. Gets the relevant info from the output of
-- `nix-prefetch-git`
data NixPrefetched = NixPrefetched
  { sha256 :: Text
  , rev :: Text
  }
  deriving stock (Show)

instance Aeson.FromJSON NixPrefetched where
  parseJSON = Aeson.withObject "NixPrefetched" $ \o ->
    NixPrefetched <$> (o .: "sha256") <*> (o .: "rev")
