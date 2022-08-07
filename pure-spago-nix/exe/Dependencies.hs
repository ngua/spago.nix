{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

{- | Parses Spago-style `packages.dhall` files in a (very) crude way. Currently
 support imports with hashes (only official Purescript package sets) and
 records in `let`-bindings
-}
module Dependencies (
  extractDependenciesIO,
) where

import Control.Exception (Exception (displayException), IOException)
import Control.Lens (at, (%=), (?=), (^.), (^?), _head)
import Control.Monad (void, (<=<))
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.State.Strict (MonadState, execStateT)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Text.IO qualified
import Data.Text.Lens (packed)
import Data.Void (Void)
import Dhall.Core qualified as Dhall
import Dhall.Crypto qualified
import Dhall.Map qualified
import Dhall.Parser qualified
import Types (
  NixExpr (NixAttrSet, NixList, NixString),
  SpagoAddition (SpagoAddition, repo, version),
  SpagoDependencies (additions, imports),
  SpagoDependencyError (
    MissingImportHash,
    MissingRecordField,
    MissingUpstream,
    UnsupportedRecord,
    UnsupportedRemoteImport
  ),
  SpagoImport (SpagoImport, path, sha256),
  emptyDependencies,
 )

extractDependenciesIO :: FilePath -> IO NixExpr
extractDependenciesIO =
  liftEither . first toIOException . dependenciesToNix
    <=< liftEither . first toIOException
    <=< runExtractDependencies
    <=< liftEither . first (userError . show)
      . Dhall.Parser.exprFromText "(spago-dependencies)"
    <=< Data.Text.IO.readFile
  where
    toIOException :: forall (a :: Type). Exception a => a -> IOException
    toIOException = userError . displayException

dependenciesToNix ::
  forall (m :: Type -> Type).
  MonadError SpagoDependencyError m =>
  SpagoDependencies ->
  m NixExpr
dependenciesToNix deps = do
  -- Although we can extract any number of imports, we really only care about
  -- the first one (this should be an official Purescript package set)
  --
  -- In the future, we can add support for multiple remote imports
  upstream <-
    maybe (throwError MissingUpstream) pure $
      deps.imports ^? _head
  pure $
    NixAttrSet $
      Map.fromList
        [ ("upstream", NixList . pure $ upstreamToAttrSet upstream)
        , ("additions", NixAttrSet $ additionToAttrSet <$> deps.additions)
        ]
  where
    upstreamToAttrSet :: SpagoImport -> NixExpr
    upstreamToAttrSet imp =
      NixAttrSet $
        Map.fromList
          [ ("path", NixString imp.path)
          ,
            ( "sha256"
            , NixString $
                Dhall.Crypto.toString imp.sha256 ^. packed
            )
          ]

    additionToAttrSet :: SpagoAddition -> NixExpr
    additionToAttrSet add =
      NixAttrSet $
        Map.fromList
          [ ("repo", NixString add.repo)
          , ("version", NixString add.version)
          ]

runExtractDependencies ::
  forall (m :: Type -> Type).
  Monad m =>
  Dhall.Expr Dhall.Parser.Src Dhall.Import ->
  m (Either SpagoDependencyError SpagoDependencies)
runExtractDependencies =
  runExceptT
    . flip execStateT emptyDependencies
    . extractDependencies'
    . Dhall.denote

extractDependencies' ::
  forall (m :: Type -> Type).
  ( MonadState SpagoDependencies m
  , MonadError SpagoDependencyError m
  ) =>
  Dhall.Expr Void Dhall.Import ->
  m ()
extractDependencies' = \case
  Dhall.Let (Dhall.Binding {Dhall.value = v}) expr ->
    extractDependencies' v *> extractDependencies' expr
  Dhall.Embed
    (Dhall.Import (Dhall.ImportHashed msha256 (Dhall.Remote remote)) _) -> do
      sha256 <- maybe (throwError MissingImportHash) pure msha256
      path <- getRemotePath remote
      #imports %= (|> SpagoImport {sha256, path})
  Dhall.Embed (Dhall.Import {}) -> throwError UnsupportedRemoteImport
  Dhall.RecordLit m -> void . flip Dhall.Map.traverseWithKey m $
    \name record -> do
      addition <- getAdditionInfo record
      #additions . at name ?= addition
  _ -> pure ()

getAdditionInfo ::
  forall (m :: Type -> Type).
  MonadError SpagoDependencyError m =>
  Dhall.RecordField Void Dhall.Import ->
  m SpagoAddition
getAdditionInfo (Dhall.RecordField {Dhall.recordFieldValue = v}) = case v of
  Dhall.RecordLit m ->
    SpagoAddition
      <$> lookupField "repo" <*> lookupField "version"
    where
      lookupField :: Text -> m Text
      lookupField =
        getFieldValue
          <=< maybe (throwError MissingRecordField) pure
            . flip Dhall.Map.lookup m

      getFieldValue :: Dhall.RecordField Void Dhall.Import -> m Text
      getFieldValue (Dhall.RecordField {Dhall.recordFieldValue = v'}) =
        case v' of
          Dhall.TextLit (Dhall.Chunks [] t) -> pure t
          _ -> throwError UnsupportedRecord
  _ -> throwError UnsupportedRecord

getRemotePath ::
  forall (m :: Type -> Type).
  MonadError SpagoDependencyError m =>
  Dhall.URL ->
  m Text
-- We only care about the last part of the remote URL, e.g. `psc-0.14.5-20211116`
-- (`Directory.components` lists them backwards)
getRemotePath
  (Dhall.URL _ _ (Dhall.File (Dhall.Directory (x : _)) _) _ _) = pure x
getRemotePath _ = throwError UnsupportedRemoteImport
