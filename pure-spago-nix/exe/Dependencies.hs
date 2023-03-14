{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

{- | Parses Spago-style `packages.dhall` files in a (very) crude way. Currently
 support imports with hashes (only official Purescript package sets) and
 records in `let`-bindings
-}
module Dependencies
  ( extractDependenciesIO
  ) where

import Control.Exception (Exception (displayException), IOException)
import Control.Lens
  ( at
  , (%=)
  , (?=)
  , (^.)
  , (^?)
  , _head
  )
import Control.Monad (void, (<=<))
import Control.Monad.Except
  ( MonadError (throwError)
  , liftEither
  , runExceptT
  )
import Control.Monad.State.Strict (MonadState, execStateT)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Data.Text.Lens
import Data.Void (Void)
import Dhall.Core (WithComponent (WithLabel))
import Dhall.Core qualified as Dhall
import Dhall.Crypto qualified
import Dhall.Map qualified
import Dhall.Parser qualified
import Dhall.Pretty qualified
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified
import Types
  ( NixExpr (NixAttrSet, NixString)
  , NixStringLineSpan (Multi)
  , SpagoAddition (SpagoAddition, repo, version)
  , SpagoDependencies (additions, additionsDhall, imports)
  , SpagoDependencyError
    ( MissingImportHash
    , MissingRecordField
    , MissingUpstream
    , UnsupportedRecord
    , UnsupportedRemoteImport
    )
  , SpagoImport (SpagoImport, path, sha256)
  , emptyDependencies
  , nixAttrSet
  , nixString
  )

extractDependenciesIO :: FilePath -> IO NixExpr
extractDependenciesIO =
  liftEither . first toIOException . dependenciesToNix
    <=< liftEither . first toIOException
    <=< runExtractDependencies
    <=< liftEither
      . first (userError . show)
      . Dhall.Parser.exprFromText "(spago-dependencies)"
    <=< Text.IO.readFile
 where
  toIOException :: forall (a :: Type). Exception a => a -> IOException
  toIOException = userError . displayException

dependenciesToNix
  :: forall (m :: Type -> Type)
   . MonadError SpagoDependencyError m
  => SpagoDependencies
  -> m NixExpr
dependenciesToNix deps = do
  -- Although we can extract any number of imports, we really only care about
  -- the first one (this should be an official Purescript package set)
  --
  -- In the future, we can add support for multiple remote imports
  upstream <-
    maybe (throwError MissingUpstream) pure $
      deps.imports ^? _head
  pure $
    nixAttrSet
      [ ("upstream", upstreamToAttrSet upstream)
      , ("additions", NixAttrSet $ additionToAttrSet <$> deps.additions)
      ,
        ( "additions-dhall"
        , NixString Multi . renderExpr $ Dhall.RecordLit deps.additionsDhall
        )
      ]
 where
  upstreamToAttrSet :: SpagoImport -> NixExpr
  upstreamToAttrSet imp =
    nixAttrSet
      [ ("path", nixString imp.path)
      ,
        ( "sha256"
        , nixString $
            Dhall.Crypto.toString imp.sha256 ^. packed
        )
      ]

  additionToAttrSet :: SpagoAddition -> NixExpr
  additionToAttrSet add =
    nixAttrSet
      [ ("repo", nixString add.repo)
      , ("version", nixString add.version)
      ]

runExtractDependencies
  :: forall (m :: Type -> Type)
   . Monad m
  => Dhall.Expr Dhall.Parser.Src Dhall.Import
  -> m (Either SpagoDependencyError SpagoDependencies)
runExtractDependencies =
  runExceptT
    . flip execStateT emptyDependencies
    . extractDependencies'
    . Dhall.denote

extractDependencies'
  :: forall (m :: Type -> Type)
   . ( MonadState SpagoDependencies m
     , MonadError SpagoDependencyError m
     )
  => Dhall.Expr Void Dhall.Import
  -> m ()
extractDependencies' = \case
  Dhall.Let (Dhall.Binding {Dhall.value = v}) expr ->
    extractDependencies' v *> extractDependencies' expr
  Dhall.With expr (WithLabel name :| _) (Dhall.RecordLit r) -> do
    extractDependencies' expr
    (#additions . at name ?=) =<< getAdditionInfo r
    #additionsDhall %= addDhallAddition name r
  Dhall.Embed
    (Dhall.Import (Dhall.ImportHashed msha256 (Dhall.Remote remote)) _) -> do
      sha256 <- maybe (throwError MissingImportHash) pure msha256
      path <- getRemotePath remote
      #imports %= (|> SpagoImport {sha256, path})
  Dhall.Embed (Dhall.Import {}) -> throwError UnsupportedRemoteImport
  Dhall.RecordLit m -> do
    void . flip Dhall.Map.traverseWithKey m $ \name r ->
      getNestedRecordList r >>= \nr -> do
        (#additions . at name ?=) =<< getAdditionInfo nr
        #additionsDhall %= addDhallAddition name nr
  _ -> pure ()

addDhallAddition
  :: Text
  -> Dhall.Map.Map Text (Dhall.RecordField Void Dhall.Import)
  -> Dhall.Map.Map Text (Dhall.RecordField Void Dhall.Import)
  -> Dhall.Map.Map Text (Dhall.RecordField Void Dhall.Import)
addDhallAddition name r =
  Dhall.Map.insert
    name
    (Dhall.RecordField Nothing (Dhall.RecordLit r) Nothing Nothing)

renderExpr :: Dhall.Expr Void Dhall.Import -> Text
renderExpr =
  Prettyprinter.Render.Text.renderStrict
    . Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
    . Dhall.Pretty.prettyExpr

getNestedRecordList
  :: forall (m :: Type -> Type)
   . MonadError SpagoDependencyError m
  => Dhall.RecordField Void Dhall.Import
  -> m (Dhall.Map.Map Text (Dhall.RecordField Void Dhall.Import))
getNestedRecordList = \case
  Dhall.RecordField _ (Dhall.RecordLit m) _ _ -> pure m
  _ -> throwError UnsupportedRecord

getAdditionInfo
  :: forall (m :: Type -> Type)
   . MonadError SpagoDependencyError m
  => Dhall.Map.Map Text (Dhall.RecordField Void Dhall.Import)
  -> m SpagoAddition
getAdditionInfo m =
  SpagoAddition
    <$> lookupField "repo"
    <*> lookupField "version"
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

getRemotePath
  :: forall (m :: Type -> Type)
   . MonadError SpagoDependencyError m
  => Dhall.URL
  -> m Text
-- We only care about the last part of the remote URL, e.g. `psc-0.14.5-20211116`
-- (`Directory.components` lists them backwards)
getRemotePath
  (Dhall.URL _ _ (Dhall.File (Dhall.Directory (x : _)) _) _ _) = pure x
getRemotePath _ = throwError UnsupportedRemoteImport
