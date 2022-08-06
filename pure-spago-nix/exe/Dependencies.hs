{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Parses Spago-style `packages.dhall` files in a (very) crude way. Currently
-- support imports with hashes (only official Purescript package sets) and
-- records in `let`-bindings
module Dependencies (
  gatherDependenciesIO,
  gatherDependencies,
) where

import Control.Lens (Identity (runIdentity), (%=))
import Control.Monad (void, (<=<))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState, execStateT)
import Data.Kind (Type)
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Text.IO qualified
import Data.Void (Void)
import Dhall.Core qualified as Dhall
import Dhall.Map qualified
import Dhall.Parser qualified
import System.Exit (die)
import Types (
  SpagoAddition (SpagoAddition, name, repo, version),
  SpagoDependencies,
  SpagoImport (SpagoImport, path, sha256),
  emptyDependencies,
 )

gatherDependenciesIO :: FilePath -> IO (Either Text SpagoDependencies)
gatherDependenciesIO =
  runGatherDependencies
    <=< parseOrExit
    <=< Data.Text.IO.readFile
  where
    parseOrExit :: Text -> IO (Dhall.Expr Dhall.Parser.Src Dhall.Import)
    parseOrExit =
      either (die . show) pure
        . Dhall.Parser.exprFromText "(spago-dependencies)"

gatherDependencies ::
  Dhall.Expr Dhall.Parser.Src Dhall.Import ->
  Either Text SpagoDependencies
gatherDependencies = runIdentity . runGatherDependencies

runGatherDependencies ::
  forall (m :: Type -> Type).
  Monad m =>
  Dhall.Expr Dhall.Parser.Src Dhall.Import ->
  m (Either Text SpagoDependencies)
runGatherDependencies =
  runExceptT
    . flip execStateT emptyDependencies
    . gatherDependencies'
    . Dhall.denote

gatherDependencies' ::
  forall (m :: Type -> Type).
  ( MonadState SpagoDependencies m
  , -- TODO use a better/specific error type
    MonadError Text m
  ) =>
  Dhall.Expr Void Dhall.Import ->
  m ()
gatherDependencies' = \case
  Dhall.Let (Dhall.Binding {Dhall.value = v}) expr ->
    gatherDependencies' v *> gatherDependencies' expr
  Dhall.Embed
    (Dhall.Import (Dhall.ImportHashed msha256 (Dhall.Remote remote)) _) -> do
      sha256 <- maybe (throwError "Imports must include hash") pure msha256
      path <- getRemotePath remote
      #imports %= (|> SpagoImport {sha256, path})
  Dhall.Embed (Dhall.Import {}) -> throwError "Unsupported import type"
  Dhall.RecordLit m -> void . flip Dhall.Map.traverseWithKey m $
    \name record -> do
      (repo, version) <- getAdditionInfo record
      #additions %= (|> SpagoAddition {name, repo, version})
  _ -> pure ()

getAdditionInfo ::
  forall (m :: Type -> Type).
  MonadError Text m =>
  Dhall.RecordField Void Dhall.Import ->
  m (Text, Text)
getAdditionInfo (Dhall.RecordField {Dhall.recordFieldValue = v}) = case v of
  Dhall.RecordLit m -> (,) <$> lookupField "repo" <*> lookupField "version"
    where
      lookupField :: Text -> m Text
      lookupField =
        getFieldValue
          <=< maybe (throwError "Missing record field") pure
            . flip Dhall.Map.lookup m

      getFieldValue :: Dhall.RecordField Void Dhall.Import -> m Text
      getFieldValue (Dhall.RecordField {Dhall.recordFieldValue = v'}) =
        case v' of
          Dhall.TextLit (Dhall.Chunks [] t) -> pure t
          _ -> throwError "Unsupported field type"
  _ -> throwError "Unsupported record type"

getRemotePath ::
  forall (m :: Type -> Type).
  MonadError Text m =>
  Dhall.URL ->
  m Text
-- We only care about the last part of the remote URL, e.g. `psc-0.14.5-20211116`
-- (`Directory.components` lists them backwards)
getRemotePath
  (Dhall.URL _ _ (Dhall.File (Dhall.Directory (x : _)) _) _ _) = pure x
getRemotePath _ = throwError "Unsupported remote"
