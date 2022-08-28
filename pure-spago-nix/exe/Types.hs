{-# LANGUAGE LambdaCase #-}

module Types (
  Options (ExtractDependencies, GenerateUpstream),
  NixExpr (NixAttrSet, NixList, NixString, NixFunApp),
  NixStringLineSpan (Multi, Single),
  SpagoAddition (SpagoAddition, repo, version),
  SpagoImport (SpagoImport, path, sha256),
  SpagoDependencies (
    SpagoDependencies,
    imports,
    additions,
    additionsDhall
  ),
  SpagoDependencyError (
    MissingUpstream,
    MissingImportHash,
    MissingRecordField,
    ExtraRecordField,
    UnsupportedRecord,
    UnsupportedRemoteImport
  ),
  emptyDependencies,
  prettyNixExpr,
  options,
  nixString,
  nixAttrSet,
) where

import Control.Exception (Exception (displayException))
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Dhall.Crypto (SHA256Digest)
import GHC.Exts (toList)
import GHC.Generics (Generic)
import Options.Applicative qualified as Options
import Prettyprinter ((<+>))
import Prettyprinter qualified

data Options
  = ExtractDependencies FilePath
  | GenerateUpstream FilePath
  deriving stock (Show, Eq, Generic)

options :: Options.ParserInfo Options
options =
  Options.info (Options.helper <*> optionsP) $
    Options.fullDesc
      <> Options.progDesc "Generate Nix packages from Spago configuration"
      <> Options.header "pure-spago-nix"

optionsP :: Options.Parser Options
optionsP =
  Options.hsubparser $
    Options.command
      "extract"
      ( Options.info
          (Options.helper <*> extractCommand)
          (Options.progDesc "Extract dependencies from `packages.dhall`")
      )
      <> Options.command
        "generate"
        ( Options.info
            (Options.helper <*> generateCommand)
            (Options.progDesc "Generate package set from Purescript upstream")
        )

generateCommand :: Options.Parser Options
generateCommand =
  GenerateUpstream
    <$> Options.argument Options.str (Options.metavar "PACKAGE-SET")

extractCommand :: Options.Parser Options
extractCommand =
  ExtractDependencies
    <$> Options.argument Options.str (Options.metavar "FILE")

data SpagoDependencyError
  = -- | The user hasn't specified an upstream package set
    MissingUpstream
  | -- | The import hash wasn't provided
    MissingImportHash
  | -- | A record field is missing for a 'SpagoAddition'
    MissingRecordField
  | -- | An extra record field has been provided for a 'SpagoAddition'
    ExtraRecordField
  | -- | The record doesn't conform to the expected 'SpagoAddition' format
    UnsupportedRecord
  | -- | The remote does not conform to the standard Spago package set format
    UnsupportedRemoteImport
  deriving stock (Show, Eq, Generic)

instance Exception SpagoDependencyError where
  displayException = \case
    MissingUpstream -> "No upstream specified"
    MissingImportHash -> "No sha256 hash has been provided for the import"
    MissingRecordField -> "`repo` or `version` missing"
    ExtraRecordField -> "Unexpected record field provided for `SpagoAddition`"
    UnsupportedRecord -> "Record does not conform to `SpagoAddition` format"
    UnsupportedRemoteImport ->
      "Only official Purescript package sets are supported"

{- | The dependencies declared in `packages.dhall`. Includes Dhall imports
 as well as additional git dependencies
-}
data SpagoDependencies = SpagoDependencies
  { imports :: Seq SpagoImport
  , additions :: Map Text SpagoAddition
  , additionsDhall :: Text
  -- ^ The actual Dhall expression containing the additional dependencies; this
  -- is needed to use `spago` to list the exact dependencies
  }
  deriving stock (Show, Eq, Generic)

emptyDependencies :: SpagoDependencies
emptyDependencies = SpagoDependencies mempty mempty mempty

{- | A Dhall import, usually an upstream package set. We only care about the
 path and the hash
-}
data SpagoImport = SpagoImport
  { path :: Text
  , sha256 :: SHA256Digest
  }
  deriving stock (Show, Eq, Generic)

{- | An additional Spago dependency. We will check these against the sha256
 hash provided by the user
-}
data SpagoAddition = SpagoAddition
  { repo :: Text
  , version :: Text
  }
  deriving stock (Show, Eq, Generic)

{- | The `hnix` package is currently broken on `nixpkgs`, so we need to define
 our own (extremely limited) set of Nix expressions. We only care about creating
very primitive literals -- just lists and attribute sets
-}
data NixExpr
  = NixList (Seq NixExpr)
  | NixAttrSet (Map Text NixExpr)
  | -- | Boolean argument indicates if this is a multi-line string
    NixString NixStringLineSpan Text
  | NixFunApp Text NixExpr
  deriving stock (Show, Eq, Generic)

data NixStringLineSpan = Multi | Single
  deriving stock (Show, Eq, Generic)

prettyNixExpr :: forall (ann :: Type). NixExpr -> Prettyprinter.Doc ann
prettyNixExpr = \case
  NixFunApp name arg -> Prettyprinter.pretty name <+> prettyNixExpr arg
  NixString Single t -> qtext t
  NixString Multi t ->
    Prettyprinter.sep
      [ Prettyprinter.nest 2 $
          Prettyprinter.vsep
            [ "''"
            , Prettyprinter.pretty t
            ]
      , "''"
      ]
  NixList nexprs -> prettyElems ("[", "]") prettyNixExpr $ toList nexprs
  NixAttrSet attrs -> prettyElems ("{", "}") f $ Map.toList attrs
    where
      f :: (Text, NixExpr) -> Prettyprinter.Doc ann
      f (k, nexpr) =
        qtext k -- It's safer to always quote the text
          <+> Prettyprinter.equals
          <+> prettyNixExpr nexpr <> Prettyprinter.semi

prettyElems ::
  forall (a :: Type) (ann :: Type).
  (Prettyprinter.Doc ann, Prettyprinter.Doc ann) ->
  (a -> Prettyprinter.Doc ann) ->
  [a] ->
  Prettyprinter.Doc ann
prettyElems (open, close) f xs =
  Prettyprinter.sep
    [ Prettyprinter.nest 2 . Prettyprinter.sep $ open : (f <$> xs)
    , close
    ]

qtext :: forall (ann :: Type). Text -> Prettyprinter.Doc ann
qtext = Prettyprinter.dquotes . Prettyprinter.pretty

nixString :: Text -> NixExpr
nixString = NixString Single

nixAttrSet :: [(Text, NixExpr)] -> NixExpr
nixAttrSet = NixAttrSet . Map.fromList
