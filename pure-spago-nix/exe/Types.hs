{-# LANGUAGE LambdaCase #-}

module Types (
  SpagoDependencies (SpagoDependencies, imports),
  SpagoImport (SpagoImport, path, sha256),
  SpagoAddition (SpagoAddition, name, repo, version),
  emptyDependencies,
  NixExpr (..),
  prettyNixExpr,
) where

import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Dhall.Crypto (SHA256Digest)
import GHC.Exts (toList)
import GHC.Generics (Generic)
import Prettyprinter ((<+>))
import Prettyprinter qualified

{- | The dependencies declared in `packages.dhall`. Includes Dhall imports
 as well as additional git dependencies
-}
data SpagoDependencies = SpagoDependencies
  { imports :: Seq SpagoImport
  , additions :: Seq SpagoAddition
  }
  deriving stock (Show, Eq, Generic)

emptyDependencies :: SpagoDependencies
emptyDependencies = SpagoDependencies mempty mempty

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
  { name :: Text
  , repo :: Text
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
  | NixString Text
  deriving stock (Show, Eq, Generic)

prettyNixExpr :: forall (ann :: Type). NixExpr -> Prettyprinter.Doc ann
prettyNixExpr = \case
  NixString t -> Prettyprinter.dquotes $ Prettyprinter.pretty t
  NixList nexprs -> prettyElems ("[", "]") prettyNixExpr $ toList nexprs
  NixAttrSet attrs -> prettyElems ("{", "}") f $ Map.toList attrs
    where
      f :: (Text, NixExpr) -> Prettyprinter.Doc ann
      f (k, nexpr) =
        Prettyprinter.pretty k
          <+> "="
          <+> prettyNixExpr nexpr <> ";"

prettyElems ::
  forall (a :: Type) (ann :: Type).
  (Prettyprinter.Doc ann, Prettyprinter.Doc ann) ->
  (a -> Prettyprinter.Doc ann) ->
  [a] ->
  Prettyprinter.Doc ann
prettyElems (open, close) f xs =
  Prettyprinter.sep
    [ Prettyprinter.nest 2
        . Prettyprinter.sep
        $ [open] <> (f <$> xs)
    , close
    ]
