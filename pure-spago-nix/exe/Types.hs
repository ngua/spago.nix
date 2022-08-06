module Types (
  SpagoDependencies (SpagoDependencies, imports),
  SpagoImport (SpagoImport, path, sha256),
  SpagoAddition (SpagoAddition, name, repo, version),
  emptyDependencies,
) where

import Data.Generics.Labels ()
import Data.Sequence (Seq)
import Data.Text (Text)
import Dhall.Crypto (SHA256Digest)
import GHC.Generics (Generic)

{- | The dependencies declared in `packages.dhall`. Includes Dhall imports
 as well as additional git dependencies
-}
data SpagoDependencies = SpagoDependencies
  { imports :: Seq SpagoImport
  , additions :: Seq SpagoAddition
  }
  deriving stock (Generic, Eq, Show)

emptyDependencies :: SpagoDependencies
emptyDependencies = SpagoDependencies mempty mempty

{- | A Dhall import, usually an upstream package set. We only care about the
 path and the hash
-}
data SpagoImport = SpagoImport
  { path :: Text
  , sha256 :: SHA256Digest
  }
  deriving stock (Generic, Eq, Show)

{- | An additional Spago dependency. We will check these against the sha256
 hash provided by the user
-}
data SpagoAddition = SpagoAddition
  { name :: Text
  , repo :: Text
  , version :: Text
  }
  deriving stock (Generic, Eq, Show)
