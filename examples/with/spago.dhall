{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "spago-nix-simple-example"
, dependencies = [ "console", "effect", "prelude", "bigints" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
