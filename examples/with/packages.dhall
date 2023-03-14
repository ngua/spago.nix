let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220527/packages.dhall
        sha256:0000000000000000000000000000000000000000000000000000000000000000

in  upstream
  with properties =
    { dependencies = [ "prelude", "console" ]
    , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
    , version = "v0.2.0"
    }
  with lattice =
    { dependencies = [ "prelude", "console", "properties" ]
    , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
    , version = "v0.3.0"
    }
