{ abides =
  { dependencies = [ "enums", "foldable-traversable" ]
  , repo = "https://github.com/athanclark/purescript-abides.git"
  , version = "v0.0.1"
  }
, ace =
  { dependencies =
    [ "arrays"
    , "effect"
    , "foreign"
    , "nullable"
    , "prelude"
    , "web-html"
    , "web-uievents"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-ace.git"
  , version = "v8.0.0"
  }
, aff =
  { dependencies =
    [ "datetime"
    , "effect"
    , "exceptions"
    , "functions"
    , "parallel"
    , "transformers"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-aff.git"
  , version = "v6.0.0"
  }
, aff-bus =
  { dependencies = [ "avar", "effect", "prelude" ]
  , repo = "https://github.com/purescript-contrib/purescript-aff-bus.git"
  , version = "v5.0.0"
  }
, aff-coroutines =
  { dependencies = [ "aff", "avar", "coroutines", "effect" ]
  , repo = "https://github.com/purescript-contrib/purescript-aff-coroutines.git"
  , version = "v8.0.0"
  }
, aff-promise =
  { dependencies = [ "aff", "foreign" ]
  , repo = "https://github.com/nwolverson/purescript-aff-promise.git"
  , version = "v2.1.0"
  }
, aff-retry =
  { dependencies =
    [ "psci-support"
    , "console"
    , "aff"
    , "datetime"
    , "prelude"
    , "random"
    , "transformers"
    , "exceptions"
    , "test-unit"
    ]
  , repo = "https://github.com/Unisay/purescript-aff-retry.git"
  , version = "v1.2.1"
  }
, affjax =
  { dependencies =
    [ "aff"
    , "argonaut-core"
    , "arraybuffer-types"
    , "foreign"
    , "form-urlencoded"
    , "http-methods"
    , "integers"
    , "math"
    , "media-types"
    , "nullable"
    , "refs"
    , "unsafe-coerce"
    , "web-xhr"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-affjax.git"
  , version = "v12.0.0"
  }
, ansi =
  { dependencies = [ "foldable-traversable", "lists", "strings" ]
  , repo = "https://github.com/hdgarrood/purescript-ansi.git"
  , version = "v6.0.0"
  }
, argonaut =
  { dependencies = [ "argonaut-codecs", "argonaut-core", "argonaut-traversals" ]
  , repo = "https://github.com/purescript-contrib/purescript-argonaut.git"
  , version = "v8.0.0"
  }
, argonaut-codecs =
  { dependencies =
    [ "argonaut-core"
    , "arrays"
    , "effect"
    , "foreign-object"
    , "identity"
    , "integers"
    , "maybe"
    , "nonempty"
    , "ordered-collections"
    , "prelude"
    , "record"
    ]
  , repo =
      "https://github.com/purescript-contrib/purescript-argonaut-codecs.git"
  , version = "v8.0.0"
  }
, argonaut-core =
  { dependencies =
    [ "arrays"
    , "control"
    , "either"
    , "foreign-object"
    , "functions"
    , "gen"
    , "maybe"
    , "nonempty"
    , "prelude"
    , "strings"
    , "tailrec"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-argonaut-core.git"
  , version = "v6.0.0"
  }
, argonaut-generic =
  { dependencies = [ "argonaut-codecs", "argonaut-core", "prelude", "record" ]
  , repo =
      "https://github.com/purescript-contrib/purescript-argonaut-generic.git"
  , version = "v7.0.0"
  }
, argonaut-traversals =
  { dependencies = [ "argonaut-codecs", "argonaut-core", "profunctor-lenses" ]
  , repo =
      "https://github.com/purescript-contrib/purescript-argonaut-traversals.git"
  , version = "v9.0.0"
  }
, arraybuffer-types =
  { dependencies = [] : List Text
  , repo =
      "https://github.com/purescript-contrib/purescript-arraybuffer-types.git"
  , version = "v3.0.0"
  }
, arrays =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "foldable-traversable"
    , "maybe"
    , "nonempty"
    , "partial"
    , "prelude"
    , "st"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript/purescript-arrays.git"
  , version = "v6.0.0"
  }
, arrays-zipper =
  { dependencies = [ "arrays", "control", "quickcheck" ]
  , repo = "https://github.com/JordanMartinez/purescript-arrays-zipper.git"
  , version = "v1.1.1"
  }
, `assert` =
  { dependencies = [ "console", "effect", "prelude" ]
  , repo = "https://github.com/purescript/purescript-assert.git"
  , version = "v5.0.0"
  }
, avar =
  { dependencies =
    [ "aff", "effect", "either", "exceptions", "functions", "maybe" ]
  , repo = "https://github.com/purescript-contrib/purescript-avar.git"
  , version = "v4.0.0"
  }
, basic-auth =
  { dependencies = [ "crypto", "node-http" ]
  , repo = "https://github.com/oreshinya/purescript-basic-auth.git"
  , version = "v2.0.0"
  }
, bifunctors =
  { dependencies = [ "const", "either", "newtype", "prelude", "tuples" ]
  , repo = "https://github.com/purescript/purescript-bifunctors.git"
  , version = "v5.0.0"
  }
, bigints =
  { dependencies = [ "integers", "maybe", "strings" ]
  , repo = "https://github.com/sharkdp/purescript-bigints.git"
  , version = "v5.0.0"
  }
, bip39 =
  { dependencies = [ "arraybuffer-types", "nullable" ]
  , repo = "https://github.com/athanclark/purescript-bip39.git"
  , version = "v1.0.1"
  }
, biscotti-cookie =
  { dependencies =
    [ "datetime"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "formatters"
    , "gen"
    , "newtype"
    , "now"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "record"
    , "string-parsers"
    , "strings"
    , "test-unit"
    ]
  , repo = "https://github.com/drewolson/purescript-biscotti-cookie.git"
  , version = "v0.3.0"
  }
, bucketchain =
  { dependencies =
    [ "aff", "console", "node-http", "node-streams", "transformers" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain.git"
  , version = "v0.4.0"
  }
, bucketchain-basic-auth =
  { dependencies = [ "basic-auth", "bucketchain" ]
  , repo =
      "https://github.com/Bucketchain/purescript-bucketchain-basic-auth.git"
  , version = "v0.3.0"
  }
, bucketchain-conditional =
  { dependencies = [ "bucketchain", "js-date" ]
  , repo =
      "https://github.com/Bucketchain/purescript-bucketchain-conditional.git"
  , version = "v0.3.0"
  }
, bucketchain-cors =
  { dependencies = [ "bucketchain", "bucketchain-header-utils", "http-methods" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-cors.git"
  , version = "v0.4.0"
  }
, bucketchain-csrf =
  { dependencies = [ "bucketchain" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-csrf.git"
  , version = "v0.3.0"
  }
, bucketchain-header-utils =
  { dependencies = [ "bucketchain" ]
  , repo =
      "https://github.com/Bucketchain/purescript-bucketchain-header-utils.git"
  , version = "v0.4.0"
  }
, bucketchain-health =
  { dependencies = [ "bucketchain" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-health.git"
  , version = "v0.3.0"
  }
, bucketchain-history-api-fallback =
  { dependencies = [ "bucketchain" ]
  , repo =
      "https://github.com/Bucketchain/purescript-bucketchain-history-api-fallback.git"
  , version = "v0.4.0"
  }
, bucketchain-logger =
  { dependencies = [ "bucketchain", "js-date", "node-process" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-logger.git"
  , version = "v0.4.0"
  }
, bucketchain-secure =
  { dependencies = [ "bucketchain" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-secure.git"
  , version = "v0.1.0"
  }
, bucketchain-sslify =
  { dependencies = [ "bucketchain" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-sslify.git"
  , version = "v0.3.0"
  }
, bucketchain-static =
  { dependencies = [ "bucketchain", "node-fs-aff" ]
  , repo = "https://github.com/Bucketchain/purescript-bucketchain-static.git"
  , version = "v0.4.0"
  }
, bytestrings =
  { dependencies =
    [ "arrays"
    , "effect"
    , "exceptions"
    , "foldable-traversable"
    , "integers"
    , "leibniz"
    , "maybe"
    , "newtype"
    , "node-buffer"
    , "prelude"
    , "quickcheck"
    , "quotient"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/rightfold/purescript-bytestrings.git"
  , version = "v8.0.0"
  }
, call-by-name =
  { dependencies = [ "unsafe-coerce", "lazy", "maybe", "either", "control" ]
  , repo = "https://github.com/natefaubion/purescript-call-by-name.git"
  , version = "v3.0.0"
  }
, canvas =
  { dependencies =
    [ "arraybuffer-types", "effect", "exceptions", "functions", "maybe" ]
  , repo = "https://github.com/purescript-web/purescript-canvas.git"
  , version = "v5.0.0"
  }
, cartesian =
  { dependencies = [ "console", "effect", "integers", "psci-support" ]
  , repo = "https://github.com/Ebmtranceboy/purescript-cartesian.git"
  , version = "v1.0.4"
  }
, catenable-lists =
  { dependencies =
    [ "control"
    , "foldable-traversable"
    , "lists"
    , "maybe"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-catenable-lists.git"
  , version = "v6.0.0"
  }
, cheerio =
  { dependencies = [ "console", "effect", "functions", "prelude", "test-unit" ]
  , repo = "https://github.com/icyrockcom/purescript-cheerio.git"
  , version = "v0.2.3"
  }
, clipboardy =
  { dependencies = [ "aff", "aff-promise", "effect" ]
  , repo = "https://github.com/hrajchert/purescript-clipboardy.git"
  , version = "v1.0.3"
  }
, codec =
  { dependencies = [ "transformers", "profunctor" ]
  , repo = "https://github.com/garyb/purescript-codec.git"
  , version = "v4.0.0"
  }
, codec-argonaut =
  { dependencies =
    [ "argonaut-core"
    , "codec"
    , "ordered-collections"
    , "type-equality"
    , "variant"
    ]
  , repo = "https://github.com/garyb/purescript-codec-argonaut.git"
  , version = "v8.0.0"
  }
, colors =
  { dependencies = [ "arrays", "integers", "lists", "partial", "strings" ]
  , repo = "https://github.com/sharkdp/purescript-colors.git"
  , version = "v5.0.0"
  }
, concur-core =
  { dependencies =
    [ "aff"
    , "arrays"
    , "avar"
    , "console"
    , "foldable-traversable"
    , "free"
    , "nonempty"
    , "profunctor-lenses"
    , "tailrec"
    ]
  , repo = "https://github.com/purescript-concur/purescript-concur-core.git"
  , version = "v0.4.2"
  }
, concur-react =
  { dependencies =
    [ "aff"
    , "arrays"
    , "avar"
    , "concur-core"
    , "console"
    , "foldable-traversable"
    , "free"
    , "nonempty"
    , "react"
    , "react-dom"
    , "tailrec"
    , "web-dom"
    , "web-html"
    ]
  , repo = "https://github.com/purescript-concur/purescript-concur-react.git"
  , version = "v0.4.2"
  }
, concurrent-queues =
  { dependencies = [ "aff", "avar", "effect" ]
  , repo =
      "https://github.com/purescript-contrib/purescript-concurrent-queues.git"
  , version = "v2.0.0"
  }
, console =
  { dependencies = [ "effect", "prelude" ]
  , repo = "https://github.com/purescript/purescript-console.git"
  , version = "v5.0.0"
  }
, const =
  { dependencies = [ "invariant", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-const.git"
  , version = "v5.0.0"
  }
, contravariant =
  { dependencies = [ "const", "either", "newtype", "prelude", "tuples" ]
  , repo = "https://github.com/purescript/purescript-contravariant.git"
  , version = "v5.0.0"
  }
, control =
  { dependencies = [ "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-control.git"
  , version = "v5.0.0"
  }
, coroutines =
  { dependencies = [ "freet", "parallel", "profunctor" ]
  , repo = "https://github.com/purescript-contrib/purescript-coroutines.git"
  , version = "v6.0.0"
  }
, crypto =
  { dependencies = [ "node-buffer" ]
  , repo = "https://github.com/oreshinya/purescript-crypto.git"
  , version = "v3.0.0"
  }
, cssom =
  { dependencies = [ "effect" ]
  , repo = "https://github.com/danieljharvey/purescript-cssom.git"
  , version = "v0.0.2"
  }
, datetime =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "functions"
    , "gen"
    , "integers"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "ordered-collections"
    , "partial"
    , "prelude"
    , "tuples"
    ]
  , repo = "https://github.com/purescript/purescript-datetime.git"
  , version = "v5.0.0"
  }
, debug =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/garyb/purescript-debug.git"
  , version = "v4.0.1"
  }
, debuggest =
  { dependencies = [ "prelude", "unsafe-coerce" ]
  , repo = "https://github.com/ursi/purescript-debuggest.git"
  , version = "v0.4.0"
  }
, decimals =
  { dependencies = [ "maybe" ]
  , repo = "https://github.com/sharkdp/purescript-decimals.git"
  , version = "v6.0.0"
  }
, distributive =
  { dependencies =
    [ "identity", "newtype", "prelude", "tuples", "type-equality" ]
  , repo = "https://github.com/purescript/purescript-distributive.git"
  , version = "v5.0.0"
  }
, dom-filereader =
  { dependencies = [ "aff", "arraybuffer-types", "web-file", "web-html" ]
  , repo = "https://github.com/nwolverson/purescript-dom-filereader.git"
  , version = "v5.0.0"
  }
, dom-indexed =
  { dependencies =
    [ "media-types", "prelude", "web-clipboard", "web-touchevents" ]
  , repo = "https://github.com/purescript-halogen/purescript-dom-indexed.git"
  , version = "v7.0.0"
  }
, downloadjs =
  { dependencies =
    [ "arraybuffer-types"
    , "console"
    , "effect"
    , "foreign"
    , "psci-support"
    , "web-file"
    ]
  , repo = "https://github.com/chekoopa/purescript-downloadjs.git"
  , version = "v1.0.0"
  }
, drawing =
  { dependencies =
    [ "canvas", "colors", "integers", "lists", "math", "prelude" ]
  , repo = "https://github.com/paf31/purescript-drawing.git"
  , version = "v4.0.0"
  }
, dynamic-buffer =
  { dependencies = [ "arraybuffer-types", "effect", "refs" ]
  , repo = "https://github.com/kritzcreek/purescript-dynamic-buffer.git"
  , version = "v2.0.0"
  }
, easy-ffi =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/pelotom/purescript-easy-ffi"
  , version = "v2.1.2"
  }
, effect =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purescript/purescript-effect.git"
  , version = "v3.0.0"
  }
, either =
  { dependencies = [ "control", "invariant", "maybe", "prelude" ]
  , repo = "https://github.com/purescript/purescript-either.git"
  , version = "v5.0.0"
  }
, elmish =
  { dependencies =
    [ "aff"
    , "argonaut-core"
    , "console"
    , "debug"
    , "either"
    , "foreign-object"
    , "functions"
    , "maybe"
    , "prelude"
    , "record"
    , "tuples"
    , "typelevel-prelude"
    , "web-html"
    ]
  , repo = "https://github.com/collegevine/purescript-elmish.git"
  , version = "v0.4.0"
  }
, elmish-html =
  { dependencies = [ "elmish", "foreign-object" ]
  , repo = "https://github.com/collegevine/purescript-elmish-html.git"
  , version = "v0.2.0"
  }
, email-validate =
  { dependencies = [ "aff", "string-parsers", "transformers" ]
  , repo = "https://github.com/cdepillabout/purescript-email-validate.git"
  , version = "v6.0.0"
  }
, encoding =
  { dependencies =
    [ "arraybuffer-types", "either", "exceptions", "functions", "prelude" ]
  , repo = "https://github.com/menelaos/purescript-encoding.git"
  , version = "v0.0.6"
  }
, enums =
  { dependencies =
    [ "control"
    , "either"
    , "gen"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-enums.git"
  , version = "v5.0.0"
  }
, errors =
  { dependencies = [ "control", "effect", "either", "maybe", "transformers" ]
  , repo = "https://github.com/passy/purescript-errors.git"
  , version = "v4.1.0"
  }
, exceptions =
  { dependencies = [ "effect", "either", "maybe", "prelude" ]
  , repo = "https://github.com/purescript/purescript-exceptions.git"
  , version = "v5.0.0"
  }
, exists =
  { dependencies = [ "unsafe-coerce" ]
  , repo = "https://github.com/purescript/purescript-exists.git"
  , version = "v5.0.0"
  }
, exitcodes =
  { dependencies = [ "enums" ]
  , repo = "https://github.com/Risto-Stevcev/purescript-exitcodes.git"
  , version = "v4.0.0"
  }
, expect-inferred =
  { dependencies = [ "prelude", "typelevel-prelude" ]
  , repo = "https://github.com/justinwoo/purescript-expect-inferred.git"
  , version = "v2.0.0"
  }
, ffi-foreign =
  { dependencies = [ "console", "effect", "foreign", "prelude", "psci-support" ]
  , repo = "https://github.com/markfarrell/purescript-ffi-foreign.git"
  , version = "v0.0.2"
  }
, filterable =
  { dependencies =
    [ "arrays"
    , "either"
    , "foldable-traversable"
    , "identity"
    , "lists"
    , "ordered-collections"
    ]
  , repo = "https://github.com/purescript/purescript-filterable.git"
  , version = "v4.0.0"
  }
, fixed-points =
  { dependencies = [ "exists", "newtype", "prelude", "transformers" ]
  , repo = "https://github.com/purescript-contrib/purescript-fixed-points.git"
  , version = "v6.0.0"
  }
, fixed-precision =
  { dependencies = [ "integers", "maybe", "bigints", "strings", "math" ]
  , repo = "https://github.com/lumihq/purescript-fixed-precision.git"
  , version = "v4.3.1"
  }
, foldable-traversable =
  { dependencies =
    [ "bifunctors"
    , "const"
    , "control"
    , "either"
    , "functors"
    , "identity"
    , "maybe"
    , "newtype"
    , "orders"
    , "prelude"
    , "tuples"
    ]
  , repo = "https://github.com/purescript/purescript-foldable-traversable.git"
  , version = "v5.0.0"
  }
, foreign =
  { dependencies =
    [ "either"
    , "functions"
    , "identity"
    , "integers"
    , "lists"
    , "maybe"
    , "prelude"
    , "strings"
    , "transformers"
    ]
  , repo = "https://github.com/purescript/purescript-foreign.git"
  , version = "v6.0.0"
  }
, foreign-object =
  { dependencies =
    [ "arrays"
    , "foldable-traversable"
    , "functions"
    , "gen"
    , "lists"
    , "maybe"
    , "prelude"
    , "st"
    , "tailrec"
    , "tuples"
    , "typelevel-prelude"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-foreign-object.git"
  , version = "v3.0.0"
  }
, fork =
  { dependencies = [ "aff" ]
  , repo = "https://github.com/purescript-contrib/purescript-fork.git"
  , version = "v5.0.0"
  }
, form-urlencoded =
  { dependencies =
    [ "foldable-traversable"
    , "js-uri"
    , "maybe"
    , "newtype"
    , "prelude"
    , "strings"
    , "tuples"
    ]
  , repo =
      "https://github.com/purescript-contrib/purescript-form-urlencoded.git"
  , version = "v6.0.1"
  }
, format =
  { dependencies =
    [ "arrays"
    , "effect"
    , "integers"
    , "math"
    , "prelude"
    , "strings"
    , "unfoldable"
    ]
  , repo = "https://github.com/sharkdp/purescript-format.git"
  , version = "v4.0.0"
  }
, formatters =
  { dependencies =
    [ "datetime"
    , "fixed-points"
    , "lists"
    , "numbers"
    , "parsing"
    , "prelude"
    , "transformers"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-formatters.git"
  , version = "v5.0.0"
  }
, free =
  { dependencies =
    [ "catenable-lists"
    , "control"
    , "distributive"
    , "either"
    , "exists"
    , "foldable-traversable"
    , "invariant"
    , "lazy"
    , "maybe"
    , "prelude"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript/purescript-free.git"
  , version = "v6.0.0"
  }
, freeap =
  { dependencies = [ "const", "exists", "gen", "lists" ]
  , repo = "https://github.com/ethul/purescript-freeap.git"
  , version = "v6.0.0"
  }
, freet =
  { dependencies =
    [ "aff"
    , "bifunctors"
    , "effect"
    , "either"
    , "exists"
    , "free"
    , "prelude"
    , "tailrec"
    , "transformers"
    , "tuples"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-freet.git"
  , version = "v6.0.0"
  }
, functions =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purescript/purescript-functions.git"
  , version = "v5.0.0"
  }
, functors =
  { dependencies =
    [ "bifunctors"
    , "const"
    , "contravariant"
    , "control"
    , "distributive"
    , "either"
    , "invariant"
    , "maybe"
    , "newtype"
    , "prelude"
    , "profunctor"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript/purescript-functors.git"
  , version = "v4.0.0"
  }
, gen =
  { dependencies =
    [ "either"
    , "foldable-traversable"
    , "identity"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-gen.git"
  , version = "v3.0.0"
  }
, geometry-plane =
  { dependencies = [ "console", "effect", "psci-support", "sparse-polynomials" ]
  , repo = "https://github.com/Ebmtranceboy/purescript-geometry-plane.git"
  , version = "v1.0.1"
  }
, github-actions-toolkit =
  { dependencies =
    [ "aff"
    , "aff-promise"
    , "effect"
    , "foreign-object"
    , "node-buffer"
    , "node-path"
    , "node-streams"
    , "nullable"
    , "transformers"
    ]
  , repo =
      "https://github.com/purescript-contrib/purescript-github-actions-toolkit"
  , version = "v0.2.2"
  }
, gl-matrix =
  { dependencies =
    [ "arrays"
    , "effect"
    , "foldable-traversable"
    , "functions"
    , "math"
    , "partial"
    , "prelude"
    , "psci-support"
    ]
  , repo = "https://github.com/dirkz/purescript-gl-matrix"
  , version = "v2.0.1"
  }
, gomtang-basic =
  { dependencies = [ "console", "effect", "prelude", "record", "web-html" ]
  , repo = "https://github.com/justinwoo/purescript-gomtang-basic.git"
  , version = "v0.2.0"
  }
, grain =
  { dependencies = [ "web-html" ]
  , repo = "https://github.com/purescript-grain/purescript-grain.git"
  , version = "v1.0.0"
  }
, grain-virtualized =
  { dependencies = [ "grain" ]
  , repo =
      "https://github.com/purescript-grain/purescript-grain-virtualized.git"
  , version = "v1.0.0"
  }
, graphs =
  { dependencies = [ "catenable-lists", "ordered-collections" ]
  , repo = "https://github.com/purescript/purescript-graphs.git"
  , version = "v5.0.0"
  }
, group =
  { dependencies = [ "lists" ]
  , repo = "https://github.com/morganthomas/purescript-group.git"
  , version = "v4.1.1"
  }
, halogen-vdom =
  { dependencies =
    [ "bifunctors"
    , "effect"
    , "foreign"
    , "foreign-object"
    , "maybe"
    , "prelude"
    , "refs"
    , "tuples"
    , "unsafe-coerce"
    , "web-html"
    ]
  , repo = "https://github.com/purescript-halogen/purescript-halogen-vdom.git"
  , version = "v6.1.3"
  }
, heterogeneous =
  { dependencies =
    [ "either", "functors", "prelude", "record", "tuples", "variant" ]
  , repo = "https://github.com/natefaubion/purescript-heterogeneous.git"
  , version = "v0.5.0"
  }
, http-methods =
  { dependencies = [ "either", "prelude", "strings" ]
  , repo = "https://github.com/purescript-contrib/purescript-http-methods.git"
  , version = "v5.0.0"
  }
, httpure =
  { dependencies =
    [ "aff"
    , "arrays"
    , "bifunctors"
    , "console"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "foreign"
    , "js-uri"
    , "maybe"
    , "newtype"
    , "node-buffer"
    , "node-fs"
    , "node-http"
    , "node-streams"
    , "nullable"
    , "options"
    , "ordered-collections"
    , "prelude"
    , "refs"
    , "strings"
    , "tuples"
    , "type-equality"
    ]
  , repo = "https://github.com/cprussin/purescript-httpure.git"
  , version = "v0.11.0"
  }
, identity =
  { dependencies = [ "control", "invariant", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-identity.git"
  , version = "v5.0.0"
  }
, inflection =
  { dependencies = [ "functions" ]
  , repo = "https://github.com/athanclark/purescript-inflection.git"
  , version = "v1.0.1"
  }
, integers =
  { dependencies = [ "math", "maybe", "numbers", "prelude" ]
  , repo = "https://github.com/purescript/purescript-integers.git"
  , version = "v5.0.0"
  }
, interpolate =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/jordanmartinez/purescript-interpolate.git"
  , version = "v2.0.1"
  }
, invariant =
  { dependencies = [ "control", "prelude" ]
  , repo = "https://github.com/purescript/purescript-invariant.git"
  , version = "v5.0.0"
  }
, js-date =
  { dependencies =
    [ "datetime", "effect", "exceptions", "foreign", "integers", "now" ]
  , repo = "https://github.com/purescript-contrib/purescript-js-date.git"
  , version = "v7.0.0"
  }
, js-fileio =
  { dependencies = [ "aff", "effect", "prelude" ]
  , repo = "https://github.com/newlandsvalley/purescript-js-fileio.git"
  , version = "v2.1.0"
  }
, js-timers =
  { dependencies = [ "effect" ]
  , repo = "https://github.com/purescript-contrib/purescript-js-timers.git"
  , version = "v5.0.1"
  }
, js-uri =
  { dependencies = [ "functions", "maybe" ]
  , repo = "https://github.com/purescript-contrib/purescript-js-uri.git"
  , version = "v2.0.0"
  }
, justifill =
  { dependencies = [ "record", "spec", "typelevel-prelude" ]
  , repo = "https://github.com/i-am-the-slime/purescript-justifill.git"
  , version = "v0.2.0"
  }
, lazy =
  { dependencies = [ "control", "foldable-traversable", "invariant", "prelude" ]
  , repo = "https://github.com/purescript/purescript-lazy.git"
  , version = "v5.0.0"
  }
, lcg =
  { dependencies =
    [ "effect", "integers", "math", "maybe", "partial", "prelude", "random" ]
  , repo = "https://github.com/purescript/purescript-lcg.git"
  , version = "v3.0.0"
  }
, leibniz =
  { dependencies = [ "prelude", "unsafe-coerce" ]
  , repo = "https://github.com/paf31/purescript-leibniz.git"
  , version = "v5.0.0"
  }
, lists =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "foldable-traversable"
    , "lazy"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-lists.git"
  , version = "v6.0.0"
  }
, literals =
  { dependencies =
    [ "assert"
    , "effect"
    , "console"
    , "integers"
    , "numbers"
    , "partial"
    , "psci-support"
    , "unsafe-coerce"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/jvliwanag/purescript-literals.git"
  , version = "v0.1.1"
  }
, logging =
  { dependencies =
    [ "prelude"
    , "contravariant"
    , "console"
    , "effect"
    , "transformers"
    , "tuples"
    , "either"
    ]
  , repo = "https://github.com/rightfold/purescript-logging.git"
  , version = "v3.0.0"
  }
, longs =
  { dependencies =
    [ "effect"
    , "console"
    , "prelude"
    , "strings"
    , "foreign"
    , "nullable"
    , "functions"
    , "quickcheck"
    ]
  , repo = "https://github.com/zapph/purescript-longs.git"
  , version = "v0.1.1"
  }
, machines =
  { dependencies =
    [ "arrays"
    , "control"
    , "effect"
    , "lists"
    , "maybe"
    , "prelude"
    , "profunctor"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-machines.git"
  , version = "v6.0.0"
  }
, makkori =
  { dependencies = [ "functions", "node-http", "prelude", "record" ]
  , repo = "https://github.com/justinwoo/purescript-makkori.git"
  , version = "v1.0.0"
  }
, math =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-math.git"
  , version = "v3.0.0"
  }
, matrices =
  { dependencies = [ "arrays", "strings" ]
  , repo = "https://github.com/kritzcreek/purescript-matrices.git"
  , version = "v5.0.1"
  }
, matryoshka =
  { dependencies =
    [ "fixed-points", "free", "prelude", "profunctor", "transformers" ]
  , repo = "https://github.com/purescript-contrib/purescript-matryoshka.git"
  , version = "v0.5.0"
  }
, maybe =
  { dependencies = [ "control", "invariant", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-maybe.git"
  , version = "v5.0.0"
  }
, media-types =
  { dependencies = [ "newtype", "prelude" ]
  , repo = "https://github.com/purescript-contrib/purescript-media-types.git"
  , version = "v5.0.0"
  }
, metadata =
  { dependencies = [] : List Text
  , repo = "https://github.com/spacchetti/purescript-metadata.git"
  , version = "v0.14.0"
  }
, midi =
  { dependencies =
    [ "effect", "integers", "lists", "prelude", "signal", "string-parsers" ]
  , repo = "https://github.com/newlandsvalley/purescript-midi.git"
  , version = "v3.0.0"
  }
, milkis =
  { dependencies =
    [ "aff-promise"
    , "arraybuffer-types"
    , "foreign-object"
    , "prelude"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/justinwoo/purescript-milkis.git"
  , version = "v7.4.0"
  }
, minibench =
  { dependencies =
    [ "console"
    , "effect"
    , "integers"
    , "math"
    , "numbers"
    , "partial"
    , "prelude"
    , "refs"
    ]
  , repo = "https://github.com/purescript/purescript-minibench.git"
  , version = "v3.0.0"
  }
, mmorph =
  { dependencies = [ "free", "functors", "transformers" ]
  , repo = "https://github.com/thimoteus/purescript-mmorph.git"
  , version = "v6.0.0"
  }
, monad-control =
  { dependencies = [ "aff", "freet", "identity", "lists" ]
  , repo = "https://github.com/athanclark/purescript-monad-control.git"
  , version = "v5.0.0"
  }
, monad-logger =
  { dependencies =
    [ "aff"
    , "ansi"
    , "argonaut"
    , "arrays"
    , "console"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "foreign-object"
    , "integers"
    , "js-date"
    , "maybe"
    , "newtype"
    , "ordered-collections"
    , "prelude"
    , "strings"
    , "transformers"
    , "tuples"
    ]
  , repo = "https://github.com/cprussin/purescript-monad-logger.git"
  , version = "v1.3.1"
  }
, monad-loops =
  { dependencies = [ "maybe", "tailrec", "prelude", "tuples", "lists" ]
  , repo = "https://github.com/mlang/purescript-monad-loops.git"
  , version = "v0.5.0"
  }
, monad-unlift =
  { dependencies = [ "monad-control" ]
  , repo = "https://github.com/athanclark/purescript-monad-unlift.git"
  , version = "v1.0.1"
  }
, monoidal =
  { dependencies = [ "profunctor", "either", "tuples", "these" ]
  , repo = "https://github.com/mcneissue/purescript-monoidal.git"
  , version = "v0.16.0"
  }
, morello =
  { dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "heterogeneous"
    , "profunctor-lenses"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    , "strings"
    , "validation"
    ]
  , repo = "https://github.com/sigma-andex/purescript-morello.git"
  , version = "v0.1.1"
  }
, motsunabe =
  { dependencies = [ "lists", "strings" ]
  , repo = "https://github.com/justinwoo/purescript-motsunabe.git"
  , version = "v2.0.0"
  }
, naturals =
  { dependencies = [ "maybe", "prelude", "enums" ]
  , repo = "https://github.com/LiamGoodacre/purescript-naturals.git"
  , version = "v3.0.0"
  }
, nested-functor =
  { dependencies = [ "prelude", "type-equality" ]
  , repo = "https://github.com/acple/purescript-nested-functor.git"
  , version = "v0.2.1"
  }
, newtype =
  { dependencies = [ "prelude", "safe-coerce" ]
  , repo = "https://github.com/purescript/purescript-newtype.git"
  , version = "v4.0.0"
  }
, node-buffer =
  { dependencies =
    [ "arraybuffer-types", "effect", "maybe", "st", "unsafe-coerce" ]
  , repo = "https://github.com/purescript-node/purescript-node-buffer.git"
  , version = "v7.0.0"
  }
, node-child-process =
  { dependencies =
    [ "exceptions"
    , "foreign"
    , "foreign-object"
    , "functions"
    , "node-fs"
    , "node-streams"
    , "nullable"
    , "posix-types"
    , "unsafe-coerce"
    ]
  , repo =
      "https://github.com/purescript-node/purescript-node-child-process.git"
  , version = "v7.0.0"
  }
, node-fs =
  { dependencies =
    [ "datetime"
    , "effect"
    , "either"
    , "enums"
    , "exceptions"
    , "functions"
    , "integers"
    , "js-date"
    , "maybe"
    , "node-buffer"
    , "node-path"
    , "node-streams"
    , "nullable"
    , "partial"
    , "prelude"
    , "strings"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript-node/purescript-node-fs.git"
  , version = "v6.0.0"
  }
, node-fs-aff =
  { dependencies = [ "aff", "either", "node-fs", "node-path" ]
  , repo = "https://github.com/purescript-node/purescript-node-fs-aff.git"
  , version = "v7.0.0"
  }
, node-he =
  { dependencies = [] : List Text
  , repo = "https://github.com/justinwoo/purescript-node-he.git"
  , version = "v0.2.0"
  }
, node-http =
  { dependencies =
    [ "arraybuffer-types"
    , "contravariant"
    , "effect"
    , "foreign"
    , "foreign-object"
    , "maybe"
    , "node-buffer"
    , "node-net"
    , "node-streams"
    , "node-url"
    , "nullable"
    , "options"
    , "prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript-node/purescript-node-http.git"
  , version = "v6.0.0"
  }
, node-net =
  { dependencies =
    [ "effect"
    , "either"
    , "exceptions"
    , "foreign"
    , "maybe"
    , "node-buffer"
    , "node-fs"
    , "nullable"
    , "options"
    , "prelude"
    , "transformers"
    ]
  , repo = "https://github.com/purescript-node/purescript-node-net.git"
  , version = "v2.0.0"
  }
, node-path =
  { dependencies = [ "effect" ]
  , repo = "https://github.com/purescript-node/purescript-node-path.git"
  , version = "v4.0.0"
  }
, node-postgres =
  { dependencies =
    [ "aff"
    , "arrays"
    , "datetime"
    , "either"
    , "foldable-traversable"
    , "foreign"
    , "integers"
    , "nullable"
    , "prelude"
    , "transformers"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/epost/purescript-node-postgres.git"
  , version = "v5.0.0"
  }
, node-process =
  { dependencies =
    [ "effect"
    , "foreign-object"
    , "maybe"
    , "node-streams"
    , "posix-types"
    , "prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript-node/purescript-node-process.git"
  , version = "v8.0.0"
  }
, node-readline =
  { dependencies =
    [ "effect"
    , "foreign"
    , "node-process"
    , "node-streams"
    , "options"
    , "prelude"
    ]
  , repo = "https://github.com/purescript-node/purescript-node-readline.git"
  , version = "v5.0.0"
  }
, node-sqlite3 =
  { dependencies = [ "aff", "foreign" ]
  , repo = "https://github.com/justinwoo/purescript-node-sqlite3.git"
  , version = "v6.0.0"
  }
, node-streams =
  { dependencies =
    [ "effect", "either", "exceptions", "node-buffer", "prelude" ]
  , repo = "https://github.com/purescript-node/purescript-node-streams.git"
  , version = "v5.0.0"
  }
, node-url =
  { dependencies = [ "nullable" ]
  , repo = "https://github.com/purescript-node/purescript-node-url.git"
  , version = "v5.0.0"
  }
, nonempty =
  { dependencies =
    [ "control"
    , "foldable-traversable"
    , "maybe"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-nonempty.git"
  , version = "v6.0.0"
  }
, now =
  { dependencies = [ "datetime", "effect" ]
  , repo = "https://github.com/purescript-contrib/purescript-now.git"
  , version = "v5.0.0"
  }
, nullable =
  { dependencies = [ "effect", "functions", "maybe" ]
  , repo = "https://github.com/purescript-contrib/purescript-nullable.git"
  , version = "v5.0.0"
  }
, numbers =
  { dependencies = [ "functions", "math", "maybe" ]
  , repo = "https://github.com/purescript/purescript-numbers.git"
  , version = "v8.0.0"
  }
, options =
  { dependencies =
    [ "contravariant", "foreign", "foreign-object", "maybe", "tuples" ]
  , repo = "https://github.com/purescript-contrib/purescript-options.git"
  , version = "v6.0.0"
  }
, ordered-collections =
  { dependencies =
    [ "arrays"
    , "foldable-traversable"
    , "gen"
    , "lists"
    , "maybe"
    , "partial"
    , "prelude"
    , "st"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-ordered-collections.git"
  , version = "v2.0.0"
  }
, ordered-set =
  { dependencies =
    [ "argonaut-codecs", "arrays", "partial", "prelude", "unfoldable" ]
  , repo = "https://github.com/flip111/purescript-ordered-set.git"
  , version = "v0.4.0"
  }
, orders =
  { dependencies = [ "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-orders.git"
  , version = "v5.0.0"
  }
, pairs =
  { dependencies =
    [ "console", "distributive", "foldable-traversable", "quickcheck" ]
  , repo = "https://github.com/sharkdp/purescript-pairs.git"
  , version = "v7.0.0"
  }
, parallel =
  { dependencies =
    [ "control"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "functors"
    , "maybe"
    , "newtype"
    , "prelude"
    , "profunctor"
    , "refs"
    , "transformers"
    ]
  , repo = "https://github.com/purescript/purescript-parallel.git"
  , version = "v5.0.0"
  }
, parsing =
  { dependencies =
    [ "arrays"
    , "either"
    , "foldable-traversable"
    , "identity"
    , "integers"
    , "lists"
    , "maybe"
    , "prelude"
    , "strings"
    , "transformers"
    , "unicode"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-parsing.git"
  , version = "v6.0.0"
  }
, parsing-expect =
  { dependencies = [ "console", "effect", "parsing", "prelude", "psci-support" ]
  , repo = "https://github.com/markfarrell/purescript-parsing-expect.git"
  , version = "v0.0.3"
  }
, parsing-hexadecimal =
  { dependencies =
    [ "console"
    , "effect"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "parsing-expect"
    ]
  , repo = "https://github.com/markfarrell/purescript-parsing-hexadecimal.git"
  , version = "v0.0.2"
  }
, parsing-repetition =
  { dependencies =
    [ "console"
    , "effect"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "parsing-expect"
    ]
  , repo = "https://github.com/markfarrell/purescript-parsing-repetition.git"
  , version = "v0.0.6"
  }
, parsing-replace =
  { dependencies = [ "parsing" ]
  , repo = "https://github.com/jamesdbrock/purescript-parsing-replace.git"
  , version = "v1.0.2"
  }
, parsing-uuid =
  { dependencies =
    [ "console"
    , "effect"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "parsing-expect"
    , "parsing-repetition"
    , "parsing-hexadecimal"
    ]
  , repo = "https://github.com/markfarrell/purescript-parsing-uuid.git"
  , version = "v0.0.3"
  }
, parsing-validation =
  { dependencies = [ "console", "effect", "parsing", "prelude", "psci-support" ]
  , repo = "https://github.com/markfarrell/purescript-parsing-validation.git"
  , version = "v0.1.2"
  }
, partial =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-partial.git"
  , version = "v3.0.0"
  }
, pathy =
  { dependencies =
    [ "console"
    , "exceptions"
    , "lists"
    , "partial"
    , "profunctor"
    , "strings"
    , "transformers"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-pathy.git"
  , version = "v8.0.0"
  }
, phoenix =
  { dependencies = [ "options" ]
  , repo = "https://github.com/brandonhamilton/purescript-phoenix.git"
  , version = "v4.0.0"
  }
, pipes =
  { dependencies =
    [ "aff", "lists", "mmorph", "prelude", "tailrec", "transformers", "tuples" ]
  , repo = "https://github.com/felixSchl/purescript-pipes.git"
  , version = "v7.0.0"
  }
, point-free =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/ursi/purescript-point-free.git"
  , version = "v0.1.3"
  }
, polymorphic-vectors =
  { dependencies =
    [ "distributive"
    , "foldable-traversable"
    , "math"
    , "prelude"
    , "record"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/artemisSystem/purescript-polymorphic-vectors.git"
  , version = "v3.0.0"
  }
, posix-types =
  { dependencies = [ "maybe", "prelude" ]
  , repo = "https://github.com/purescript-node/purescript-posix-types.git"
  , version = "v5.0.0"
  }
, precise =
  { dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "exceptions"
    , "gen"
    , "integers"
    , "lists"
    , "numbers"
    , "prelude"
    , "strings"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-precise.git"
  , version = "v5.0.0"
  }
, precise-datetime =
  { dependencies =
    [ "arrays"
    , "console"
    , "datetime"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "formatters"
    , "integers"
    , "js-date"
    , "lists"
    , "maybe"
    , "newtype"
    , "prelude"
    , "strings"
    , "tuples"
    , "unicode"
    , "numbers"
    , "decimals"
    ]
  , repo = "https://github.com/awakesecurity/purescript-precise-datetime.git"
  , version = "v6.0.0"
  }
, prelude =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-prelude.git"
  , version = "v5.0.0"
  }
, prettier =
  { dependencies = [ "maybe", "prelude" ]
  , repo = "https://github.com/epicallan/purescript-prettier.git"
  , version = "v0.3.0"
  }
, prettier-printer =
  { dependencies = [ "prelude", "console", "lists", "tuples", "strings" ]
  , repo = "https://github.com/paulyoung/purescript-prettier-printer.git"
  , version = "v3.0.0"
  }
, profunctor =
  { dependencies =
    [ "control"
    , "distributive"
    , "either"
    , "exists"
    , "invariant"
    , "newtype"
    , "prelude"
    , "tuples"
    ]
  , repo = "https://github.com/purescript/purescript-profunctor.git"
  , version = "v5.0.0"
  }
, profunctor-lenses =
  { dependencies =
    [ "arrays"
    , "bifunctors"
    , "const"
    , "control"
    , "distributive"
    , "either"
    , "foldable-traversable"
    , "foreign-object"
    , "functors"
    , "identity"
    , "lists"
    , "maybe"
    , "newtype"
    , "ordered-collections"
    , "partial"
    , "prelude"
    , "profunctor"
    , "record"
    , "transformers"
    , "tuples"
    ]
  , repo =
      "https://github.com/purescript-contrib/purescript-profunctor-lenses.git"
  , version = "v7.0.0"
  }
, promises =
  { dependencies =
    [ "console"
    , "datetime"
    , "exceptions"
    , "functions"
    , "prelude"
    , "transformers"
    ]
  , repo = "https://github.com/thimoteus/purescript-promises.git"
  , version = "v3.1.1"
  }
, psa-utils =
  { dependencies =
    [ "ansi"
    , "argonaut-codecs"
    , "argonaut-core"
    , "arrays"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "maybe"
    , "node-path"
    , "ordered-collections"
    , "prelude"
    , "strings"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/natefaubion/purescript-psa-utils.git"
  , version = "v8.0.0"
  }
, psc-ide =
  { dependencies =
    [ "aff"
    , "argonaut"
    , "arrays"
    , "console"
    , "maybe"
    , "node-child-process"
    , "node-fs"
    , "parallel"
    , "random"
    ]
  , repo = "https://github.com/kRITZCREEK/purescript-psc-ide.git"
  , version = "v18.0.0"
  }
, psci-support =
  { dependencies = [ "console", "effect", "prelude" ]
  , repo = "https://github.com/purescript/purescript-psci-support.git"
  , version = "v5.0.0"
  }
, queue =
  { dependencies = [ "refs", "aff", "foreign-object", "avar" ]
  , repo = "https://github.com/athanclark/purescript-queue.git"
  , version = "v8.0.2"
  }
, quickcheck =
  { dependencies =
    [ "arrays"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "enums"
    , "exceptions"
    , "foldable-traversable"
    , "gen"
    , "identity"
    , "integers"
    , "lazy"
    , "lcg"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "record"
    , "st"
    , "strings"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-quickcheck.git"
  , version = "v7.0.0"
  }
, quickcheck-combinators =
  { dependencies = [ "quickcheck", "typelevel" ]
  , repo = "https://github.com/athanclark/purescript-quickcheck-combinators.git"
  , version = "v0.1.3"
  }
, quickcheck-laws =
  { dependencies = [ "enums", "quickcheck" ]
  , repo =
      "https://github.com/purescript-contrib/purescript-quickcheck-laws.git"
  , version = "v6.0.0"
  }
, quickcheck-utf8 =
  { dependencies = [ "quickcheck" ]
  , repo = "https://github.com/openchronology/purescript-quickcheck-utf8.git"
  , version = "v0.0.0"
  }
, quotient =
  { dependencies = [ "prelude", "quickcheck" ]
  , repo = "https://github.com/rightfold/purescript-quotient.git"
  , version = "v3.0.0"
  }
, random =
  { dependencies = [ "effect", "integers", "math" ]
  , repo = "https://github.com/purescript/purescript-random.git"
  , version = "v5.0.0"
  }
, rationals =
  { dependencies = [ "integers", "prelude" ]
  , repo = "https://github.com/anttih/purescript-rationals.git"
  , version = "v5.0.0"
  }
, react =
  { dependencies =
    [ "effect"
    , "exceptions"
    , "maybe"
    , "nullable"
    , "prelude"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-react.git"
  , version = "v9.0.0"
  }
, react-dom =
  { dependencies = [ "effect", "react", "web-dom" ]
  , repo = "https://github.com/purescript-contrib/purescript-react-dom.git"
  , version = "v7.0.0"
  }
, react-enzyme =
  { dependencies =
    [ "aff", "console", "effect", "foreign", "psci-support", "react" ]
  , repo = "https://github.com/alvart/purescript-react-enzyme.git"
  , version = "v1.1.1"
  }
, react-queue =
  { dependencies = [ "exceptions", "queue", "react", "zeta" ]
  , repo = "https://github.com/athanclark/purescript-react-queue.git"
  , version = "v1.3.2"
  }
, read =
  { dependencies = [ "maybe", "prelude", "strings" ]
  , repo = "https://github.com/truqu/purescript-read.git"
  , version = "v1.0.1"
  }
, record =
  { dependencies = [ "functions", "prelude", "unsafe-coerce" ]
  , repo = "https://github.com/purescript/purescript-record.git"
  , version = "v3.0.0"
  }
, redux-devtools =
  { dependencies = [ "effect", "foreign", "nullable", "prelude" ]
  , repo = "https://github.com/justinwoo/purescript-redux-devtools.git"
  , version = "v0.1.0"
  }
, refined =
  { dependencies =
    [ "argonaut", "effect", "prelude", "quickcheck", "typelevel" ]
  , repo = "https://github.com/danieljharvey/purescript-refined.git"
  , version = "v1.0.0"
  }
, refs =
  { dependencies = [ "effect", "prelude" ]
  , repo = "https://github.com/purescript/purescript-refs.git"
  , version = "v5.0.0"
  }
, remotedata =
  { dependencies = [ "bifunctors", "either", "profunctor-lenses" ]
  , repo = "https://github.com/krisajenkins/purescript-remotedata.git"
  , version = "v4.2.0"
  }
, resource =
  { dependencies =
    [ "aff"
    , "console"
    , "control"
    , "effect"
    , "newtype"
    , "prelude"
    , "psci-support"
    , "refs"
    ]
  , repo = "https://github.com/joneshf/purescript-resource.git"
  , version = "v2.0.0"
  }
, result =
  { dependencies = [ "either", "foldable-traversable", "prelude" ]
  , repo = "https://github.com/ad-si/purescript-result.git"
  , version = "v1.0.3"
  }
, ring-modules =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/f-o-a-m/purescript-ring-modules.git"
  , version = "v5.0.1"
  }
, routing =
  { dependencies =
    [ "aff"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "integers"
    , "js-uri"
    , "lists"
    , "maybe"
    , "numbers"
    , "partial"
    , "prelude"
    , "semirings"
    , "tuples"
    , "validation"
    , "web-html"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-routing.git"
  , version = "v10.0.0"
  }
, routing-duplex =
  { dependencies =
    [ "arrays"
    , "control"
    , "either"
    , "js-uri"
    , "lazy"
    , "numbers"
    , "prelude"
    , "profunctor"
    , "record"
    , "strings"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/natefaubion/purescript-routing-duplex.git"
  , version = "v0.5.0"
  }
, row-extra =
  { dependencies = [] : List Text
  , repo = "https://github.com/athanclark/purescript-row-extra.git"
  , version = "v0.0.1"
  }
, run =
  { dependencies =
    [ "aff"
    , "effect"
    , "either"
    , "free"
    , "maybe"
    , "newtype"
    , "prelude"
    , "profunctor"
    , "tailrec"
    , "tuples"
    , "type-equality"
    , "typelevel-prelude"
    , "unsafe-coerce"
    , "variant"
    ]
  , repo = "https://github.com/natefaubion/purescript-run.git"
  , version = "v4.0.0"
  }
, safe-coerce =
  { dependencies = [ "unsafe-coerce" ]
  , repo = "https://github.com/purescript/purescript-safe-coerce.git"
  , version = "v1.0.0"
  }
, safely =
  { dependencies = [ "freet", "lists" ]
  , repo = "https://github.com/paf31/purescript-safely.git"
  , version = "v4.0.1"
  }
, scrypt =
  { dependencies = [ "aff", "arraybuffer-types" ]
  , repo = "https://github.com/athanclark/purescript-scrypt.git"
  , version = "v1.0.1"
  }
, selection-foldable =
  { dependencies = [ "filterable", "foldable-traversable", "maybe", "prelude" ]
  , repo = "https://github.com/jamieyung/purescript-selection-foldable.git"
  , version = "v0.2.0"
  }
, semirings =
  { dependencies = [ "foldable-traversable", "lists", "newtype", "prelude" ]
  , repo = "https://github.com/purescript/purescript-semirings.git"
  , version = "v6.0.0"
  }
, setimmediate =
  { dependencies = [ "effect", "functions" ]
  , repo = "https://github.com/athanclark/purescript-setimmediate.git"
  , version = "v1.0.2"
  }
, signal =
  { dependencies =
    [ "aff", "foldable-traversable", "js-timers", "maybe", "prelude" ]
  , repo = "https://github.com/bodil/purescript-signal.git"
  , version = "v12.0.1"
  }
, simple-emitter =
  { dependencies = [ "ordered-collections", "refs" ]
  , repo = "https://github.com/oreshinya/purescript-simple-emitter.git"
  , version = "v2.0.0"
  }
, simple-ulid =
  { dependencies = [ "exceptions", "now", "strings" ]
  , repo = "https://github.com/oreshinya/purescript-simple-ulid.git"
  , version = "v2.0.0"
  }
, sized-matrices =
  { dependencies =
    [ "sized-vectors"
    , "prelude"
    , "foldable-traversable"
    , "maybe"
    , "arrays"
    , "unfoldable"
    , "typelevel"
    , "distributive"
    , "vectorfield"
    , "strings"
    ]
  , repo = "https://github.com/csicar/purescript-sized-matrices"
  , version = "v1.0.0"
  }
, sized-vectors =
  { dependencies =
    [ "argonaut"
    , "arrays"
    , "distributive"
    , "foldable-traversable"
    , "maybe"
    , "prelude"
    , "quickcheck"
    , "typelevel"
    , "unfoldable"
    ]
  , repo = "https://github.com/bodil/purescript-sized-vectors.git"
  , version = "v5.0.2"
  }
, slug =
  { dependencies =
    [ "prelude", "maybe", "strings", "unicode", "argonaut-codecs" ]
  , repo = "https://github.com/thomashoneyman/purescript-slug.git"
  , version = "v3.0.0"
  }
, snabbdom =
  { dependencies = [ "ordered-collections", "prelude", "web-dom", "web-html" ]
  , repo = "https://github.com/LukaJCB/purescript-snabbdom.git"
  , version = "v1.0.1"
  }
, sodium =
  { dependencies =
    [ "aff", "nullable", "numbers", "quickcheck-laws", "test-unit" ]
  , repo = "https://github.com/SodiumFRP/purescript-sodium.git"
  , version = "v2.1.0"
  }
, sparse-matrices =
  { dependencies = [ "prelude", "console", "effect", "sparse-polynomials" ]
  , repo = "https://github.com/Ebmtranceboy/purescript-sparse-matrices.git"
  , version = "v1.1.0"
  }
, sparse-polynomials =
  { dependencies =
    [ "prelude"
    , "console"
    , "effect"
    , "ordered-collections"
    , "tuples"
    , "rationals"
    , "cartesian"
    ]
  , repo = "https://github.com/Ebmtranceboy/purescript-sparse-polynomials.git"
  , version = "v1.0.3"
  }
, spec =
  { dependencies =
    [ "aff"
    , "ansi"
    , "avar"
    , "console"
    , "exceptions"
    , "foldable-traversable"
    , "fork"
    , "now"
    , "pipes"
    , "prelude"
    , "strings"
    , "transformers"
    ]
  , repo = "https://github.com/purescript-spec/purescript-spec.git"
  , version = "v5.0.0"
  }
, spec-discovery =
  { dependencies = [ "arrays", "effect", "node-fs", "prelude", "spec" ]
  , repo = "https://github.com/purescript-spec/purescript-spec-discovery.git"
  , version = "v6.0.0"
  }
, spec-quickcheck =
  { dependencies = [ "aff", "prelude", "quickcheck", "random", "spec" ]
  , repo = "https://github.com/purescript-spec/purescript-spec-quickcheck.git"
  , version = "v4.0.0"
  }
, spork =
  { dependencies =
    [ "prelude"
    , "console"
    , "tailrec"
    , "arrays"
    , "refs"
    , "foldable-traversable"
    , "maybe"
    , "aff"
    , "halogen-vdom"
    , "dom-indexed"
    , "unsafe-reference"
    , "web-dom"
    , "web-html"
    , "web-events"
    , "web-uievents"
    , "effect"
    , "foreign"
    , "ordered-collections"
    ]
  , repo = "https://github.com/natefaubion/purescript-spork.git"
  , version = "v1.0.0"
  }
, st =
  { dependencies = [ "partial", "prelude", "tailrec", "unsafe-coerce" ]
  , repo = "https://github.com/purescript/purescript-st.git"
  , version = "v5.0.0"
  }
, string-parsers =
  { dependencies =
    [ "arrays"
    , "bifunctors"
    , "control"
    , "either"
    , "foldable-traversable"
    , "lists"
    , "maybe"
    , "prelude"
    , "strings"
    , "tailrec"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-string-parsers.git"
  , version = "v6.0.0"
  }
, strings =
  { dependencies =
    [ "arrays"
    , "control"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "gen"
    , "integers"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/purescript/purescript-strings.git"
  , version = "v5.0.0"
  }
, strings-extra =
  { dependencies =
    [ "arrays"
    , "foldable-traversable"
    , "maybe"
    , "prelude"
    , "strings"
    , "unicode"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-strings-extra.git"
  , version = "v3.0.0"
  }
, stringutils =
  { dependencies =
    [ "arrays", "either", "integers", "maybe", "partial", "prelude", "strings" ]
  , repo = "https://github.com/menelaos/purescript-stringutils.git"
  , version = "v0.0.11"
  }
, subcategory =
  { dependencies = [ "prelude", "profunctor", "record", "typelevel-prelude" ]
  , repo = "https://github.com/matthew-hilty/purescript-subcategory.git"
  , version = "v0.2.0"
  }
, subtlecrypto =
  { dependencies = [ "aff", "arraybuffer-types", "foreign", "promises" ]
  , repo = "https://github.com/athanclark/purescript-subtlecrypto.git"
  , version = "v0.0.1"
  }
, suggest =
  { dependencies =
    [ "console"
    , "node-buffer"
    , "node-fs"
    , "node-process"
    , "node-streams"
    , "psa-utils"
    , "refs"
    ]
  , repo = "https://github.com/nwolverson/purescript-suggest.git"
  , version = "v5.0.0"
  }
, sunde =
  { dependencies = [ "aff", "effect", "node-child-process", "prelude" ]
  , repo = "https://github.com/justinwoo/purescript-sunde.git"
  , version = "v2.0.0"
  }
, supply =
  { dependencies = [ "console", "effect", "newtype", "tuples", "lazy", "refs" ]
  , repo = "https://github.com/ajnsit/purescript-supply.git"
  , version = "v0.1.0"
  }
, systemd-journald =
  { dependencies = [ "console", "functions", "prelude" ]
  , repo = "https://github.com/paluh/purescript-systemd-journald.git"
  , version = "v0.2.1"
  }
, tailrec =
  { dependencies =
    [ "bifunctors"
    , "effect"
    , "either"
    , "identity"
    , "maybe"
    , "partial"
    , "prelude"
    , "refs"
    ]
  , repo = "https://github.com/purescript/purescript-tailrec.git"
  , version = "v5.0.0"
  }
, test-unit =
  { dependencies =
    [ "aff"
    , "avar"
    , "effect"
    , "either"
    , "free"
    , "js-timers"
    , "lists"
    , "prelude"
    , "quickcheck"
    , "strings"
    ]
  , repo = "https://github.com/bodil/purescript-test-unit.git"
  , version = "v16.0.0"
  }
, text-encoding =
  { dependencies =
    [ "arraybuffer-types", "either", "exceptions", "functions", "strings" ]
  , repo = "https://github.com/AlexaDeWit/purescript-text-encoding.git"
  , version = "v1.0.0"
  }
, thermite =
  { dependencies =
    [ "aff", "coroutines", "freet", "profunctor-lenses", "react" ]
  , repo = "https://github.com/paf31/purescript-thermite.git"
  , version = "v6.3.1"
  }
, thermite-dom =
  { dependencies = [ "thermite", "react", "react-dom", "web-html" ]
  , repo = "https://github.com/athanclark/purescript-thermite-dom.git"
  , version = "v0.3.1"
  }
, these =
  { dependencies =
    [ "arrays", "gen", "lists", "quickcheck", "quickcheck-laws", "tuples" ]
  , repo = "https://github.com/purescript-contrib/purescript-these.git"
  , version = "v5.0.0"
  }
, toppokki =
  { dependencies =
    [ "aff-promise"
    , "functions"
    , "node-buffer"
    , "node-http"
    , "prelude"
    , "record"
    ]
  , repo = "https://github.com/justinwoo/purescript-toppokki.git"
  , version = "v2.4.0"
  }
, transformers =
  { dependencies =
    [ "control"
    , "distributive"
    , "effect"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "identity"
    , "lazy"
    , "maybe"
    , "newtype"
    , "prelude"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript/purescript-transformers.git"
  , version = "v5.0.0"
  }
, tree-rose =
  { dependencies = [ "prelude", "lists", "free" ]
  , repo = "https://github.com/jordanmartinez/purescript-tree-rose.git"
  , version = "v2.0.0"
  }
, tuples =
  { dependencies = [ "control", "invariant", "prelude" ]
  , repo = "https://github.com/purescript/purescript-tuples.git"
  , version = "v6.0.0"
  }
, turf =
  { dependencies =
    [ "argonaut-codecs", "argonaut-core", "foreign-object", "quickcheck" ]
  , repo = "https://github.com/jisantuc/purescript-turf.git"
  , version = "v1.0.0"
  }
, type-equality =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-type-equality.git"
  , version = "v4.0.0"
  }
, typelevel =
  { dependencies =
    [ "partial", "prelude", "tuples", "typelevel-prelude", "unsafe-coerce" ]
  , repo = "https://github.com/bodil/purescript-typelevel.git"
  , version = "v6.0.0"
  }
, typelevel-prelude =
  { dependencies = [ "prelude", "type-equality" ]
  , repo = "https://github.com/purescript/purescript-typelevel-prelude.git"
  , version = "v6.0.0"
  }
, undefinable =
  { dependencies = [ "functions", "maybe" ]
  , repo = "https://github.com/ethul/purescript-undefinable.git"
  , version = "v4.0.0"
  }
, undefined =
  { dependencies = [] : List Text
  , repo = "https://github.com/bklaric/purescript-undefined.git"
  , version = "v1.0.2"
  }
, undefined-or =
  { dependencies = [ "maybe" ]
  , repo = "https://github.com/d86leader/purescript-undefined-or.git"
  , version = "v1.0.1"
  }
, unfoldable =
  { dependencies =
    [ "foldable-traversable", "maybe", "partial", "prelude", "tuples" ]
  , repo = "https://github.com/purescript/purescript-unfoldable.git"
  , version = "v5.0.0"
  }
, unicode =
  { dependencies = [ "foldable-traversable", "maybe", "strings" ]
  , repo = "https://github.com/purescript-contrib/purescript-unicode.git"
  , version = "v5.0.0"
  }
, unordered-collections =
  { dependencies =
    [ "arrays"
    , "enums"
    , "functions"
    , "integers"
    , "lists"
    , "prelude"
    , "record"
    , "tuples"
    , "typelevel-prelude"
    ]
  , repo = "https://github.com/fehrenbach/purescript-unordered-collections.git"
  , version = "v1.10.0"
  }
, unorm =
  { dependencies = [] : List Text
  , repo = "https://github.com/athanclark/purescript-unorm.git"
  , version = "v1.0.1"
  }
, unsafe-coerce =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-unsafe-coerce.git"
  , version = "v5.0.0"
  }
, unsafe-reference =
  { dependencies = [ "prelude" ]
  , repo =
      "https://github.com/purescript-contrib/purescript-unsafe-reference.git"
  , version = "v4.0.0"
  }
, uri =
  { dependencies =
    [ "arrays"
    , "integers"
    , "js-uri"
    , "numbers"
    , "parsing"
    , "prelude"
    , "profunctor-lenses"
    , "these"
    , "transformers"
    , "unfoldable"
    ]
  , repo = "https://github.com/purescript-contrib/purescript-uri.git"
  , version = "v8.0.0"
  }
, validation =
  { dependencies =
    [ "bifunctors"
    , "control"
    , "either"
    , "foldable-traversable"
    , "newtype"
    , "prelude"
    ]
  , repo = "https://github.com/purescript/purescript-validation.git"
  , version = "v5.0.0"
  }
, variant =
  { dependencies =
    [ "enums"
    , "lists"
    , "maybe"
    , "partial"
    , "prelude"
    , "record"
    , "tuples"
    , "unsafe-coerce"
    ]
  , repo = "https://github.com/natefaubion/purescript-variant.git"
  , version = "v7.0.2"
  }
, vectorfield =
  { dependencies = [ "console", "effect", "group", "prelude", "psci-support" ]
  , repo = "https://github.com/csicar/purescript-vectorfield.git"
  , version = "v1.0.1"
  }
, versions =
  { dependencies =
    [ "either"
    , "maybe"
    , "integers"
    , "strings"
    , "lists"
    , "functions"
    , "foldable-traversable"
    , "control"
    , "parsing"
    , "partial"
    , "orders"
    ]
  , repo = "https://github.com/hdgarrood/purescript-versions.git"
  , version = "v6.0.0"
  }
, web-clipboard =
  { dependencies = [ "web-html" ]
  , repo = "https://github.com/purescript-web/purescript-web-clipboard.git"
  , version = "v3.0.0"
  }
, web-cssom =
  { dependencies = [ "web-dom", "web-html", "web-uievents" ]
  , repo = "https://github.com/purescript-web/purescript-web-cssom.git"
  , version = "v1.0.0"
  }
, web-dom =
  { dependencies = [ "web-events" ]
  , repo = "https://github.com/purescript-web/purescript-web-dom.git"
  , version = "v5.0.0"
  }
, web-dom-parser =
  { dependencies = [ "effect", "partial", "prelude", "web-dom" ]
  , repo = "https://github.com/purescript-web/purescript-web-dom-parser.git"
  , version = "v7.0.0"
  }
, web-dom-xpath =
  { dependencies = [ "web-dom" ]
  , repo = "https://github.com/purescript-web/purescript-web-dom-xpath.git"
  , version = "v2.0.0"
  }
, web-encoding =
  { dependencies = [ "arraybuffer-types", "effect", "newtype", "prelude" ]
  , repo = "https://github.com/purescript-web/purescript-web-encoding.git"
  , version = "v2.0.0"
  }
, web-events =
  { dependencies = [ "datetime", "enums", "nullable" ]
  , repo = "https://github.com/purescript-web/purescript-web-events.git"
  , version = "v3.0.0"
  }
, web-fetch =
  { dependencies =
    [ "effect"
    , "foreign-object"
    , "http-methods"
    , "prelude"
    , "record"
    , "typelevel-prelude"
    , "web-file"
    , "web-promise"
    , "web-streams"
    ]
  , repo = "https://github.com/purescript-web/purescript-web-fetch.git"
  , version = "v2.0.0"
  }
, web-file =
  { dependencies = [ "foreign", "media-types", "web-dom" ]
  , repo = "https://github.com/purescript-web/purescript-web-file.git"
  , version = "v3.0.0"
  }
, web-html =
  { dependencies = [ "js-date", "web-dom", "web-file", "web-storage" ]
  , repo = "https://github.com/purescript-web/purescript-web-html.git"
  , version = "v3.0.0"
  }
, web-promise =
  { dependencies =
    [ "effect"
    , "foldable-traversable"
    , "exceptions"
    , "functions"
    , "maybe"
    , "prelude"
    ]
  , repo = "https://github.com/purescript-web/purescript-web-promise.git"
  , version = "v2.0.1"
  }
, web-socket =
  { dependencies = [ "arraybuffer-types", "web-file" ]
  , repo = "https://github.com/purescript-web/purescript-web-socket.git"
  , version = "v3.0.0"
  }
, web-storage =
  { dependencies = [ "nullable", "web-events" ]
  , repo = "https://github.com/purescript-web/purescript-web-storage.git"
  , version = "v4.0.0"
  }
, web-streams =
  { dependencies =
    [ "arraybuffer-types"
    , "effect"
    , "exceptions"
    , "nullable"
    , "prelude"
    , "tuples"
    , "web-promise"
    ]
  , repo = "https://github.com/purescript-web/purescript-web-streams.git"
  , version = "v2.0.0"
  }
, web-touchevents =
  { dependencies = [ "web-uievents" ]
  , repo = "https://github.com/purescript-web/purescript-web-touchevents.git"
  , version = "v3.0.0"
  }
, web-uievents =
  { dependencies = [ "web-html" ]
  , repo = "https://github.com/purescript-web/purescript-web-uievents.git"
  , version = "v3.0.0"
  }
, web-xhr =
  { dependencies =
    [ "arraybuffer-types"
    , "datetime"
    , "http-methods"
    , "web-dom"
    , "web-file"
    , "web-html"
    ]
  , repo = "https://github.com/purescript-web/purescript-web-xhr.git"
  , version = "v4.0.0"
  }
, which =
  { dependencies = [ "arrays", "console", "effect", "nullable", "psci-support" ]
  , repo = "https://github.com/maxdeviant/purescript-which.git"
  , version = "v0.1.0"
  }
, wire =
  { dependencies = [ "aff", "filterable", "refs", "unsafe-reference" ]
  , repo = "https://github.com/robertdp/purescript-wire.git"
  , version = "v0.5.0"
  }
, yargs =
  { dependencies =
    [ "console", "either", "exceptions", "foreign", "unsafe-coerce" ]
  , repo = "https://github.com/paf31/purescript-yargs.git"
  , version = "v4.0.0"
  }
, zeta =
  { dependencies = [ "refs", "aff", "foreign-object", "arrays" ]
  , repo = "https://github.com/athanclark/purescript-zeta.git"
  , version = "v6.0.0"
  }
, zeta-extra =
  { dependencies = [ "js-timers", "web-html", "zeta" ]
  , repo = "https://github.com/athanclark/purescript-zeta-extra.git"
  , version = "v0.0.1"
  }
}
