{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "orecord"
, dependencies =
    [ "lists"
    , "maybe"
    , "proxy"
    , "typelevel-prelude"
    , "unsafe-coerce"
    -- Dev Dependencies
    , "assert"
    , "console"
    , "debug"
    , "effect"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
