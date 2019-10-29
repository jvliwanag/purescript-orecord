{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "orecord"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "lists"
    , "maybe"
    , "proxy"
    , "psci-support"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
