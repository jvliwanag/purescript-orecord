{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "orecord"
, dependencies =
    [ "lists", "maybe", "effect", "proxy", "unsafe-coerce", "console", "psci-support", "debug" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
