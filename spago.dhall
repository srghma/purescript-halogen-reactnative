{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-reactnaitve"
, dependencies = [ "console", "debug", "effect", "prelude", "psci-support", "halogen-vdom", "halogen", "event" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
