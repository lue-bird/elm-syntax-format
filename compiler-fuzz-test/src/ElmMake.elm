module ElmMake exposing (compiles)

{-| A huge hack.
-}


{-| As is, this does not work (gives false negatives).

Will be patched after compilation by the wrapped-elm-compiler.js to actually
shell out to `elm make` with a dummy project containing this source code.

That way we can use `elm-test` fuzz testing machinery!

-}
compiles : String -> Bool
compiles elmSourceCode =
    False
