module Main exposing (main)

import ElmMake
import Html


main =
    let
        _ =
            ElmMake.compiles ""
    in
    Html.text "compiles"
