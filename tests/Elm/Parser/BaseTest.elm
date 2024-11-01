module Elm.Parser.BaseTest exposing (all)

import Elm.Parser.Base
import Elm.Parser.TestUtil
import Elm.Syntax.Node
import Expect
import Test


all : Test.Test
all =
    Test.describe "BaseTest"
        [ Test.test "moduleName"
            (\() ->
                Elm.Parser.TestUtil.parseToResult "Foo" Elm.Parser.Base.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo" ])
            )
        , Test.test "moduleNameDir"
            (\() ->
                Elm.Parser.TestUtil.parseToResult "Foo.Bar" Elm.Parser.Base.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo", "Bar" ])
            )
        ]
