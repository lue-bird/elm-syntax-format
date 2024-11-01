module Elm.Parser.ImportsTests exposing (all)

import Elm.Parser.Imports
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Exposing
import Elm.Syntax.Import
import Elm.Syntax.Node
import Expect
import Test


all : Test.Test
all =
    Test.describe "ImportTest"
        [ Test.test "import with explicits"
            (\() ->
                "import Foo exposing (Model, Msg(..))"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Just
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 37 } }
                                        (Elm.Syntax.Exposing.Explicit
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 27 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Model"), Elm.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 36 } } (Elm.Syntax.Exposing.TypeExpose { name = "Msg", open = Just { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } }) ]
                                        )
                                    )
                            }
                        )
            )
        , Test.test "import with explicits 2"
            (\() ->
                "import Html exposing (text)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Just
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 28 } }
                                        (Elm.Syntax.Exposing.Explicit
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 27 } } (Elm.Syntax.Exposing.FunctionExpose "text") ]
                                        )
                                    )
                            }
                        )
            )
        , Test.test "import minimal"
            (\() ->
                "import Foo"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList = Nothing
                            }
                        )
            )
        , Test.test "import with alias"
            (\() ->
                "import Foo as Bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                            , exposingList = Nothing
                            }
                        )
            )
        , Test.test "import with alias and exposing all"
            (\() ->
                "import Foo as Bar exposing (..)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                            , exposingList =
                                Just
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                        (Elm.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                    )
                            }
                        )
            )
        , Test.test "import with invalid alias containing ."
            (\() ->
                "import Foo as Bar.Buzz"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid Elm.Parser.Imports.importDefinition
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Import.Import -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst Elm.Parser.Imports.importDefinition
