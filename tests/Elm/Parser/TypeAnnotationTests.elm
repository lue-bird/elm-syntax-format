module Elm.Parser.TypeAnnotationTests exposing (all)

import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Parser.TypeAnnotation
import Elm.Syntax.Node
import Elm.Syntax.TypeAnnotation
import Expect
import Test


all : Test.Test
all =
    Test.describe "TypeAnnotationTests"
        [ Test.test "unitTypeReference"
            (\() ->
                "()"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.TypeAnnotation.Unit)
            )
        , Test.test "unitTypeReference with spaces"
            (\() ->
                "( )"
                    |> expectInvalid
            )
        , Test.test "tupledTypeReference"
            (\() ->
                "( (), ())"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Elm.Syntax.TypeAnnotation.Tupled
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 5 } } Elm.Syntax.TypeAnnotation.Unit
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } Elm.Syntax.TypeAnnotation.Unit
                                ]
                            )
                        )
            )
        , Test.test "4-tuple type annotation is invalid"
            (\() ->
                "(Int,String,(),a)"
                    |> expectInvalid
            )
        , Test.test "tupledTypeReference 2"
            (\() ->
                -- TODO This feels incorrect, there should be a Parenthesized type for this
                "( () )"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } Elm.Syntax.TypeAnnotation.Unit)
            )
        , Test.test "tupledTypeReference 3"
            (\() ->
                "( () , Maybe m )"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            (Elm.Syntax.TypeAnnotation.Tupled
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 5 } } Elm.Syntax.TypeAnnotation.Unit
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                    (Elm.Syntax.TypeAnnotation.Typed
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } } ( [], "Maybe" ))
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Elm.Syntax.TypeAnnotation.GenericType "m") ]
                                    )
                                ]
                            )
                        )
            )
        , Test.test "qualified type reference"
            (\() ->
                "Foo.Bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                        )
            )
        , Test.test "typeAnnotationNoFn"
            (\() ->
                "Bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                        )
            )
        , Test.test "typedTypeReference 1"
            (\() ->
                "Foo () a Bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } Elm.Syntax.TypeAnnotation.Unit
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (Elm.Syntax.TypeAnnotation.GenericType "a")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                ]
                            )
                        )
            )
        , Test.test "typedTypeReference 2"
            (\() ->
                "Foo () a Bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } Elm.Syntax.TypeAnnotation.Unit
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (Elm.Syntax.TypeAnnotation.GenericType "a")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                ]
                            )
                        )
            )
        , Test.test "recordTypeReference empty"
            (\() ->
                "{}"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.TypeAnnotation.Record []))
            )
        , Test.test "recordTypeReference one field"
            (\() ->
                "{color: String }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            (Elm.Syntax.TypeAnnotation.Record
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                    )
                                ]
                            )
                        )
            )
        , Test.test "record with generic"
            (\() ->
                "{ attr | position : Vec2, texture : Vec2 }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                            (Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                        ( Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                        ( Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
                                        )
                                    ]
                                )
                            )
                        )
            )
        , Test.test "generic record with no fields"
            (\() ->
                "{ attr |}"
                    |> expectInvalid
            )
        , Test.test "recordTypeReference nested record"
            (\() ->
                "{color: {r : Int, g :Int, b: Int } }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            (Elm.Syntax.TypeAnnotation.Record
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 35 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 35 } }
                                        (Elm.Syntax.TypeAnnotation.Record
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 17 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "r"
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } ( [], "Int" )) [])
                                                )
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } "g"
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } ( [], "Int" )) [])
                                                )
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } "b"
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } } ( [], "Int" )) [])
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
            )
        , Test.test "record field ranges"
            (\() ->
                "{ foo : Int, bar : Int, baz : Int }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            (Elm.Syntax.TypeAnnotation.Record
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Int" )) [])
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } "bar"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } ( [], "Int" )) [])
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 35 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } "baz"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } ( [], "Int" )) [])
                                    )
                                ]
                            )
                        )
            )
        , Test.test "recordTypeReference with generic"
            (\() ->
                "{color: s }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Elm.Syntax.TypeAnnotation.Record
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 10 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.TypeAnnotation.GenericType "s")
                                    )
                                ]
                            )
                        )
            )
        , Test.test "function type reference"
            (\() ->
                "Foo -> Bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                )
                            )
                        )
            )
        , Test.test "function type reference multiple"
            (\() ->
                "Foo -> Bar -> baz"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } (Elm.Syntax.TypeAnnotation.GenericType "baz"))
                                    )
                                )
                            )
                        )
            )
        , Test.test "function type reference generics"
            (\() ->
                "cMsg -> cModel -> a"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.TypeAnnotation.GenericType "cMsg"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 20 } }
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } (Elm.Syntax.TypeAnnotation.GenericType "cModel"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Elm.Syntax.TypeAnnotation.GenericType "a"))
                                    )
                                )
                            )
                        )
            )
        , Test.test "annotation with parens"
            (\() ->
                "Msg -> Model -> (Model, Cmd Msg)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Msg" )) [])
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 33 } }
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } }
                                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } } ( [], "Model" )) [])
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                            (Elm.Syntax.TypeAnnotation.Tupled
                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 23 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 23 } } ( [], "Model" )) [])
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } ( [], "Cmd" ))
                                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } }
                                                            (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } } ( [], "Msg" )) [])
                                                        ]
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
                        )
            )
        , Test.test "function as argument"
            (\() ->
                "( cMsg -> cModel -> a ) -> b"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } (Elm.Syntax.TypeAnnotation.GenericType "cMsg"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 22 } }
                                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } (Elm.Syntax.TypeAnnotation.GenericType "cModel"))
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (Elm.Syntax.TypeAnnotation.GenericType "a"))
                                            )
                                        )
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Elm.Syntax.TypeAnnotation.GenericType "b"))
                            )
                        )
            )
        , Test.test "type with params"
            (\() ->
                "(Foo -> Bar)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                    (Elm.Syntax.TypeAnnotation.Typed
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } ( [], "Foo" ))
                                        []
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                    (Elm.Syntax.TypeAnnotation.Typed
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Bar" ))
                                        []
                                    )
                                )
                            )
                        )
            )
        , Test.test "function type reference multiple and parens"
            (\() ->
                "(Foo -> Bar) -> baz"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                    (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } ( [], "Foo" ))
                                                []
                                            )
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Bar" ))
                                                []
                                            )
                                        )
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 20 } } (Elm.Syntax.TypeAnnotation.GenericType "baz"))
                            )
                        )
            )
        , Test.test "parseTypeWith wrong indent"
            (\() ->
                "Maybe\na"
                    |> expectInvalid
            )
        , Test.test "parseTypeWith good indent"
            (\() ->
                "Maybe\n a"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 3 } }
                            (Elm.Syntax.TypeAnnotation.Typed
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } ( [], "Maybe" ))
                                [ Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 3 } } (Elm.Syntax.TypeAnnotation.GenericType "a") ]
                            )
                        )
            )
        , Test.test "issue #5 - no spaces between type and generic with parens"
            (\() ->
                "List(String)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Elm.Syntax.TypeAnnotation.Typed
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "List" ))
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                    (Elm.Syntax.TypeAnnotation.Typed
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                        []
                                    )
                                ]
                            )
                        )
            )
        , Test.test "parse type with multiple params"
            (\() ->
                "Dict String Int"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Elm.Syntax.TypeAnnotation.Typed
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "Dict" ))
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                    (Elm.Syntax.TypeAnnotation.Typed
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                        []
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } ( [], "Int" )) [])
                                ]
                            )
                        )
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst Elm.Parser.TypeAnnotation.typeAnnotation


expectInvalid : String -> Expect.Expectation
expectInvalid =
    Elm.Parser.ParserWithCommentsTestUtil.expectInvalid Elm.Parser.TypeAnnotation.typeAnnotation
