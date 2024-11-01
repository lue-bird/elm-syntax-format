module Elm.Parser.PatternTests exposing (all)

import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Parser.Patterns
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Expect
import Test


all : Test.Test
all =
    Test.describe "PatternTests"
        [ Test.test "Unit"
            (\() ->
                "()"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.Pattern.UnitPattern)
            )
        , Test.test "Unit with inner layout"
            (\() ->
                """(
   -- comment
   )"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments Elm.Parser.Patterns.pattern
                        { ast = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } } Elm.Syntax.Pattern.UnitPattern
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } } "-- comment" ]
                        }
            )
        , Test.test "String"
            (\() ->
                "\"Foo\""
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.StringPattern "Foo"))
            )
        , Test.test "Char"
            (\() ->
                "'f'"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.CharPattern 'f'))
            )
        , Test.test "Wildcard"
            (\() ->
                "_"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } Elm.Syntax.Pattern.AllPattern)
            )
        , Test.test "Parenthesized"
            (\() ->
                "(x)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            (Elm.Syntax.Pattern.ParenthesizedPattern
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.VarPattern "x"))
                            )
                        )
            )
        , Test.test "Int"
            (\() ->
                "1"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.IntPattern 1))
            )
        , Test.test "Hex int"
            (\() ->
                "0x1"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.HexPattern 1))
            )
        , Test.test "Float should not be valid" (\() -> expectInvalid "1.0")
        , Test.test "Uncons"
            (\() ->
                "n :: tail"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.VarPattern "n"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } (Elm.Syntax.Pattern.VarPattern "tail"))
                            )
                        )
            )
        , Test.test "Uncons multiple"
            (\() ->
                "a :: b :: cUp"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (Elm.Syntax.Pattern.UnConsPattern
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.VarPattern "a"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                    (Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Pattern.VarPattern "b"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (Elm.Syntax.Pattern.VarPattern "cUp"))
                                    )
                                )
                            )
                        )
            )
        , Test.test "Uncons with parens"
            (\() ->
                "(X x) :: xs"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Elm.Syntax.Pattern.UnConsPattern
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                    (Elm.Syntax.Pattern.ParenthesizedPattern
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                            (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "X" }
                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.VarPattern "x") ]
                                            )
                                        )
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } (Elm.Syntax.Pattern.VarPattern "xs"))
                            )
                        )
            )
        , Test.test "Empty list"
            (\() ->
                "[]"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.ListPattern []))
            )
        , Test.test "Empty list pattern with whitespace"
            (\() ->
                "[ ]"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern []))
            )
        , Test.test "Single element list"
            (\() ->
                "[1]"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
            )
        , Test.test "Single element list with trailing whitespace"
            (\() ->
                "[1 ]"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
            )
        , Test.test "Single element list with leading whitespace"
            (\() ->
                "[ 1]"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
            )
        , Test.test "Empty record"
            (\() ->
                "{}"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.RecordPattern []))
            )
        , Test.test "Empty record with whitespace"
            (\() ->
                "{ }"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.RecordPattern []))
            )
        , Test.test "Record"
            (\() ->
                "{a,b}"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (Elm.Syntax.Pattern.RecordPattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } "b"
                                ]
                            )
                        )
            )
        , Test.test "Record pattern with whitespace"
            (\() ->
                "{a , b}"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Elm.Syntax.Pattern.RecordPattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                ]
                            )
                        )
            )
        , Test.test "Record pattern with trailing whitespace"
            (\() ->
                "{a }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (Elm.Syntax.Pattern.RecordPattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                            )
                        )
            )
        , Test.test "Record pattern with leading whitespace"
            (\() ->
                "{ a}"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (Elm.Syntax.Pattern.RecordPattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                            )
                        )
            )
        , Test.test "Named"
            (\() ->
                "True"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                        )
            )
        , Test.test "Named pattern without and with spacing should parse to the same"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parse "Bar " Elm.Parser.Patterns.pattern
                    |> Expect.equal (Elm.Parser.ParserWithCommentsTestUtil.parse "Bar" Elm.Parser.Patterns.pattern)
            )
        , Test.test "Qualified named"
            (\() ->
                "Basics.True"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Elm.Syntax.Pattern.NamedPattern { moduleName = [ "Basics" ], name = "True" } [])
                        )
            )
        , Test.test "Named pattern with data"
            (\() ->
                "Set x"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (Elm.Syntax.Pattern.NamedPattern
                                { moduleName = [], name = "Set" }
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.VarPattern "x") ]
                            )
                        )
            )
        , Test.test "Qualified named pattern with data"
            (\() ->
                "Set.Set x"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Elm.Syntax.Pattern.NamedPattern
                                { moduleName = [ "Set" ], name = "Set" }
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Pattern.VarPattern "x") ]
                            )
                        )
            )
        , Test.test "Tuple"
            (\() ->
                "(model, cmd)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Elm.Syntax.Pattern.TuplePattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Elm.Syntax.Pattern.VarPattern "model")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Elm.Syntax.Pattern.VarPattern "cmd")
                                ]
                            )
                        )
            )
        , Test.test "4-tuple pattern is invalid"
            (\() ->
                "(1,2,3,4)"
                    |> expectInvalid
            )
        , Test.test "Nested tuple"
            (\() ->
                "(a,{b,c},())"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Elm.Syntax.Pattern.TuplePattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.VarPattern "a")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 9 } } (Elm.Syntax.Pattern.RecordPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "b", Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "c" ])
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } Elm.Syntax.Pattern.UnitPattern
                                ]
                            )
                        )
            )
        , Test.test "As pattern"
            (\() ->
                "x as y"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (Elm.Syntax.Pattern.AsPattern
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.VarPattern "x"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "y")
                            )
                        )
            )
        , Test.test "should fail to parse when right side is not a direct variable name"
            (\() ->
                "x as (y)"
                    |> expectInvalid
            )
        , Test.test "should fail to parse consecutive as"
            (\() ->
                "x as y as z"
                    |> expectInvalid
            )
        , Test.test "should fail to parse :: after as"
            (\() ->
                "x as y :: z"
                    |> expectInvalid
            )
        , Test.test "should fail to parse :: after as even when :: was already used before"
            (\() ->
                "w :: x as y :: z"
                    |> expectInvalid
            )
        , Test.test "should fail to parse when right side is an invalid variable name"
            (\() ->
                "x as _y"
                    |> expectInvalid
            )
        , Test.test "should fail to parse when right side is not a variable name"
            (\() ->
                "x as 1"
                    |> expectInvalid
            )
        , Test.test "Record as"
            (\() ->
                "{model,context} as appState"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            (Elm.Syntax.Pattern.AsPattern
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                    (Elm.Syntax.Pattern.RecordPattern
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model"
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } "context"
                                        ]
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } "appState")
                            )
                        )
            )
        , Test.test "Complex"
            (\() ->
                "(Index irec as index, docVector)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            (Elm.Syntax.Pattern.TuplePattern
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 21 } }
                                    (Elm.Syntax.Pattern.AsPattern
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 12 } }
                                            (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Index" }
                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } (Elm.Syntax.Pattern.VarPattern "irec") ]
                                            )
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 21 } } "index")
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 32 } } (Elm.Syntax.Pattern.VarPattern "docVector")
                                ]
                            )
                        )
            )
        , Test.test "Complex pattern 2"
            (\() ->
                "RBNode_elm_builtin col (RBNode_elm_builtin Red  (RBNode_elm_builtin Red xv))"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 77 } }
                            (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "RBNode_elm_builtin" }
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } (Elm.Syntax.Pattern.VarPattern "col")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 77 } }
                                    (Elm.Syntax.Pattern.ParenthesizedPattern
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 76 } }
                                            (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "RBNode_elm_builtin" }
                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Red" } [])
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 76 } } (Elm.Syntax.Pattern.ParenthesizedPattern (Elm.Syntax.Node.Node { start = { row = 1, column = 50 }, end = { row = 1, column = 75 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "RBNode_elm_builtin" } [ Elm.Syntax.Node.Node { start = { row = 1, column = 69 }, end = { row = 1, column = 72 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Red" } []), Elm.Syntax.Node.Node { start = { row = 1, column = 73 }, end = { row = 1, column = 75 } } (Elm.Syntax.Pattern.VarPattern "xv") ])))
                                                ]
                                            )
                                        )
                                    )
                                ]
                            )
                        )
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst Elm.Parser.Patterns.pattern


expectInvalid : String -> Expect.Expectation
expectInvalid =
    Elm.Parser.ParserWithCommentsTestUtil.expectInvalid Elm.Parser.Patterns.pattern
