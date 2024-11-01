module Elm.Parser.ExpressionTests exposing (all)

import Elm.Parser.Expression
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import ElmSyntaxParserLenient
import Expect
import Test


all : Test.Test
all =
    Test.describe "ExpressionTests"
        [ Test.test "empty"
            (\() ->
                "a = "
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "Integer literal"
            (\() ->
                "101"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 101))
            )
        , Test.test "Hex integer literal"
            (\() ->
                "0x56"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.Hex 86))
            )
        , Test.test "String literal"
            (\() ->
                "\"Bar\""
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.Literal "Bar"))
            )
        , Test.test "character literal"
            (\() ->
                "'c'"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.CharLiteral 'c'))
            )
        , Test.test "tuple expression"
            (\() ->
                "(1,2)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (Elm.Syntax.Expression.TupledExpression
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.Integer 1)
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.Integer 2)
                                ]
                            )
                        )
            )
        , Test.test "triple expression"
            (\() ->
                "(1,2,3)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Elm.Syntax.Expression.TupledExpression
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.Integer 1)
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.Integer 2)
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.Integer 3)
                                ]
                            )
                        )
            )
        , Test.test "tuple expression with spaces"
            (\() ->
                "( 1  ,  2 )"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Elm.Syntax.Expression.TupledExpression
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 1)
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 2)
                                ]
                            )
                        )
            )
        , Test.test "4-tuple expression is invalid"
            (\() ->
                "a = (1,2,3,4)"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "String literal multiline"
            (\() ->
                "\"\"\"Bar foo \n a\"\"\""
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Elm.Syntax.Expression.Literal "Bar foo \n a"))
            )
        , Test.test "Regression test for multiline strings with backslashes"
            (\() ->
                "a = \"\"\"\\{\\}\"\"\""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "Regression test 2 for multiline strings with backslashes"
            (\() ->
                "\"\"\"\\\\{\\\\}\"\"\""
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Elm.Syntax.Expression.Literal "\\{\\}"))
            )
        , Test.test "Regression test 3 for multiline strings with backslashes"
            (\() ->
                "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Elm.Syntax.Expression.Literal "\\a-blablabla-\\b"))
            )
        , Test.test "Type expression for upper case"
            (\() ->
                "Bar"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "Bar"))
            )
        , Test.test "Type expression for lower case"
            (\() ->
                "bar"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
            )
        , Test.test "Type expression for lower case but qualified"
            (\() ->
                "Bar.foo"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [ "Bar" ] "foo"))
            )
        , Test.test "parenthesizedExpression"
            (\() ->
                "(bar)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (Elm.Syntax.Expression.ParenthesizedExpression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                            )
                        )
            )
        , Test.test "parenthesized expression starting with a negation"
            (\() ->
                "(-1 * sign)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Elm.Syntax.Expression.ParenthesizedExpression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }
                                    (Elm.Syntax.Expression.OperatorApplication "*"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                            (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 1)))
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "sign"))
                                    )
                                )
                            )
                        )
            )
        , Test.test "application expression"
            (\() ->
                "List.concat []"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "concat")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } (Elm.Syntax.Expression.ListExpr [])
                                ]
                            )
                        )
            )
        , Test.test "Binary operation"
            (\() ->
                "model + 1"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Elm.Syntax.Expression.OperatorApplication "+"
                                Elm.Syntax.Infix.Left
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "model"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 1))
                            )
                        )
            )
        , Test.test "Nested binary operations (+ and ==)"
            (\() ->
                "count + 1 == 1"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Elm.Syntax.Expression.OperatorApplication "=="
                                Elm.Syntax.Infix.Non
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "count"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 1))
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Elm.Syntax.Expression.Integer 1))
                            )
                        )
            )
        , Test.test "Nested binary operations (+ and /=)"
            (\() ->
                "count + 1 /= 1"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Elm.Syntax.Expression.OperatorApplication "/="
                                Elm.Syntax.Infix.Non
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "count"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 1))
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Elm.Syntax.Expression.Integer 1))
                            )
                        )
            )
        , Test.test "Nested binary operations (+ and //)"
            (\() ->
                "count + 1 // 2"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Elm.Syntax.Expression.OperatorApplication "+"
                                Elm.Syntax.Infix.Left
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "count"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                    (Elm.Syntax.Expression.OperatorApplication "//"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 1))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Elm.Syntax.Expression.Integer 2))
                                    )
                                )
                            )
                        )
            )
        , Test.test "Nested binary operations (&& and <|)"
            (\() ->
                "condition && condition <| f"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            (Elm.Syntax.Expression.OperatorApplication "<|"
                                Elm.Syntax.Infix.Right
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                    (Elm.Syntax.Expression.OperatorApplication "&&"
                                        Elm.Syntax.Infix.Right
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "condition"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } } (Elm.Syntax.Expression.FunctionOrValue [] "condition"))
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } (Elm.Syntax.Expression.FunctionOrValue [] "f"))
                            )
                        )
            )
        , Test.test "application expression 2"
            (\() ->
                "(\"\", always (List.concat [ [ fileName ], [] ]))"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } }
                            (Elm.Syntax.Expression.TupledExpression
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Literal "")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } }
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "always")
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } }
                                            (Elm.Syntax.Expression.ParenthesizedExpression
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } }
                                                    (Elm.Syntax.Expression.Application
                                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "concat")
                                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } }
                                                            (Elm.Syntax.Expression.ListExpr
                                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } }
                                                                    (Elm.Syntax.Expression.ListExpr
                                                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } } (Elm.Syntax.Expression.FunctionOrValue [] "fileName")
                                                                        ]
                                                                    )
                                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } } (Elm.Syntax.Expression.ListExpr [])
                                                                ]
                                                            )
                                                        ]
                                                    )
                                                )
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
            )
        , Test.test "expressionNotApplication simple"
            (\() ->
                "foo"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
            )
        , Test.test "unit application"
            (\() ->
                "Task.succeed ()"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Elm.Syntax.Expression.FunctionOrValue [ "Task" ] "succeed")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } Elm.Syntax.Expression.UnitExpr
                                ]
                            )
                        )
            )
        , Test.test "Function call"
            (\() ->
                "foo bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                ]
                            )
                        )
            )
        , Test.test "Function call with argument badly indented"
            (\() ->
                "a = foo\nbar"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "ifBlockExpression"
            (\() ->
                "if True then foo else bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            (Elm.Syntax.Expression.IfBlock
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                            )
                        )
            )
        , Test.test "nestedIfExpression"
            (\() ->
                "if True then if False then foo else baz else bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 49 } }
                            (Elm.Syntax.Expression.IfBlock
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 40 } }
                                    (Elm.Syntax.Expression.IfBlock
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } (Elm.Syntax.Expression.FunctionOrValue [] "False"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 40 } } (Elm.Syntax.Expression.FunctionOrValue [] "baz"))
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 46 }, end = { row = 1, column = 49 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                            )
                        )
            )
        , Test.test "recordExpression"
            (\() ->
                "{ model = 0, view = view, update = update }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 44 } }
                            (Elm.Syntax.Expression.RecordExpr
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.Integer 0)
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "view"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } (Elm.Syntax.Expression.FunctionOrValue [] "view")
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 43 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } } "update"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 42 } } (Elm.Syntax.Expression.FunctionOrValue [] "update")
                                    )
                                ]
                            )
                        )
            )
        , Test.test "recordExpression with comment"
            (\() ->
                "{ foo = 1 -- bar\n , baz = 2 }"
                    |> expectAstWithComments
                        { ast =
                            Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
                                (Elm.Syntax.Expression.RecordExpr
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 10 } }
                                        ( Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 1)
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 12 } }
                                        ( Elm.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 7 } } "baz"
                                        , Elm.Syntax.Node.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } (Elm.Syntax.Expression.Integer 2)
                                        )
                                    ]
                                )
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } "-- bar" ]
                        }
            )
        , Test.test "listExpression"
            (\() ->
                "[ class \"a\", text \"Foo\"]"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            (Elm.Syntax.Expression.ListExpr
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "class")
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.Literal "a")
                                        ]
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (Elm.Syntax.Expression.FunctionOrValue [] "text")
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (Elm.Syntax.Expression.Literal "Foo")
                                        ]
                                    )
                                ]
                            )
                        )
            )
        , Test.test "listExpression singleton with comment"
            (\() ->
                "[ 1 {- Foo-} ]"
                    |> expectAstWithComments
                        { ast =
                            Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (Elm.Syntax.Expression.ListExpr
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 1)
                                    ]
                                )
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                        }
            )
        , Test.test "listExpression empty with comment"
            (\() ->
                "[{- Foo -}]"
                    |> expectAstWithComments
                        { ast = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.ListExpr [])
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                        }
            )
        , Test.test "qualified expression"
            (\() ->
                "Html.text"
                    |> expectAst (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [ "Html" ] "text"))
            )
        , Test.test "record access"
            (\() ->
                "foo.bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Elm.Syntax.Expression.RecordAccess
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                            )
                        )
            )
        , Test.test "multiple record access operations"
            (\() ->
                "foo.bar.baz"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Elm.Syntax.Expression.RecordAccess
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                    (Elm.Syntax.Expression.RecordAccess
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "baz")
                            )
                        )
            )
        , Test.test "multiple record access operations with module name"
            (\() ->
                "A.B.foo.bar.baz"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Elm.Syntax.Expression.RecordAccess
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                    (Elm.Syntax.Expression.RecordAccess
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [ "A", "B" ] "foo"))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "bar")
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } "baz")
                            )
                        )
            )
        , Test.test "record update"
            (\() ->
                "{ model | count = 1, loading = True }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (Elm.Syntax.Expression.RecordUpdateExpression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Elm.Syntax.Expression.Integer 1)
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (Elm.Syntax.Expression.FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
            )
        , Test.test "record update no spacing"
            (\() ->
                "{model| count = 1, loading = True }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            (Elm.Syntax.Expression.RecordUpdateExpression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model")
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } } "count"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Elm.Syntax.Expression.Integer 1)
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } } "loading"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (Elm.Syntax.Expression.FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
            )
        , Test.test "record access as function"
            (\() ->
                "List.map .name people"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Elm.Syntax.Expression.RecordAccessFunction ".name")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } } (Elm.Syntax.Expression.FunctionOrValue [] "people")
                                ]
                            )
                        )
            )
        , Test.test "record access direct"
            (\() ->
                "(.spaceEvenly Internal.Style.classes)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (Elm.Syntax.Expression.ParenthesizedExpression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                    (Elm.Syntax.Expression.Application
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } } (Elm.Syntax.Expression.RecordAccessFunction ".spaceEvenly")
                                        , Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } } (Elm.Syntax.Expression.FunctionOrValue [ "Internal", "Style" ] "classes")
                                        ]
                                    )
                                )
                            )
                        )
            )
        , Test.test "positive integer should be invalid"
            (\() ->
                "a = +1"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "expression ending with an operator should not be valid"
            (\() ->
                "a = 1++"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "multiple < in a row should not be valid"
            (\() ->
                "z = a < b < c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "multiple > in a row should not be valid"
            (\() ->
                "z = a > b > c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "multiple == in a row should not be valid"
            (\() ->
                "z = a == b == c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "multiple /= in a row should not be valid"
            (\() ->
                "z = a /= b /= c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "multiple >= in a row should not be valid"
            (\() ->
                "z = a >= b >= c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "multiple <= in a row should not be valid"
            (\() ->
                "z = a <= b <= c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "mixing comparison operators without parenthesis should not be valid"
            (\() ->
                "z = a < b == c"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )

        -- TODO introduce validation step for
        -- , test "<| followed by |> operation without parenthesis should not be valid" <|
        --     \() ->
        --         "z = a <| b |> c"
        --             |> ParserWithCommentsUtil.expectInvalid ElmSyntaxParserLenient.declaration
        -- , test "|> followed by <| operation without parenthesis should not be valid" <|
        --     \() ->
        --         "z = a |> b <| c"
        --             |> ParserWithCommentsUtil.expectInvalid ElmSyntaxParserLenient.declaration
        -- , test "<< followed by >> operation without parenthesis should not be valid" <|
        --     \() ->
        --         "z = a << b >> c"
        --             |> ParserWithCommentsUtil.expectInvalid ElmSyntaxParserLenient.declaration
        -- , test ">> followed by << operation without parenthesis should not be valid" <|
        --     \() ->
        --         "z = a >> b << c"
        --             |> ParserWithCommentsUtil.expectInvalid ElmSyntaxParserLenient.declaration
        , Test.test "prefix notation"
            (\() ->
                "(::) x"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.PrefixOperator "::")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                ]
                            )
                        )
            )
        , Test.test "subtraction without spaces"
            (\() ->
                "2-1"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            (Elm.Syntax.Expression.OperatorApplication "-"
                                Elm.Syntax.Infix.Left
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Expression.Integer 2))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 1))
                            )
                        )
            )
        , Test.test "negated expression for value"
            (\() ->
                "-x"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")))
                        )
            )
        , Test.test "negated expression in application"
            (\() ->
                "toFloat -5"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "toFloat")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                    (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.Integer 5)))
                                ]
                            )
                        )
            )
        , Test.test "negated expression for parenthesized"
            (\() ->
                "-(x - y)"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            (Elm.Syntax.Expression.Negation
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 9 } }
                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                            (Elm.Syntax.Expression.OperatorApplication "-"
                                                Elm.Syntax.Infix.Left
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "x"))
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "y"))
                                            )
                                        )
                                    )
                                )
                            )
                        )
            )
        , Test.test "negated expression with other operations"
            (\() ->
                "-1 + -10 * -100^2 == -100001"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                            (Elm.Syntax.Expression.OperatorApplication "=="
                                Elm.Syntax.Infix.Non
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                                            (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.Integer 1)))
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 18 } }
                                            (Elm.Syntax.Expression.OperatorApplication "*"
                                                Elm.Syntax.Infix.Left
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } }
                                                    (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (Elm.Syntax.Expression.Integer 10)))
                                                )
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 18 } }
                                                    (Elm.Syntax.Expression.OperatorApplication "^"
                                                        Elm.Syntax.Infix.Right
                                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 16 } }
                                                            (Elm.Syntax.Expression.Negation
                                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } (Elm.Syntax.Expression.Integer 100))
                                                            )
                                                        )
                                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Elm.Syntax.Expression.Integer 2))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } }
                                    (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 29 } } (Elm.Syntax.Expression.Integer 100001)))
                                )
                            )
                        )
            )
        , Test.test "plus and minus in the same expression"
            (\() ->
                "1 + 2 - 3"
                    |> expectAst
                        (Elm.Syntax.Node.Node
                            { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Elm.Syntax.Expression.OperatorApplication "-"
                                Elm.Syntax.Infix.Left
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                        Elm.Syntax.Infix.Left
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Expression.Integer 1))
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.Integer 2))
                                    )
                                )
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.Integer 3))
                            )
                        )
            )
        , Test.test "pipe operation"
            (\() ->
                "a |> b"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (Elm.Syntax.Expression.OperatorApplication "|>"
                                Elm.Syntax.Infix.Left
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Expression.FunctionOrValue [] "a"))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "b"))
                            )
                        )
            )
        , Test.test "function with higher order"
            (\() ->
                "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "chompWhile")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                            (Elm.Syntax.Expression.LambdaExpression
                                                { args = [ Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Elm.Syntax.Pattern.VarPattern "c") ]
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                        (Elm.Syntax.Expression.OperatorApplication "||"
                                                            Elm.Syntax.Infix.Right
                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 27 } }
                                                                (Elm.Syntax.Expression.OperatorApplication "=="
                                                                    Elm.Syntax.Infix.Non
                                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Elm.Syntax.Expression.FunctionOrValue [] "c"))
                                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (Elm.Syntax.Expression.CharLiteral ' '))
                                                                )
                                                            )
                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 53 } }
                                                                (Elm.Syntax.Expression.OperatorApplication "||"
                                                                    Elm.Syntax.Infix.Right
                                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 40 } }
                                                                        (Elm.Syntax.Expression.OperatorApplication "=="
                                                                            Elm.Syntax.Infix.Non
                                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } (Elm.Syntax.Expression.FunctionOrValue [] "c"))
                                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } }
                                                                                (Elm.Syntax.Expression.CharLiteral '\n')
                                                                            )
                                                                        )
                                                                    )
                                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 53 } }
                                                                        (Elm.Syntax.Expression.OperatorApplication "=="
                                                                            Elm.Syntax.Infix.Non
                                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } } (Elm.Syntax.Expression.FunctionOrValue [] "c"))
                                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } }
                                                                                (Elm.Syntax.Expression.CharLiteral '\u{000D}')
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                }
                                            )
                                        )
                                    )
                                ]
                            )
                        )
            )
        , Test.test "application should be lower-priority than field access"
            (\() ->
                "foo { d | b = f x y }.b"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 24 } }
                                    (Elm.Syntax.Expression.RecordAccess
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                            (Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "d")
                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 21 } }
                                                    ( Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } "b"
                                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }
                                                        (Elm.Syntax.Expression.Application
                                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Elm.Syntax.Expression.FunctionOrValue [] "y")
                                                            ]
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 24 } } "b")
                                    )
                                ]
                            )
                        )
            )
        , Test.test "should not consider a negative number parameter as the start of a new application"
            (\() ->
                "Random.list -1 generator"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            (Elm.Syntax.Expression.Application
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [ "Random" ] "list")
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }
                                    (Elm.Syntax.Expression.Negation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                            (Elm.Syntax.Expression.Integer 1)
                                        )
                                    )
                                , Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 25 } } (Elm.Syntax.Expression.FunctionOrValue [] "generator")
                                ]
                            )
                        )
            )
        , Test.test "negation can be applied on record access"
            (\() ->
                "1 + -{x = 10}.x"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Elm.Syntax.Expression.OperatorApplication "+"
                                Elm.Syntax.Infix.Left
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Expression.Integer 1))
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 16 } }
                                    (Elm.Syntax.Expression.Negation
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 16 } }
                                            (Elm.Syntax.Expression.RecordAccess
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                                    (Elm.Syntax.Expression.RecordExpr
                                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 13 } }
                                                            ( Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "x"
                                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } } (Elm.Syntax.Expression.Integer 10)
                                                            )
                                                        ]
                                                    )
                                                )
                                                (Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } "x")
                                            )
                                        )
                                    )
                                )
                            )
                        )
            )
        , Test.test "fail if condition not positively indented"
            (\() ->
                """a =
    let
        x =
            if
        f y then  1 else 0
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if `then` not positively indented"
            (\() ->
                """a =
    let
        x =
            if True
       then  1 else 0
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if if-true-branch not positively indented"
            (\() ->
                """a =
    let
        x =
            if True then
        1   else 0
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if `else` not positively indented"
            (\() ->
                """a =
    let
        x =
            if True then 1
       else 0
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if if-false-branch not positively indented"
            (\() ->
                """ a =
    let
        x =
            if True then 1 else
        0
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if record closing curly not positively indented"
            (\() ->
                """a =
    let
        x =
            { a = 0, b = 1
        }
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if record field value not positively indented"
            (\() ->
                """a =
    let
        x =
            { a = 0, b =
        1 }
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if record field name not positively indented"
            (\() ->
                """a =
    let
        x =
            { a = 0,
        b       = 1 }
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if record field `=` not positively indented"
            (\() ->
                """a =
    let
        x =
            { a = 0, b
        =         1 }
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if tuple closing parens not positively indented"
            (\() ->
                """a =
    let
        x =
            ( 0, 1
        )
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if first tuple part not positively indented"
            (\() ->
                """a =
    let
        x =
            (
        0   , 1
            )
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if second tuple part not positively indented"
            (\() ->
                """a =
    let
        x =
            ( 0,
        1   )
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if operator not positively indented"
            (\() ->
                """a =
    let
        x =
            0
        + 1
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if function call argument not positively indented"
            (\() ->
                """a =
    let
        x =
            f 0
        1
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if lambda result not positively indented"
            (\() ->
                """a =
    let
        x =
            \\y ->
        y
    in
    x"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        , Test.test "fail if case branch result call argument not positively indented"
            (\() ->
                """foo = 
    case Nothing of
        Nothing -> a
  b"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst Elm.Parser.Expression.expressionFollowedByOptimisticLayout


expectAstWithComments : { ast : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression, comments : List (Elm.Syntax.Node.Node String) } -> String -> Expect.Expectation
expectAstWithComments =
    Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments Elm.Parser.Expression.expressionFollowedByOptimisticLayout
