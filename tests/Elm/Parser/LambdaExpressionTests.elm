module Elm.Parser.LambdaExpressionTests exposing (all)

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
    Test.describe "LambdaExpressionTests"
        [ Test.test "unit lambda"
            (\() ->
                "\\() -> foo"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Elm.Syntax.Expression.LambdaExpression
                                { args = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } Elm.Syntax.Pattern.UnitPattern ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                }
                            )
                        )
            )
        , Test.test "record lambda"
            (\() ->
                "\\{foo} -> foo"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (Elm.Syntax.Expression.LambdaExpression
                                { args = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Elm.Syntax.Pattern.RecordPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo" ]) ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                }
                            )
                        )
            )
        , Test.test "empty record lambda"
            (\() ->
                "\\{} -> foo"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Elm.Syntax.Expression.LambdaExpression
                                { args = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.RecordPattern []) ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                }
                            )
                        )
            )
        , Test.test "args lambda"
            (\() ->
                "\\a b -> a + b"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (Elm.Syntax.Expression.LambdaExpression
                                { args =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.VarPattern "a")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.VarPattern "b")
                                    ]
                                , expression =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                        (Elm.Syntax.Expression.OperatorApplication "+"
                                            Elm.Syntax.Infix.Left
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "a"))
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (Elm.Syntax.Expression.FunctionOrValue [] "b"))
                                        )
                                }
                            )
                        )
            )
        , Test.test "tuple lambda"
            (\() ->
                "\\(a,b) -> a + b"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Elm.Syntax.Expression.LambdaExpression
                                { args =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                        (Elm.Syntax.Pattern.TuplePattern
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.VarPattern "a")
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.VarPattern "b")
                                            ]
                                        )
                                    ]
                                , expression =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }
                                        (Elm.Syntax.Expression.OperatorApplication "+"
                                            Elm.Syntax.Infix.Left
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "a"))
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (Elm.Syntax.Expression.FunctionOrValue [] "b"))
                                        )
                                }
                            )
                        )
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
