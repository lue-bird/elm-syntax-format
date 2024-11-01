module Elm.Parser.CaseExpressionTests exposing (all)

import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import ElmSyntaxParserLenient
import Expect
import Test


all : Test.Test
all =
    Test.describe "CaseExpressionTests"
        [ Test.test "should fail to parse when the matched expression has the wrong indentation"
            (\() ->
                """case
True
  of
    A -> 1"""
                    |> expectInvalid
            )
        , Test.test "should fail to parse when the `of` keyword has the wrong indentation"
            (\() ->
                """case True
of
               A -> 1"""
                    |> expectInvalid
            )
        , Test.test "should fail to parse a branch at the start of a line"
            (\() ->
                """case True of
True -> 1"""
                    |> expectInvalid
            )
        , Test.test "should fail to parse when branch body starts at the start of a line"
            (\() ->
                """case f of
  True ->
1"""
                    |> expectInvalid
            )
        , Test.test "case expression"
            (\() ->
                """case f of
  True -> 1
  False -> 2"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                , cases =
                                    [ ( Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (Elm.Syntax.Expression.Integer 1)
                                      )
                                    , ( Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Elm.Syntax.Expression.Integer 2)
                                      )
                                    ]
                                }
                            )
                        )
            )
        , Test.test "case expression with qualified imports"
            (\() ->
                """case f of
  Foo.Bar -> 1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 15 } }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                , cases =
                                    [ ( Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [ "Foo" ], name = "Bar" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } (Elm.Syntax.Expression.Integer 1)
                                      )
                                    ]
                                }
                            )
                        )
            )
        , Test.test "case expression with no space between pattern and value"
            (\() ->
                """case f of
  x->1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                , cases =
                                    [ ( Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } } (Elm.Syntax.Pattern.VarPattern "x")
                                      , Elm.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } (Elm.Syntax.Expression.Integer 1)
                                      )
                                    ]
                                }
                            )
                        )
            )
        , Test.test "should parse case expression with first branch on the same line as case of"
            (\() ->
                """case x of True -> 1
          False -> 2"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 21 } }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                , cases =
                                    [ ( Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Elm.Syntax.Expression.Integer 1)
                                      )
                                    , ( Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 16 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } } (Elm.Syntax.Expression.Integer 2)
                                      )
                                    ]
                                }
                            )
                        )
            )
        , Test.test "should parse case expression with a multiline pattern"
            (\() ->
                """case x of
        \"\"\"single line triple quote\"\"\" ->
            1
        \"\"\"multi line
            triple quote\"\"\" ->
            2
        _ -> 3"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 15 } }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression =
                                    Elm.Syntax.Node.Node
                                        { start = { row = 1, column = 6 }
                                        , end = { row = 1, column = 7 }
                                        }
                                        (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                , cases =
                                    [ ( Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 39 } } (Elm.Syntax.Pattern.StringPattern "single line triple quote")
                                      , Elm.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Elm.Syntax.Expression.Integer 1)
                                      )
                                    , ( Elm.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 5, column = 28 } } (Elm.Syntax.Pattern.StringPattern "multi line\n            triple quote")
                                      , Elm.Syntax.Node.Node { start = { row = 6, column = 13 }, end = { row = 6, column = 14 } } (Elm.Syntax.Expression.Integer 2)
                                      )
                                    , ( Elm.Syntax.Node.Node { start = { row = 7, column = 9 }, end = { row = 7, column = 10 } } Elm.Syntax.Pattern.AllPattern
                                      , Elm.Syntax.Node.Node { start = { row = 7, column = 14 }, end = { row = 7, column = 15 } } (Elm.Syntax.Expression.Integer 3)
                                      )
                                    ]
                                }
                            )
                        )
            )
        , Test.test "should fail to parse case expression with second branch indented differently than the first line (before)"
            (\() ->
                expectInvalid """case f of
  True -> 1
 False -> 2"""
            )
        , Test.test "should fail to parse case expression with second branch indented differently than the first line (after)"
            (\() ->
                """case f of
  True -> 1
   False -> 2
"""
                    |> expectInvalid
            )
        , Test.test "should parse case expression when "
            (\() ->
                """case msg of
  Increment ->
    1

  Decrement ->
    2"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 6, column = 6 } }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "msg")
                                , cases =
                                    [ ( Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Increment" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (Elm.Syntax.Expression.Integer 1)
                                      )
                                    , ( Elm.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 12 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Decrement" } [])
                                      , Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Elm.Syntax.Expression.Integer 2)
                                      )
                                    ]
                                }
                            )
                        )
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression


expectInvalid : String -> Expect.Expectation
expectInvalid =
    Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.expression
