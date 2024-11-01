module Elm.Parser.LetExpressionTests exposing (all)

import Elm.Parser.Expression
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.TypeAnnotation
import Expect
import Test


all : Test.Test
all =
    Test.describe "LetExpressionTests"
        [ Test.test "let expression with multiple declarations"
            (\() ->
                """let
  foo = bar

  john n = n in 1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 18 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "foo"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } "john"
                                                    , arguments = [ Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Elm.Syntax.Pattern.VarPattern "n") ]
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (Elm.Syntax.Expression.FunctionOrValue [] "n")
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } } (Elm.Syntax.Expression.Integer 1)
                                }
                            )
                        )
            )
        , Test.test "Let with `in` indented more than the body and let declarations"
            (\() ->
                """let
  bar = 1
            in
  bar"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 6 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                }
                            )
                        )
            )
        , Test.test "should fail to parse if declaration is indented as much as `let`"
            (\() ->
                """  let
  bar = 1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should fail to parse if declarations are not indented the same way"
            (\() ->
                """  let
    bar = 1
      foo = 2
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "let with deindented expression in in"
            (\() ->
                """let
  bar = 1
 in
   bar"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 7 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 4 }, end = { row = 4, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                }
                            )
                        )
            )
        , Test.test "Let function with type annotation"
            (\() ->
                """let
    bar : Int
    bar = 1
  in
  bar"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                        , typeAnnotation =
                                                            Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                }
                            )
                        )
            )
        , Test.test "Let function with type annotation (separated by a few lines)"
            (\() ->
                """let
    bar : Int


    bar = 1
  in
  bar"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 5, column = 12 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                        , typeAnnotation = Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 12 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                }
                            )
                        )
            )
        , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation earlier)"
            (\() ->
                """let
    bar : Int
      bar = 1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation later)"
            (\() ->
                """let
       bar : Int
    bar = 1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should fail to parse `as` pattern not surrounded by parentheses"
            (\() ->
                """let
          bar n as m = 1
        in
        bar"""
                    |> expectInvalid
            )
        , Test.test "correctly parse variant + args pattern not surrounded by parentheses"
            (\() ->
                """let
          bar Bar m = 1
        in
        bar"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 12 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } "bar"
                                                    , arguments =
                                                        [ Elm.Syntax.Node.Node { start = { row = 2, column = 15 }, end = { row = 2, column = 18 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Bar" } [])
                                                        , Elm.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 20 } } (Elm.Syntax.Pattern.VarPattern "m")
                                                        ]
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 23 }, end = { row = 2, column = 24 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                }
                            )
                        )
            )
        , Test.test "should not parse let destructuring with a type annotation"
            (\() ->
                """let
    bar : Int
    (bar) = 1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should not parse let destructuring with `as` not surrounded by parentheses"
            (\() ->
                """let
    foo as bar = 1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should not parse let destructuring with variant + arguments not surrounded by parentheses"
            (\() ->
                """let
    Foo bar = 1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should not parse let destructuring with non-positive layout before ="
            (\() ->
                """let
    (bar)
    =     1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should not parse let destructuring with non-positive layout before expression"
            (\() ->
                """let
    (bar) =
    1
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "should not parse let type annotation without a declaration"
            (\() ->
                """let
    bar : Int
  in
  bar"""
                    |> expectInvalid
            )
        , Test.test "Using destructuring"
            (\() ->
                """let
    _ = b
    {a} = b
    (c, d) = e
    (Node _ f) = g
 in
    1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                        (Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } } Elm.Syntax.Pattern.AllPattern) (Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "b")))
                                    , Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                        (Elm.Syntax.Expression.LetDestructuring
                                            (Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                                                (Elm.Syntax.Pattern.RecordPattern [ Elm.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } "a" ])
                                            )
                                            (Elm.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "b"))
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 5 }, end = { row = 4, column = 15 } }
                                        (Elm.Syntax.Expression.LetDestructuring
                                            (Elm.Syntax.Node.Node { start = { row = 4, column = 5 }, end = { row = 4, column = 11 } }
                                                (Elm.Syntax.Pattern.TuplePattern
                                                    [ Elm.Syntax.Node.Node { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } } (Elm.Syntax.Pattern.VarPattern "c")
                                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 10 } } (Elm.Syntax.Pattern.VarPattern "d")
                                                    ]
                                                )
                                            )
                                            (Elm.Syntax.Node.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 15 } } (Elm.Syntax.Expression.FunctionOrValue [] "e"))
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 19 } }
                                        (Elm.Syntax.Expression.LetDestructuring
                                            (Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 15 } }
                                                (Elm.Syntax.Pattern.ParenthesizedPattern
                                                    (Elm.Syntax.Node.Node { start = { row = 5, column = 6 }, end = { row = 5, column = 14 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Node" } [ Elm.Syntax.Node.Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } Elm.Syntax.Pattern.AllPattern, Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (Elm.Syntax.Pattern.VarPattern "f") ]))
                                                )
                                            )
                                            (Elm.Syntax.Node.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (Elm.Syntax.Expression.FunctionOrValue [] "g"))
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } } (Elm.Syntax.Expression.Integer 1)
                                }
                            )
                        )
            )
        , Test.test "On one line"
            (\() ->
                "let indent = String.length s in indent"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } } "indent"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 29 } }
                                                            (Elm.Syntax.Expression.Application
                                                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 27 } } (Elm.Syntax.Expression.FunctionOrValue [ "String" ] "length")
                                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Elm.Syntax.Expression.FunctionOrValue [] "s")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 33 }, end = { row = 1, column = 39 } } (Elm.Syntax.Expression.FunctionOrValue [] "indent")
                                }
                            )
                        )
            )
        , Test.test "let with list after in without space"
            (\() ->
                """let
        a = 1
    in[]"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (Elm.Syntax.Expression.ListExpr [])
                                }
                            )
                        )
            )
        , Test.test "let with record after in without space"
            (\() ->
                """let
        a = 1
    in{}"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (Elm.Syntax.Expression.RecordExpr [])
                                }
                            )
                        )
            )
        , Test.test "let with lambda after in without space"
            (\() ->
                """let
        a = 1
    in\\_ -> 1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 14 } }
                            (Elm.Syntax.Expression.LetExpression
                                { declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                        (Elm.Syntax.Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression =
                                    Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                        (Elm.Syntax.Expression.LambdaExpression { args = [ Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } Elm.Syntax.Pattern.AllPattern ], expression = Elm.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Elm.Syntax.Expression.Integer 1) })
                                }
                            )
                        )
            )
        , Test.test "let is not confused by a variable name starting with let"
            (\() ->
                "letterbox"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "letterbox"))
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst Elm.Parser.Expression.expressionFollowedByOptimisticLayout


expectInvalid : String -> Expect.Expectation
expectInvalid =
    Elm.Parser.ParserWithCommentsTestUtil.expectInvalid Elm.Parser.Expression.expressionFollowedByOptimisticLayout
