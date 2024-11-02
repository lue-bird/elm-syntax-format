module Elm.Parser.DeclarationsTests exposing (all)

import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.TypeAnnotation
import ElmSyntaxParserLenient
import Expect
import Test


all : Test.Test
all =
    Test.describe "DeclarationTests"
        [ Test.test "function declaration"
            (\() ->
                "foo = bar"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                }
                            )
                        )
            )
        , Test.test "function declaration with documentation"
            (\() ->
                """{-| Foo does bar -}
foo = bar"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } } "{-| Foo does bar -}")
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 10 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                }
                            )
                        )
            )
        , Test.test "function declaration with empty record"
            (\() ->
                "foo = {}"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (Elm.Syntax.Expression.RecordExpr [])
                                        }
                                }
                            )
                        )
            )
        , Test.test "function with case in let"
            (\() ->
                """inc x =
  let
    y =
      case x of
        True -> z
    a = b
  in a"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                        , arguments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.VarPattern "x") ]
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                (Elm.Syntax.Expression.LetExpression
                                                    { declarations =
                                                        [ Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                            (Elm.Syntax.Expression.LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                        { name = Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } "y"
                                                                        , arguments = []
                                                                        , expression =
                                                                            Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 5, column = 18 } }
                                                                                (Elm.Syntax.Expression.CaseExpression
                                                                                    { expression = Elm.Syntax.Node.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                                                                    , cases =
                                                                                        [ ( Elm.Syntax.Node.Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                                                                          , Elm.Syntax.Node.Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (Elm.Syntax.Expression.FunctionOrValue [] "z")
                                                                                          )
                                                                                        ]
                                                                                    }
                                                                                )
                                                                        }
                                                                }
                                                            )
                                                        , Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                            (Elm.Syntax.Expression.LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                        { name = Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } "a"
                                                                        , arguments = []
                                                                        , expression = Elm.Syntax.Node.Node { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "b")
                                                                        }
                                                                }
                                                            )
                                                        ]
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                                    }
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "function declaration with args"
            (\() ->
                "inc x = x + 1"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                        , arguments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.VarPattern "x") ]
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (Elm.Syntax.Expression.OperatorApplication "+"
                                                    Elm.Syntax.Infix.Left
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "x"))
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (Elm.Syntax.Expression.Integer 1))
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "function declaration with let"
            (\() ->
                """foo =
 let
  b = 1
 in
  b"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (Elm.Syntax.Expression.LetExpression
                                                    { declarations =
                                                        [ Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                            (Elm.Syntax.Expression.LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                        { name = Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } } "b"
                                                                        , arguments = []
                                                                        , expression = Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Elm.Syntax.Expression.Integer 1)
                                                                        }
                                                                }
                                                            )
                                                        ]
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "b")
                                                    }
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "documentation comment inside a let is invalid"
            (\() ->
                expectInvalid """foo =
 let
  {-| b is one -}
  b = 1
 in
  b"""
            )
        , Test.test "let destructuring with no spaces around '='"
            (\() ->
                """foo =
 let
  (b, c)=(1, 2)
 in
  b"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (Elm.Syntax.Expression.LetExpression
                                                    { declarations =
                                                        [ Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                            (Elm.Syntax.Expression.LetDestructuring
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                    (Elm.Syntax.Pattern.TuplePattern
                                                                        [ Elm.Syntax.Node.Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (Elm.Syntax.Pattern.VarPattern "b")
                                                                        , Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Elm.Syntax.Pattern.VarPattern "c")
                                                                        ]
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                    (Elm.Syntax.Expression.TupledExpression
                                                                        [ Elm.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Elm.Syntax.Expression.Integer 1)
                                                                        , Elm.Syntax.Node.Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (Elm.Syntax.Expression.Integer 2)
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        ]
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "b")
                                                    }
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "declaration with record"
            (\() ->
                """main =
  beginnerProgram { model = 0, view = view, update = update }"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                (Elm.Syntax.Expression.Application
                                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (Elm.Syntax.Expression.FunctionOrValue [] "beginnerProgram")
                                                    , Elm.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                        (Elm.Syntax.Expression.RecordExpr
                                                            [ Elm.Syntax.Node.Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                ( Elm.Syntax.Node.Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                , Elm.Syntax.Node.Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (Elm.Syntax.Expression.Integer 0)
                                                                )
                                                            , Elm.Syntax.Node.Node { start = { row = 2, column = 32 }, end = { row = 2, column = 43 } }
                                                                ( Elm.Syntax.Node.Node { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } } "view"
                                                                , Elm.Syntax.Node.Node { start = { row = 2, column = 39 }, end = { row = 2, column = 43 } } (Elm.Syntax.Expression.FunctionOrValue [] "view")
                                                                )
                                                            , Elm.Syntax.Node.Node { start = { row = 2, column = 45 }, end = { row = 2, column = 61 } }
                                                                ( Elm.Syntax.Node.Node { start = { row = 2, column = 45 }, end = { row = 2, column = 51 } } "update"
                                                                , Elm.Syntax.Node.Node { start = { row = 2, column = 54 }, end = { row = 2, column = 60 } } (Elm.Syntax.Expression.FunctionOrValue [] "update")
                                                                )
                                                            ]
                                                        )
                                                    ]
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "update function"
            (\() ->
                """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Elm.Syntax.Pattern.VarPattern "msg")
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (Elm.Syntax.Pattern.VarPattern "model")
                                            ]
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (Elm.Syntax.Expression.CaseExpression
                                                    { expression = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "msg")
                                                    , cases =
                                                        [ ( Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Increment" } [])
                                                          , Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                (Elm.Syntax.Expression.OperatorApplication "+"
                                                                    Elm.Syntax.Infix.Left
                                                                    (Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "model"))
                                                                    (Elm.Syntax.Node.Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Elm.Syntax.Expression.Integer 1))
                                                                )
                                                          )
                                                        , ( Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (Elm.Syntax.Expression.OperatorApplication "-"
                                                                    Elm.Syntax.Infix.Left
                                                                    (Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "model"))
                                                                    (Elm.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Elm.Syntax.Expression.Integer 1))
                                                                )
                                                          )
                                                        ]
                                                    }
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "port declaration for command"
            (\() ->
                "port parseResponse : ( String, String ) -> Cmd msg"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 51 } }
                            (Elm.Syntax.Declaration.PortDeclaration
                                { name = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 19 } } "parseResponse"
                                , typeAnnotation =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 51 } }
                                        (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 40 } }
                                                (Elm.Syntax.TypeAnnotation.Tupled
                                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed
                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } } ( [], "String" ))
                                                            []
                                                        )
                                                    ]
                                                )
                                            )
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 51 } }
                                                (Elm.Syntax.TypeAnnotation.Typed
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } ( [], "Cmd" ))
                                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 48 }, end = { row = 1, column = 51 } } (Elm.Syntax.TypeAnnotation.GenericType "msg") ]
                                                )
                                            )
                                        )
                                }
                            )
                        )
            )
        , Test.test "port declaration for subscription"
            (\() ->
                "port scroll : (Move -> msg) -> Sub msg"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (Elm.Syntax.Declaration.PortDeclaration
                                { name = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } "scroll"
                                , typeAnnotation =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 39 } }
                                        (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 28 } }
                                                (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } ( [], "Move" )) [])
                                                    )
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (Elm.Syntax.TypeAnnotation.GenericType "msg"))
                                                )
                                            )
                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 39 } }
                                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 35 } } ( [], "Sub" ))
                                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 39 } } (Elm.Syntax.TypeAnnotation.GenericType "msg")
                                                    ]
                                                )
                                            )
                                        )
                                }
                            )
                        )
            )
        , Test.test "should fail to parse destructuring declaration at the top-level"
            (\() ->
                "_ = b"
                    |> expectInvalid
            )
        , Test.test "declaration"
            (\() ->
                """main =
  text "Hello, World!\""""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Elm.Syntax.Expression.Application
                                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "text")
                                                    , Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Elm.Syntax.Expression.Literal "Hello, World!")
                                                    ]
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "function"
            (\() ->
                """main =
  text "Hello, World!\""""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        , arguments = []
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Elm.Syntax.Expression.Application
                                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "text")
                                                    , Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Elm.Syntax.Expression.Literal "Hello, World!")
                                                    ]
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "function starting with multi line comment"
            (\() ->
                """main =
  {- y -} x"""
                    |> expectAstWithComments
                        { ast =
                            Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                (Elm.Syntax.Declaration.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                            { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression = Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")
                                            }
                                    }
                                )
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } "{- y -}" ]
                        }
            )
        , Test.test "function with a lot of symbols"
            (\() ->
                "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                                        , arguments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (Elm.Syntax.Pattern.VarPattern "update"), Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (Elm.Syntax.Pattern.VarPattern "sendPort") ]
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                (Elm.Syntax.Expression.OperatorApplication "<|"
                                                    Elm.Syntax.Infix.Right
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (Elm.Syntax.Expression.FunctionOrValue [] "curry"))
                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 40 }, end = { row = 1, column = 83 } }
                                                        (Elm.Syntax.Expression.OperatorApplication ">>"
                                                            Elm.Syntax.Infix.Right
                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                                (Elm.Syntax.Expression.ParenthesizedExpression
                                                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                        (Elm.Syntax.Expression.Application
                                                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } } (Elm.Syntax.Expression.FunctionOrValue [] "uncurry")
                                                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } } (Elm.Syntax.Expression.FunctionOrValue [] "update")
                                                                            ]
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            (Elm.Syntax.Node.Node { start = { row = 1, column = 60 }, end = { row = 1, column = 83 } }
                                                                (Elm.Syntax.Expression.Application
                                                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } } (Elm.Syntax.Expression.FunctionOrValue [] "batchStateCmds")
                                                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } } (Elm.Syntax.Expression.FunctionOrValue [] "sendPort")
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "Some function"
            (\() ->
                """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Elm.Syntax.Pattern.VarPattern "msg")
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (Elm.Syntax.Pattern.VarPattern "model")
                                            ]
                                        , expression =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (Elm.Syntax.Expression.CaseExpression
                                                    { expression = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "msg")
                                                    , cases =
                                                        [ ( Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Increment" } [])
                                                          , Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                (Elm.Syntax.Expression.OperatorApplication "+"
                                                                    Elm.Syntax.Infix.Left
                                                                    (Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "model"))
                                                                    (Elm.Syntax.Node.Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Elm.Syntax.Expression.Integer 1))
                                                                )
                                                          )
                                                        , ( Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (Elm.Syntax.Expression.OperatorApplication "-"
                                                                    Elm.Syntax.Infix.Left
                                                                    (Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (Elm.Syntax.Expression.FunctionOrValue [] "model"))
                                                                    (Elm.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Elm.Syntax.Expression.Integer 1))
                                                                )
                                                          )
                                                        ]
                                                    }
                                                )
                                        }
                                }
                            )
                        )
            )
        , Test.test "some other function"
            (\() ->
                """update : Model
update msg model =
    msg"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 8 } }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature =
                                    Just
                                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                            { name = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , typeAnnotation = Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } ( [], "Model" )) [])
                                            }
                                        )
                                , declaration =
                                    Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } } "update"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (Elm.Syntax.Pattern.VarPattern "msg")
                                            , Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (Elm.Syntax.Pattern.VarPattern "model")
                                            ]
                                        , expression = Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "msg")
                                        }
                                }
                            )
                        )
            )
        , Test.test "type alias"
            (\() ->
                "type alias Foo = {color: String }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 34 } }
                            (Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 34 } }
                                        (Elm.Syntax.TypeAnnotation.Record
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } "color"
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
            )
        , Test.test "type alias with documentation"
            (\() ->
                """{-| Foo is colorful -}
type alias Foo = {color: String }"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 34 } }
                            (Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } } "{-| Foo is colorful -}")
                                , name = Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } } "Foo"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.Node { start = { row = 2, column = 18 }, end = { row = 2, column = 34 } }
                                        (Elm.Syntax.TypeAnnotation.Record
                                            [ Elm.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 32 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } } "color"
                                                , Elm.Syntax.Node.Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
            )
        , Test.test "type alias without spacings around '='"
            (\() ->
                "type alias Foo={color: String }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
                                , typeAnnotation =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 32 } }
                                        (Elm.Syntax.TypeAnnotation.Record
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 30 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } "color"
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
            )
        , Test.test "type alias with GenericType "
            (\() ->
                "type alias Foo a = {some : a }"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                            (Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = [ Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } "a" ]
                                , typeAnnotation =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 31 } }
                                        (Elm.Syntax.TypeAnnotation.Record
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 29 } }
                                                ( Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } "some"
                                                , Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Elm.Syntax.TypeAnnotation.GenericType "a")
                                                )
                                            ]
                                        )
                                }
                            )
                        )
            )
        , Test.test "type"
            (\() ->
                "type Color = Blue String | Red | Green"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                            ]
                                        }
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                        , arguments = []
                                        }
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
            )
        , Test.test "type with documentation"
            (\() ->
                """{-| Classic RGB -}
type Color = Blue String | Red | Green"""
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 39 } }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| Classic RGB -}")
                                , name = Elm.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 11 } } "Color"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.Node
                                        { start = { row = 2, column = 14 }
                                        , end = { row = 2, column = 25 }
                                        }
                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 18 } } "Blue"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } } ( [], "String" )) [])
                                            ]
                                        }
                                    , Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } } "Red"
                                        , arguments = []
                                        }
                                    , Elm.Syntax.Node.Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } } "Green"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
            )
        , Test.test "type with multiple args"
            (\() ->
                "type D = C a B"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } (Elm.Syntax.TypeAnnotation.GenericType "a")
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } ( [], "B" )) [])
                                            ]
                                        }
                                    ]
                                }
                            )
                        )
            )
        , Test.test "type with multiple args and correct distribution of args"
            (\() ->
                "type D = C B a"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                , generics = []
                                , constructors =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                        , arguments =
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }
                                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } ( [], "B" )) [])
                                            , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Elm.Syntax.TypeAnnotation.GenericType "a")
                                            ]
                                        }
                                    ]
                                }
                            )
                        )
            )
        , Test.test "type args should not continue on next line"
            (\() ->
                "type D = C B\na"
                    |> expectInvalid
            )
        , Test.test "type with GenericType"
            (\() ->
                "type Maybe a = Just a | Nothing"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Maybe"
                                , generics = [ Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } "a" ]
                                , constructors =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } "Just"
                                        , arguments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (Elm.Syntax.TypeAnnotation.GenericType "a") ]
                                        }
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                        { name = Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } } "Nothing"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
            )
        , Test.test "type with value on next line "
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parse "type Maybe a = Just a |\nNothing" ElmSyntaxParserLenient.declaration
                    |> Expect.equal Nothing
            )
        , Test.test "fail if declarations not on module-level"
            (\() ->
                """a = f
    3
    b = 4"""
                    |> expectInvalid
            )
        , Test.test "fail if function declaration argument is `as` without parenthesis"
            (\() ->
                """a foo as bar = f3"""
                    |> expectInvalid
            )
        , Test.test "regression test for disallowing ( +)"
            (\() ->
                "a = ( +)"
                    |> expectInvalid
            )
        , Test.test "regression test for disallowing (+ )"
            (\() ->
                "a = (+ )"
                    |> expectInvalid
            )
        , Test.test "right infix"
            (\() ->
                "infix right 7 (</>) = slash"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.declaration
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            (Elm.Syntax.Declaration.InfixDeclaration
                                { direction = Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } } Elm.Syntax.Infix.Right
                                , precedence = Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 7
                                , operator = Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "</>"
                                , function = Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 28 } } "slash"
                                }
                            )
                        )
            )
        , Test.test "left infix"
            (\() ->
                "infix left  8 (<?>) = questionMark"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.declaration
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 35 } }
                            (Elm.Syntax.Declaration.InfixDeclaration
                                { direction = Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } Elm.Syntax.Infix.Left
                                , precedence = Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 8
                                , operator = Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "<?>"
                                , function = Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 35 } } "questionMark"
                                }
                            )
                        )
            )
        , Test.test "non infix"
            (\() ->
                "infix non   4 (==) = eq"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.declaration
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            (Elm.Syntax.Declaration.InfixDeclaration
                                { direction = Elm.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } Elm.Syntax.Infix.Non
                                , precedence = Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 4
                                , operator = Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 19 } } "=="
                                , function = Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } } "eq"
                                }
                            )
                        )
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAstWithIndent1 ElmSyntaxParserLenient.declaration


expectAstWithComments : { ast : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration, comments : List (Elm.Syntax.Node.Node String) } -> String -> Expect.Expectation
expectAstWithComments =
    Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments ElmSyntaxParserLenient.declaration


expectInvalid : String -> Expect.Expectation
expectInvalid =
    Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
