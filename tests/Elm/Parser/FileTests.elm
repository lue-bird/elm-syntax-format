module Elm.Parser.FileTests exposing (all)

import Elm.Parser
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Parser.Samples
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.TypeAnnotation
import ElmSyntaxParserLenient
import Expect
import ParserFast
import Rope
import Test


all : Test.Test
all =
    Test.concat
        [ Test.describe "FileTests"
            (List.map
                (\( n, s ) ->
                    Test.test ("sample " ++ String.fromInt n)
                        (\() ->
                            case ParserFast.run ElmSyntaxParserLenient.module_ s of
                                Nothing ->
                                    Expect.fail "failed to parse"

                                Just _ ->
                                    Expect.pass
                        )
                )
                Elm.Parser.Samples.allSamples
            )

        -- , describe "Error messages" <|
        --     [ test "failure on module name" <|
        --         \() ->
        --             Parser.parse "module foo exposing (..)\nx = 1"
        --                 |> Result.toMaybe
        --                 |> Expect.equal Nothing
        --     , test "failure on declaration" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..)\n\ntype x = \n  1"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,0)" ])
        --     , test "failure on declaration expression" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..) \nx = \n  x + _"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,6)" ])
        --     ]
        , Test.describe "module header"
            [ Test.test "formatted moduleDefinition"
                (\() ->
                    "module Foo exposing (Bar)"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.NormalModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , exposingList =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                        (Elm.Syntax.Exposing.Explicit
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Bar")
                                            ]
                                        )
                                }
                            )
                )
            , Test.test "port moduleDefinition"
                (\() ->
                    "port module Foo exposing (Bar)"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.PortModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                                , exposingList =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 31 } }
                                        (Elm.Syntax.Exposing.Explicit
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 30 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Bar") ]
                                        )
                                }
                            )
                )
            , Test.test "port moduleDefinition with spacing"
                (\() ->
                    "port module Foo exposing ( Bar )"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.PortModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                                , exposingList =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                        (Elm.Syntax.Exposing.Explicit
                                            [ Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Bar") ]
                                        )
                                }
                            )
                )
            , Test.test "effect moduleDefinition"
                (\() ->
                    "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.EffectModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Foo" ]
                                , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 66 }, end = { row = 1, column = 80 } } (Elm.Syntax.Exposing.Explicit [ Elm.Syntax.Node.Node { start = { row = 1, column = 76 }, end = { row = 1, column = 79 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Bar") ])
                                , command = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 41 } } "MyCmd")
                                , subscription = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 58 }, end = { row = 1, column = 63 } } "MySub")
                                }
                            )
                )
            , Test.test "unformatted"
                (\() ->
                    "module \n Foo \n exposing  (..)"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.NormalModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } [ "Foo" ]
                                , exposingList =
                                    Elm.Syntax.Node.Node { start = { row = 3, column = 2 }, end = { row = 3, column = 16 } }
                                        (Elm.Syntax.Exposing.All { start = { row = 3, column = 13 }, end = { row = 3, column = 15 } })
                                }
                            )
                )
            , Test.test "unformatted wrong"
                (\() ->
                    Elm.Parser.ParserWithCommentsTestUtil.parse "module \nFoo \n exposing  (..)" ElmSyntaxParserLenient.moduleHeader
                        |> Expect.equal Nothing
                )
            , Test.test "exposing all"
                (\() ->
                    "module Foo exposing (..)"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.NormalModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , exposingList =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                }
                            )
                )
            , Test.test "module name with _"
                (\() ->
                    "module I_en_gb exposing (..)"
                        |> moduleHeaderExpectAst
                            (Elm.Syntax.Module.NormalModule
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } [ "I_en_gb" ]
                                , exposingList =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 29 } }
                                        (Elm.Syntax.Exposing.All { start = { row = 1, column = 26 }, end = { row = 1, column = 28 } })
                                }
                            )
                )
            , Test.test "Regression test for Incorrect range in if expression"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)

a =
    if cond then
        1
    else
        2



{-| doc
-}
b = 3
"""
                        ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName =
                                                Elm.Syntax.Node.Node
                                                    { start = { row = 1, column = 8 }
                                                    , end = { row = 1, column = 18 }
                                                    }
                                                    [ "TestModule" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node
                                                    { start = { row = 1, column = 19 }
                                                    , end =
                                                        { row = 1
                                                        , column = 32
                                                        }
                                                    }
                                                    (Elm.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 4, column = 5 }, end = { row = 7, column = 10 } }
                                                            (Elm.Syntax.Expression.IfBlock
                                                                (Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } }
                                                                    (Elm.Syntax.Expression.FunctionOrValue [] "cond")
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } (Elm.Syntax.Expression.Integer 1))
                                                                (Elm.Syntax.Node.Node
                                                                    { start =
                                                                        { row = 7
                                                                        , column = 9
                                                                        }
                                                                    , end = { row = 7, column = 10 }
                                                                    }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 13, column = 6 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Just (Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 12, column = 3 } } "{-| doc\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node
                                                    { start = { row = 13, column = 1 }
                                                    , end =
                                                        { row = 13
                                                        , column = 6
                                                        }
                                                    }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 13, column = 5 }, end = { row = 13, column = 6 } } (Elm.Syntax.Expression.Integer 3)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "Simple module range test"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)

a =
    2



{-| doc
-}
b = 3
"""
                        ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    (Elm.Syntax.Exposing.All
                                                        { start = { row = 1, column = 29 }
                                                        , end = { row = 1, column = 31 }
                                                        }
                                                    )
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node
                                        { start = { row = 3, column = 1 }
                                        , end = { row = 4, column = 6 }
                                        }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 6 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node
                                                            { start = { row = 4, column = 5 }
                                                            , end = { row = 4, column = 6 }
                                                            }
                                                            (Elm.Syntax.Expression.Integer 2)
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node
                                        { start = { row = 8, column = 1 }
                                        , end = { row = 10, column = 6 }
                                        }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Just (Elm.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 9, column = 3 } } "{-| doc\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 6 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node
                                                            { start = { row = 10, column = 5 }
                                                            , end = { row = 10, column = 6 }
                                                            }
                                                            (Elm.Syntax.Expression.Integer 3)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "File with multiple imports"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)
import A
import B

a = 1
"""
                        ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    (Elm.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 9 } }
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } } [ "A" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    , Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } [ "B" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "File with multiple declarations"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)
type A = B | C
a = 1
type alias B = A
b : Int
b = 2
"""
                        ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 15 } }
                                        (Elm.Syntax.Declaration.CustomTypeDeclaration
                                            { documentation = Nothing
                                            , name = Elm.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } "A"
                                            , generics = []
                                            , constructors =
                                                [ Elm.Syntax.Node.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } "B"
                                                    , arguments = []
                                                    }
                                                , Elm.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } "C"
                                                    , arguments = []
                                                    }
                                                ]
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                        (Elm.Syntax.Declaration.AliasDeclaration
                                            { documentation = Nothing
                                            , name = Elm.Syntax.Node.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } "B"
                                            , generics = []
                                            , typeAnnotation =
                                                Elm.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } ( [], "A" )) [])
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 6 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                                                        { name = Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "b"
                                                        , typeAnnotation = Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 6 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Elm.Syntax.Expression.Integer 2)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "should fail to parse two signatures in a row"
                (\() ->
                    """module TestModule exposing (..)
a : Int
b : Int
b = 2
"""
                        |> moduleExpectInvalid
                )
            , Test.test "should fail to parse signature for a different function"
                (\() ->
                    """module TestModule exposing (..)
a : Int
b = 2
"""
                        |> moduleExpectInvalid
                )
            , Test.test "trailing comments at the end of declarations"
                (\() ->
                    parseSource """module A exposing (fun1, fun2)

fun1 n =
  fun2 n
  + fun2 n  -- a

fun2 n =
  fun1 n    -- b
"""
                        ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (Elm.Syntax.Module.NormalModule { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (Elm.Syntax.Exposing.Explicit [ Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (Elm.Syntax.Exposing.FunctionExpose "fun1"), Elm.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (Elm.Syntax.Exposing.FunctionExpose "fun2") ]) })
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ Elm.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (Elm.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 5, column = 11 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "+"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                                    (Elm.Syntax.Expression.Application
                                                                        [ Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                        , Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "n")
                                                                        ]
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 11 } }
                                                                    (Elm.Syntax.Expression.Application
                                                                        [ Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                        , Elm.Syntax.Node.Node { start = { row = 5, column = 10 }, end = { row = 5, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "n")
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ Elm.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (Elm.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (Elm.Syntax.Expression.Application
                                                                [ Elm.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "fun1")
                                                                , Elm.Syntax.Node.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 17 } } "-- a", Elm.Syntax.Node.Node { start = { row = 8, column = 13 }, end = { row = 8, column = 17 } } "-- b" ]
                                }
                            )
                )
            ]
        , Test.describe "import"
            [ Test.test "import with explicits"
                (\() ->
                    "import Foo exposing (Model, Msg(..))"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.import_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.import_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.import_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.import_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.import_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.import_
                )
            ]
        , Test.test "Comments ordering"
            (\() ->
                let
                    input : String
                    input =
                        """
module Foo exposing (..)

{-| Module documentation
-}

import A

-- 1
{- 2 -}
-- 3

{-| Function declaration
-}
f =
    -- 4
    identity

-- 5
{- 6 -}
"""
                in
                Elm.Parser.parseToFile input
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| Module documentation\n-}"
                            , Elm.Syntax.Node.Node { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } } "-- 1"
                            , Elm.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } } "{- 2 -}"
                            , Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } } "-- 3"
                            , Elm.Syntax.Node.Node { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } } "-- 4"
                            , Elm.Syntax.Node.Node { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } } "-- 5"
                            , Elm.Syntax.Node.Node { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } } "{- 6 -}"
                            ]
                        )
            )
        , Test.test "documentation comment"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parseWithState "{-|foo\nbar-}" (ElmSyntaxParserLenient.documentationComment |> ParserFast.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}" ])
            )
        , Test.test "documentation comment can handle nested comments"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parseWithState "{-| {- hello -} -}" (ElmSyntaxParserLenient.documentationComment |> ParserFast.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| {- hello -} -}" ])
            )
        , Test.test "declarations with comments"
            (\() ->
                """module Foo exposing (b, fn)

fn a =
    case a of
        X ->
            1
                -- 1
                + 2

        Y ->
            1

-- 2

b =
    1

"""
                    |> Elm.Parser.parseToFile
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Elm.Syntax.Node.Node { start = { row = 7, column = 17 }, end = { row = 7, column = 21 } } "-- 1"
                            , Elm.Syntax.Node.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 5 } } "-- 2"
                            ]
                        )
            )
        , Test.test "function declaration with a case and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

caseWhitespace f = case f   of
  True     -> 
    1   
  False   
     -> 
     
     
         2    

   --some comment

    
    """
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } } "caseWhitespace"
                                                , arguments = [ Elm.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } (Elm.Syntax.Pattern.VarPattern "f") ]
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        (Elm.Syntax.Expression.CaseExpression
                                                            { expression = Elm.Syntax.Node.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } } (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                                            , cases =
                                                                [ ( Elm.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                                                  , Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Elm.Syntax.Expression.Integer 1)
                                                                  )
                                                                , ( Elm.Syntax.Node.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                                                  , Elm.Syntax.Node.Node { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } } (Elm.Syntax.Expression.Integer 2)
                                                                  )
                                                                ]
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } } "--some comment" ]
                            }
                        )
            )
        , Test.test "function declaration with lambda and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

lambdaWhitespace =   \\ a b ->    a    

       + 

    b 


--some comment

    """
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } "lambdaWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        (Elm.Syntax.Expression.LambdaExpression
                                                            { args =
                                                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } } (Elm.Syntax.Pattern.VarPattern "a")
                                                                , Elm.Syntax.Node.Node { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } } (Elm.Syntax.Pattern.VarPattern "b")
                                                                ]
                                                            , expression =
                                                                Elm.Syntax.Node.Node { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } } (Elm.Syntax.Expression.FunctionOrValue [] "a"))
                                                                        (Elm.Syntax.Node.Node { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
            )
        , Test.test "function declaration with let and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

letWhitespace = let
                  b =   1

 in
 b


--some comment

    """
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } } "letWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        (Elm.Syntax.Expression.LetExpression
                                                            { declarations =
                                                                [ Elm.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                    (Elm.Syntax.Expression.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            Elm.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                { name = Elm.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } } "b"
                                                                                , arguments = []
                                                                                , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } } (Elm.Syntax.Expression.Integer 1)
                                                                                }
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = Elm.Syntax.Node.Node { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } } (Elm.Syntax.Expression.FunctionOrValue [] "b")
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
            )
        , Test.test "type declaration with documentation after imports"
            (\() ->
                """
module Foo exposing (..)

import Dict

{-| Config goes here
-}
type Configuration
    = Configuration
"""
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } } [ "Dict" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                    (Elm.Syntax.Declaration.CustomTypeDeclaration
                                        { documentation = Just (Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| Config goes here\n-}")
                                        , name = Elm.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ Elm.Syntax.Node.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = []
                            }
                        )
            )
        , Test.test "module documentation formatted like a type documentation"
            (\() ->
                """
module Foo exposing (..)

{-| actually module doc
-}
type Configuration
    = Configuration
"""
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                    (Elm.Syntax.Declaration.CustomTypeDeclaration
                                        { documentation = Nothing
                                        , name = Elm.Syntax.Node.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| actually module doc\n-}" ]
                            }
                        )
            )
        , Test.test "documentation on a port declaration is treated as a normal comment"
            (\() ->
                """
port module Foo exposing (..)

import String

{-| foo
-}
port sendResponse : String -> Cmd msg
"""
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 30 } }
                                    (Elm.Syntax.Module.PortModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 16 } } [ "Foo" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 17 }, end = { row = 2, column = 30 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 27 }, end = { row = 2, column = 29 } })
                                        }
                                    )
                            , imports =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } } [ "String" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 38 } }
                                    (Elm.Syntax.Declaration.PortDeclaration
                                        { name = Elm.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 18 } } "sendResponse"
                                        , typeAnnotation =
                                            Elm.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 38 } }
                                                (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Elm.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } } ( [], "String" )) [])
                                                    )
                                                    (Elm.Syntax.Node.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 38 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 34 } } ( [], "Cmd" ))
                                                            [ Elm.Syntax.Node.Node { start = { row = 8, column = 35 }, end = { row = 8, column = 38 } } (Elm.Syntax.TypeAnnotation.GenericType "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| foo\n-}" ]
                            }
                        )
            )
        , Test.test "A file with a large number of comments should not create a stack overflow"
            (\() ->
                let
                    comments : String
                    comments =
                        String.repeat 3000 "-- a\n"
                in
                ("""module Foo exposing (..)
a = 1
""" ++ comments)
                    |> Elm.Parser.parseToFile
                    |> Result.map (\ast -> List.length ast.comments)
                    |> Expect.equal (Ok 3000)
            )
        ]


moduleHeaderExpectAst : Elm.Syntax.Module.Module -> String -> Expect.Expectation
moduleHeaderExpectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst (ParserFast.map (\mod -> { comments = mod.comments, syntax = Elm.Syntax.Node.value mod.syntax }) ElmSyntaxParserLenient.moduleHeader)


parseSource : String -> ParserFast.Parser a -> Maybe a
parseSource source parser =
    ParserFast.run parser source


moduleExpectInvalid : String -> Expect.Expectation
moduleExpectInvalid source =
    case ParserFast.run ElmSyntaxParserLenient.module_ source of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
