module Elm.Parser.FileTests exposing (all)

import Elm.Parser
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Parser.Samples
import Elm.Parser.TestUtil
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
        , Test.test "moduleName"
            (\() ->
                Elm.Parser.TestUtil.parseToResult "Foo" ElmSyntaxParserLenient.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo" ])
            )
        , Test.test "moduleNameDir"
            (\() ->
                Elm.Parser.TestUtil.parseToResult "Foo.Bar" ElmSyntaxParserLenient.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo", "Bar" ])
            )
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
            , Test.describe "uppercase name"
                [ Test.test "lower and upper simple latin"
                    (\() ->
                        Elm.Parser.TestUtil.parse "MyCmd" ElmSyntaxParserLenient.typeName
                            |> Expect.equal (Just "MyCmd")
                    )
                , Test.test "typeName not empty"
                    (\() ->
                        Elm.Parser.TestUtil.parse "" ElmSyntaxParserLenient.typeName
                            |> Expect.equal Nothing
                    )
                , Test.test "typeName with number"
                    (\() ->
                        Elm.Parser.TestUtil.parse "T1" ElmSyntaxParserLenient.typeName
                            |> Expect.equal (Just "T1")
                    )
                , Test.test "ρ function"
                    (\() ->
                        Elm.Parser.TestUtil.parse "ρ" ElmSyntaxParserLenient.functionName
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ε2 function"
                    (\() ->
                        Elm.Parser.TestUtil.parse "ε2" ElmSyntaxParserLenient.functionName
                            |> Expect.notEqual Nothing
                    )
                , Test.test "εε function"
                    (\() ->
                        Elm.Parser.TestUtil.parse "εε" ElmSyntaxParserLenient.functionName
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ρ uppercase function"
                    (\() ->
                        Elm.Parser.TestUtil.parse (String.toUpper "ρ") ElmSyntaxParserLenient.functionName
                            |> Expect.equal Nothing
                    )
                , Test.test "ε uppercase function"
                    (\() ->
                        Elm.Parser.TestUtil.parse (String.toUpper "ε") ElmSyntaxParserLenient.functionName
                            |> Expect.equal Nothing
                    )
                , Test.test "ρ type name"
                    (\() ->
                        Elm.Parser.TestUtil.parse "ρ" ElmSyntaxParserLenient.typeName
                            |> Expect.equal Nothing
                    )
                , Test.test "ε2 type name"
                    (\() ->
                        Elm.Parser.TestUtil.parse "ε2" ElmSyntaxParserLenient.typeName
                            |> Expect.equal Nothing
                    )
                , Test.test "εε type name"
                    (\() ->
                        Elm.Parser.TestUtil.parse "εε" ElmSyntaxParserLenient.typeName
                            |> Expect.equal Nothing
                    )
                , Test.test "ρ uppercase type name"
                    (\() ->
                        Elm.Parser.TestUtil.parse (String.toUpper "ρ") ElmSyntaxParserLenient.typeName
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ε uppercase type name"
                    (\() ->
                        Elm.Parser.TestUtil.parse (String.toUpper "ε") ElmSyntaxParserLenient.typeName
                            |> Expect.notEqual Nothing
                    )
                ]
            , Test.describe "lowercase name"
                [ Test.test "simple latin"
                    (\() ->
                        Elm.Parser.TestUtil.parse "foo" ElmSyntaxParserLenient.functionName
                            |> Expect.equal (Just "foo")
                    )
                , Test.test "functionName may not be a keyword"
                    (\() ->
                        Elm.Parser.TestUtil.parse "type" ElmSyntaxParserLenient.functionName
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName may be a keyword suffixed with an underscore"
                    (\() ->
                        Elm.Parser.TestUtil.parse "type_" ElmSyntaxParserLenient.functionName
                            |> Expect.equal (Just "type_")
                    )
                , Test.test "functionName not empty"
                    (\() ->
                        Elm.Parser.TestUtil.parse "" ElmSyntaxParserLenient.functionName
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName with number"
                    (\() ->
                        Elm.Parser.TestUtil.parse "n1" ElmSyntaxParserLenient.functionName
                            |> Expect.equal (Just "n1")
                    )
                , Test.test "alias can be a functionName (it is not reserved)"
                    (\() ->
                        Elm.Parser.TestUtil.parse "alias" ElmSyntaxParserLenient.functionName
                            |> Expect.equal (Just "alias")
                    )
                , Test.test "infix can be a functionName (it is not reserved)"
                    (\() ->
                        Elm.Parser.TestUtil.parse "infix" ElmSyntaxParserLenient.functionName
                            |> Expect.equal (Just "infix")
                    )
                , Test.test "functionName is not matched with 'if'"
                    (\() ->
                        Elm.Parser.TestUtil.parse "if" ElmSyntaxParserLenient.functionName
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName with _"
                    (\() ->
                        Elm.Parser.TestUtil.parse "foo_" ElmSyntaxParserLenient.functionName
                            |> Expect.equal (Just "foo_")
                    )
                ]
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
        , Test.describe "type"
            [ Test.test "unitTypeReference"
                (\() ->
                    "()"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "unitTypeReference with spaces"
                (\() ->
                    "( )"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.type_
                )
            , Test.test "tupledTypeReference"
                (\() ->
                    "( (), ())"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.type_
                )
            , Test.test "tupledTypeReference 2"
                (\() ->
                    -- TODO This feels incorrect, there should be a Parenthesized type for this
                    "( () )"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } Elm.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "tupledTypeReference 3"
                (\() ->
                    "( () , Maybe m )"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                            )
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                            )
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.TypeAnnotation.Record []))
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.type_
                )
            , Test.test "recordTypeReference nested record"
                (\() ->
                    "{color: {r : Int, g :Int, b: Int } }"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.type_
                )
            , Test.test "parseTypeWith good indent"
                (\() ->
                    "Maybe\n a"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.type_
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
        , Test.describe "expression"
            [ Test.test "operatorToken 11 -- is not an operator"
                (\() ->
                    Elm.Parser.TestUtil.parse "a = (--)" ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 14"
                (\() ->
                    Elm.Parser.TestUtil.parse "a = (=)" ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 15"
                (\() ->
                    Elm.Parser.TestUtil.parse "a = (?)" ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "empty"
                (\() ->
                    "a = "
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
                )
            , Test.test "Integer literal"
                (\() ->
                    "101"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 101))
                )
            , Test.test "Hex integer literal"
                (\() ->
                    "0x56"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.Hex 86))
                )
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.Literal "Bar"))
                )
            , Test.test "multiline string"
                (\() ->
                    Elm.Parser.TestUtil.parse "\"\"\"Bar foo \n a\"\"\"" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "multiline string escape"
                (\() ->
                    Elm.Parser.TestUtil.parse """\"\"\" \\\"\"\" \"\"\"""" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal """ \"\"\" """))
                )
            , Test.test "character escaped"
                (\() ->
                    Elm.Parser.TestUtil.parse "'\\''" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\''))
                )
            , Test.test "character escaped - 2"
                (\() ->
                    Elm.Parser.TestUtil.parse "'\\r'" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char"
                (\() ->
                    Elm.Parser.TestUtil.parse "'\\u{000D}'" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char with lowercase hex"
                (\() ->
                    Elm.Parser.TestUtil.parse "'\\u{000d}'" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "string escaped 3"
                (\() ->
                    Elm.Parser.TestUtil.parse "\"\\\"\"" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal "\""))
                )
            , Test.test "string escaped"
                (\() ->
                    Elm.Parser.TestUtil.parse "\"foo\\\\\"" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal "foo\\"))
                )
            , Test.test "character escaped 3"
                (\() ->
                    Elm.Parser.TestUtil.parse "'\\n'" ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\n'))
                )
            , Test.test "long string"
                (\() ->
                    Elm.Parser.TestUtil.parse longString ElmSyntaxParserLenient.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "long multi line string"
                (\() ->
                    Elm.Parser.TestUtil.parse longMultiLineString ElmSyntaxParserLenient.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "character literal"
                (\() ->
                    "'c'"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.CharLiteral 'c'))
                )
            , Test.test "tuple expression"
                (\() ->
                    "(1,2)"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Elm.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Elm.Syntax.Expression.Literal "\\{\\}"))
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Elm.Syntax.Expression.Literal "\\a-blablabla-\\b"))
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "Bar"))
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [ "Bar" ] "foo"))
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Elm.Syntax.Expression.ParenthesizedExpression
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments ElmSyntaxParserLenient.expression
                            { ast = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.ListExpr [])
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                            }
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [ "Html" ] "text"))
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                                (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")))
                            )
                )
            , Test.test "negated expression in application"
                (\() ->
                    "toFloat -5"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
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
            , Test.test "glsl block"
                (\() ->
                    "[glsl| precision mediump float; |]"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (Elm.Syntax.Expression.GLSLExpression " precision mediump float; ")
                            )
                )
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.Pattern.UnitPattern)
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments ElmSyntaxParserLenient.pattern
                            { ast = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } } Elm.Syntax.Pattern.UnitPattern
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } } "-- comment" ]
                            }
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.StringPattern "Foo"))
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.CharPattern 'f'))
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } Elm.Syntax.Pattern.AllPattern)
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Elm.Syntax.Pattern.ParenthesizedPattern
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.VarPattern "x"))
                                )
                            )
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.IntPattern 1))
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.HexPattern 1))
                )
            , Test.test "Float should not be valid" (\() -> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.VarPattern "n"))
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } (Elm.Syntax.Pattern.VarPattern "tail"))
                                )
                            )
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                                )
                            )
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                                )
                            )
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                            )
                )
            , Test.test "Named pattern without and with spacing should parse to the same"
                (\() ->
                    Elm.Parser.ParserWithCommentsTestUtil.parse "Bar " ElmSyntaxParserLenient.pattern
                        |> Expect.equal (Elm.Parser.ParserWithCommentsTestUtil.parse "Bar" ElmSyntaxParserLenient.pattern)
                )
            , Test.test "Qualified named"
                (\() ->
                    "Basics.True"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Elm.Syntax.Pattern.NamedPattern { moduleName = [ "Basics" ], name = "True" } [])
                            )
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "Nested tuple"
                (\() ->
                    "(a,{b,c},())"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse consecutive as"
                (\() ->
                    "x as y as z"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as"
                (\() ->
                    "x as y :: z"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as even when :: was already used before"
                (\() ->
                    "w :: x as y :: z"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is an invalid variable name"
                (\() ->
                    "x as _y"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is not a variable name"
                (\() ->
                    "x as 1"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid ElmSyntaxParserLenient.pattern
                )
            , Test.test "Record as"
                (\() ->
                    "{model,context} as appState"
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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
                        |> Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.pattern
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


longString : String
longString =
    "\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\""


longMultiLineString : String
longMultiLineString =
    "\"\"\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\"\"\""
