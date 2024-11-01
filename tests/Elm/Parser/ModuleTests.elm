module Elm.Parser.ModuleTests exposing (all)

import Elm.Parser.Modules
import Elm.Parser.ParserWithCommentsTestUtil
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
import Test


all : Test.Test
all =
    Test.describe "ModuleTests"
        [ Test.test "formatted moduleDefinition"
            (\() ->
                "module Foo exposing (Bar)"
                    |> expectAst
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
                    |> expectAst
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
                    |> expectAst
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
                    |> expectAst
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
                    |> expectAst
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
                Elm.Parser.ParserWithCommentsTestUtil.parse "module \nFoo \n exposing  (..)" Elm.Parser.Modules.moduleDefinition
                    |> Expect.equal Nothing
            )
        , Test.test "exposing all"
            (\() ->
                "module Foo exposing (..)"
                    |> expectAst
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
                    |> expectAst
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
                parseCore
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
                parseCore
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
                parseCore
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
                parseCore
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
                    |> expectInvalid
            )
        , Test.test "should fail to parse signature for a different function"
            (\() ->
                """module TestModule exposing (..)
a : Int
b = 2
"""
                    |> expectInvalid
            )
        , Test.test "trailing comments at the end of declarations"
            (\() ->
                parseCore """module A exposing (fun1, fun2)

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


expectAst : Elm.Syntax.Module.Module -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst (ParserFast.map (\mod -> { comments = mod.comments, syntax = Elm.Syntax.Node.value mod.syntax }) Elm.Parser.Modules.moduleDefinition)


parseCore : String -> ParserFast.Parser a -> Maybe a
parseCore source parser =
    ParserFast.run parser source


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case ParserFast.run ElmSyntaxParserLenient.module_ source of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
