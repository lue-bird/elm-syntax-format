module ElmSyntaxParserLenientTest exposing (all)

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.TypeAnnotation
import ElmSyntaxParserLenient
import ElmSyntaxParserLenientTestFullModules
import Expect
import Test


all : Test.Test
all =
    Test.concat
        [ Test.describe "FileTests"
            (List.map
                (\( n, s ) ->
                    Test.test ("sample " ++ String.fromInt n)
                        (\() ->
                            case ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ s of
                                Nothing ->
                                    Expect.fail "failed to parse"

                                Just _ ->
                                    Expect.pass
                        )
                )
                ElmSyntaxParserLenientTestFullModules.allSamples
            )
        , -- , describe "Error messages" <|
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
          Test.test "moduleName"
            (\() ->
                "Foo"
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo" ])
            )
        , Test.test "moduleNameDir"
            (\() ->
                "Foo.Bar"
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo", "Bar" ])
            )
        , Test.describe "layout"
            [ Test.test "positively indented across multiple linebreaks and comments"
                (\() ->
                    """a =
        --x
        {- foo 
-}

    f 0"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "positively indented, too few spaces"
                (\() ->
                    """a = f
0"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "top indented across multiple linebreaks and comments"
                (\() ->
                    """a =
    let
        b = 0

        --x
        {- foo 
-}


        c = 0
    in
    b"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "top indented, too many spaces"
                (\() ->
                    """a =
    let
        b = 0
         c = 0
    in
    b"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.describe "comment"
                [ Test.test "singleLineComment"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment state"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment including 2-part utf-16 char range"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.singleLineComment "--bar🔧"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "--bar🔧"))
                    )
                , Test.test "singleLineComment does not include new line"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.singleLineComment "--bar\n"
                            |> Expect.equal Nothing
                    )
                , Test.test "multilineComment parse result"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.multiLineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment range"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.multiLineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment including 2-part utf-16 char range"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.multiLineComment "{-foo\nbar🔧-}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } } "{-foo\nbar🔧-}"))
                    )
                , Test.test "nested multilineComment only open"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.multiLineComment "{- {- -}"
                            |> Expect.equal Nothing
                    )
                , Test.test "nested multilineComment open and close"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.multiLineComment "{- {- -} -}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
                    )
                , Test.test "multilineComment on module documentation"
                    (\() ->
                        ElmSyntaxParserLenient.run ElmSyntaxParserLenient.multiLineComment "{-|foo\nbar-}"
                            |> Expect.equal Nothing
                    )
                ]
            ]
        , Test.describe "module header"
            [ Test.test "formatted moduleDefinition"
                (\() ->
                    "module Foo exposing (Bar)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
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
                )
            , Test.test "port moduleDefinition"
                (\() ->
                    "port module Foo exposing (Bar)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
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
                )
            , Test.test "port moduleDefinition with spacing"
                (\() ->
                    "port module Foo exposing ( Bar )"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
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
                )
            , Test.test "effect moduleDefinition"
                (\() ->
                    "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 80 } }
                                (Elm.Syntax.Module.EffectModule
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Foo" ]
                                    , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 66 }, end = { row = 1, column = 80 } } (Elm.Syntax.Exposing.Explicit [ Elm.Syntax.Node.Node { start = { row = 1, column = 76 }, end = { row = 1, column = 79 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Bar") ])
                                    , command = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 41 } } "MyCmd")
                                    , subscription = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 58 }, end = { row = 1, column = 63 } } "MySub")
                                    }
                                )
                            )
                )
            , Test.test "unformatted"
                (\() ->
                    "module \n Foo \n exposing  (..)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 16 } }
                                (Elm.Syntax.Module.NormalModule
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } [ "Foo" ]
                                    , exposingList =
                                        Elm.Syntax.Node.Node { start = { row = 3, column = 2 }, end = { row = 3, column = 16 } }
                                            (Elm.Syntax.Exposing.All { start = { row = 3, column = 13 }, end = { row = 3, column = 15 } })
                                    }
                                )
                            )
                )
            , Test.test "allow module name without indentation"
                (\() ->
                    """module 
Foo 
 exposing  (..)"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.moduleHeader
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "exposing all"
                (\() ->
                    "module Foo exposing (..)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (Elm.Syntax.Module.NormalModule
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                            (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                    }
                                )
                            )
                )
            , Test.test "module name with _"
                (\() ->
                    "module I_en_gb exposing (..)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.moduleHeader
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                                (Elm.Syntax.Module.NormalModule
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } [ "I_en_gb" ]
                                    , exposingList =
                                        Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 29 } }
                                            (Elm.Syntax.Exposing.All { start = { row = 1, column = 26 }, end = { row = 1, column = 28 } })
                                    }
                                )
                            )
                )
            , Test.describe "uppercase name"
                [ Test.test "lower and upper simple latin"
                    (\() ->
                        "MyCmd"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.equal (Just "MyCmd")
                    )
                , Test.test "typeName not empty"
                    (\() ->
                        ""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "typeName with number"
                    (\() ->
                        "T1"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.equal (Just "T1")
                    )
                , Test.test "ρ function"
                    (\() ->
                        "ρ"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ε2 function"
                    (\() ->
                        "ε2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "εε function"
                    (\() ->
                        "εε"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ρ uppercase function"
                    (\() ->
                        String.toUpper "ρ"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ε uppercase function"
                    (\() ->
                        String.toUpper "ε"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ρ type name"
                    (\() ->
                        "ρ"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ε2 type name"
                    (\() ->
                        "ε2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "εε type name"
                    (\() ->
                        "εε"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ρ uppercase type name"
                    (\() ->
                        String.toUpper "ρ"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ε uppercase type name"
                    (\() ->
                        String.toUpper "ε"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameUppercase
                            |> Expect.notEqual Nothing
                    )
                ]
            , Test.describe "lowercase name"
                [ Test.test "simple latin"
                    (\() ->
                        "foo"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal (Just "foo")
                    )
                , Test.test "functionName may not be a keyword"
                    (\() ->
                        "type"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName may be a keyword suffixed with an underscore"
                    (\() ->
                        "type_"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal (Just "type_")
                    )
                , Test.test "functionName not empty"
                    (\() ->
                        ""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName with number"
                    (\() ->
                        "n1"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal (Just "n1")
                    )
                , Test.test "alias can be a functionName (it is not reserved)"
                    (\() ->
                        "alias"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal (Just "alias")
                    )
                , Test.test "infix can be a functionName (it is not reserved)"
                    (\() ->
                        "infix"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal (Just "infix")
                    )
                , Test.test "functionName is not matched with 'if'"
                    (\() ->
                        "if"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName with _"
                    (\() ->
                        "foo_"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.nameLowercase
                            |> Expect.equal (Just "foo_")
                    )
                ]
            , Test.describe "exposing"
                [ Test.test "Exposing all"
                    (\() ->
                        "(..)"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.exposing_
                                (Elm.Syntax.Exposing.All { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } })
                    )
                , Test.test "Exposing all with spacing and comment"
                    (\() ->
                        """(
  .. -- foo
  )"""
                            |> expectSyntaxWithComments ElmSyntaxParserLenient.exposing_
                                { syntax = Elm.Syntax.Exposing.All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } }
                                , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } } "-- foo" ]
                                }
                    )
                , Test.test "should fail to parse multi-line exposing all when closing parens is at the end of a line"
                    (\() ->
                        """exposing (
  ..
)"""
                            |> expectFailsToParse ElmSyntaxParserLenient.exposing_
                    )
                , Test.test "should fail to parse empty with just 1 `.`"
                    (\() ->
                        "exposing ( . )"
                            |> expectFailsToParse ElmSyntaxParserLenient.exposing_
                    )
                , Test.test "should allow `...`"
                    (\() ->
                        "exposing ( ... )"
                            |> expectFailsToParse ElmSyntaxParserLenient.exposing_
                    )
                , Test.test "should fail to parse empty with 2 spaced `.`"
                    (\() ->
                        "exposing (. .)"
                            |> expectFailsToParse ElmSyntaxParserLenient.exposing_
                    )
                , Test.test "should fail to parse empty exposing list"
                    (\() ->
                        "exposing ()"
                            |> expectFailsToParse ElmSyntaxParserLenient.exposing_
                    )
                , Test.test "Explicit exposing list"
                    (\() ->
                        "(Model,Msg(..),Info(..),init,(::))"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.exposing_
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Msg"
                                            , open = Just { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 24 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 29 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with spacing on one line"
                    (\() ->
                        "(Model, Msg, Info   (..)   ,init,(::) )"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.exposing_
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Msg")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with extra commas between exposes"
                    (\() ->
                        "(Model,,Msg,,Info   (..)  ,,init,(::) )"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.exposing_
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Msg")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Explicit exposing list with spaces and newlines"
                    (\() ->
                        """(
      A
    , B(..)
    , Info (..)
         , init    ,
 (::)
    )"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.exposing_
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "A")
                                    , Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "B"
                                            , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Explicit exposing list with extra comma before first expose"
                    (\() ->
                        """(
    , A
    , B(..)
    , Info (..)
         , init    ,
 (::)
    )"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.exposing_
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "A")
                                    , Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "B"
                                            , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Comments inside the exposing clause"
                    (\() ->
                        "(foo\n --bar\n )"
                            |> expectSyntaxWithComments ElmSyntaxParserLenient.exposing_
                                { syntax =
                                    Elm.Syntax.Exposing.Explicit
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                            (Elm.Syntax.Exposing.FunctionExpose "foo")
                                        ]
                                , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 7 } } "--bar" ]
                                }
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
                    """module A exposing (fun1, fun2)

fun1 n =
  fun2 n
  + fun2 n  -- a

fun2 n =
  fun1 n    -- b
"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
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
            , Test.test "import between declarations, no existing import"
                (\() ->
                    """module A exposing (fun1, fun2)

fun1 n =
  fun2 n
import B

fun2 n =
  fun1 n
"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (Elm.Syntax.Module.NormalModule { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (Elm.Syntax.Exposing.Explicit [ Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (Elm.Syntax.Exposing.FunctionExpose "fun1"), Elm.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (Elm.Syntax.Exposing.FunctionExpose "fun2") ]) })
                                , imports =
                                    [ Elm.Syntax.Node.Node { end = { column = 1, row = 3 }, start = { column = 1, row = 3 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } } [ "B" ] }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ Elm.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (Elm.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            (Elm.Syntax.Expression.Application
                                                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                , Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
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
                                , comments = []
                                }
                            )
                )
            , Test.test "import between declarations, existing import"
                (\() ->
                    """module A exposing (fun1, fun2)
import C
fun1 n =
  fun2 n
import B

fun2 n =
  fun1 n
"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (Elm.Syntax.Module.NormalModule { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (Elm.Syntax.Exposing.Explicit [ Elm.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (Elm.Syntax.Exposing.FunctionExpose "fun1"), Elm.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (Elm.Syntax.Exposing.FunctionExpose "fun2") ]) })
                                , imports =
                                    [ Elm.Syntax.Node.Node { end = { column = 1, row = 2 }, start = { column = 1, row = 2 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } } [ "B" ] }
                                    , Elm.Syntax.Node.Node { end = { column = 9, row = 2 }, start = { column = 1, row = 2 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 2 }, start = { column = 8, row = 2 } } [ "C" ] }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ Elm.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (Elm.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            (Elm.Syntax.Expression.Application
                                                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (Elm.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                , Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
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
                                , comments = []
                                }
                            )
                )
            ]
        , Test.describe "import"
            [ Test.test "import with explicits"
                (\() ->
                    "import Foo exposing (Model, Msg(..))"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
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
            , Test.test "import with exposing ()"
                (\() ->
                    "import Html exposing ()"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias and exposing ()"
                (\() ->
                    "import Html as H exposing ()"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Just (Elm.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } [ "H" ])
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import minimal"
                (\() ->
                    "import Foo"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> expectFailsToParse ElmSyntaxParserLenient.import_
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
                ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ input
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
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
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
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
        , Test.test "port module without port gets parsed to normal module"
            (\() ->
                """
port module Foo exposing (..)

sendResponse =
    Cmd.none
"""
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
                            { comments = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { declaration =
                                            Elm.Syntax.Node.Node { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                                { arguments = []
                                                , expression =
                                                    Elm.Syntax.Node.Node { end = { column = 13, row = 5 }, start = { column = 5, row = 5 } }
                                                        (Elm.Syntax.Expression.FunctionOrValue [ "Cmd" ] "none")
                                                , name = Elm.Syntax.Node.Node { end = { column = 13, row = 4 }, start = { column = 1, row = 4 } } "sendResponse"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Elm.Syntax.Node.Node { end = { column = 30, row = 2 }, start = { column = 1, row = 2 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { exposingList =
                                            Elm.Syntax.Node.Node { end = { column = 30, row = 2 }, start = { column = 17, row = 2 } }
                                                (Elm.Syntax.Exposing.All { end = { column = 29, row = 2 }, start = { column = 27, row = 2 } })
                                        , moduleName = Elm.Syntax.Node.Node { end = { column = 16, row = 2 }, start = { column = 13, row = 2 } } [ "Foo" ]
                                        }
                                    )
                            }
                        )
            )
        , Test.test "normal module with port gets parsed to port module"
            (\() ->
                """
module Foo exposing (..)

port sendResponse : String -> Cmd msg
"""
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Expect.equal
                        (Just
                            { comments = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 1, row = 4 } }
                                    (Elm.Syntax.Declaration.PortDeclaration
                                        { name = Elm.Syntax.Node.Node { end = { column = 18, row = 4 }, start = { column = 6, row = 4 } } "sendResponse"
                                        , typeAnnotation =
                                            Elm.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 21, row = 4 } }
                                                (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Elm.Syntax.Node.Node { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } } ( [], "String" )) [])
                                                    )
                                                    (Elm.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 31, row = 4 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { end = { column = 34, row = 4 }, start = { column = 31, row = 4 } } ( [], "Cmd" ))
                                                            [ Elm.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 35, row = 4 } }
                                                                (Elm.Syntax.TypeAnnotation.GenericType "msg")
                                                            ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Elm.Syntax.Node.Node { end = { column = 25, row = 2 }, start = { column = 1, row = 2 } }
                                    (Elm.Syntax.Module.PortModule
                                        { exposingList =
                                            Elm.Syntax.Node.Node { end = { column = 25, row = 2 }, start = { column = 12, row = 2 } }
                                                (Elm.Syntax.Exposing.All { end = { column = 24, row = 2 }, start = { column = 22, row = 2 } })
                                        , moduleName = Elm.Syntax.Node.Node { end = { column = 11, row = 2 }, start = { column = 8, row = 2 } } [ "Foo" ]
                                        }
                                    )
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
"""
                    ++ comments
                )
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                    |> Maybe.map (\ast -> List.length ast.comments)
                    |> Expect.equal (Just 3000)
            )
        , Test.describe "declaration"
            [ Test.test "value/function declaration"
                (\() ->
                    "foo = bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                    expectFailsToParse ElmSyntaxParserLenient.declaration
                        """foo =
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "simple main declaration"
                (\() ->
                    """main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
            , Test.test "value/function declaration with signature"
                (\() ->
                    """main : Html msg
main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
                            (Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.Declaration.FunctionDeclaration
                                    { declaration =
                                        Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                            { arguments = []
                                            , expression =
                                                Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                    (Elm.Syntax.Expression.Application
                                                        [ Elm.Syntax.Node.Node { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "text")
                                                        , Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                            (Elm.Syntax.Expression.Literal "Hello, World!")
                                                        ]
                                                    )
                                            , name = Elm.Syntax.Node.Node { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } } "main"
                                            }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            (Elm.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 1, row = 1 } }
                                                { name = Elm.Syntax.Node.Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } "main"
                                                , typeAnnotation =
                                                    Elm.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 8, row = 1 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 8, row = 1 } } ( [], "Html" ))
                                                            [ Elm.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 13, row = 1 } }
                                                                (Elm.Syntax.TypeAnnotation.GenericType "msg")
                                                            ]
                                                        )
                                                }
                                            )
                                    }
                                )
                            )
                )
            , Test.test "value/function declaration with signature omitting start name"
                (\() ->
                    """: Html msg
main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
                            (Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.Declaration.FunctionDeclaration
                                    { declaration =
                                        Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                            { arguments = []
                                            , expression =
                                                Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                    (Elm.Syntax.Expression.Application
                                                        [ Elm.Syntax.Node.Node { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                            (Elm.Syntax.Expression.FunctionOrValue [] "text")
                                                        , Elm.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                            (Elm.Syntax.Expression.Literal "Hello, World!")
                                                        ]
                                                    )
                                            , name = Elm.Syntax.Node.Node { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } } "main"
                                            }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            (Elm.Syntax.Node.Node { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                                { name = Elm.Syntax.Node.Node { end = { column = 1, row = 1 }, start = { column = 1, row = 1 } } "main"
                                                , typeAnnotation =
                                                    Elm.Syntax.Node.Node { end = { column = 11, row = 1 }, start = { column = 3, row = 1 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { end = { column = 7, row = 1 }, start = { column = 3, row = 1 } } ( [], "Html" ))
                                                            [ Elm.Syntax.Node.Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } }
                                                                (Elm.Syntax.TypeAnnotation.GenericType "msg")
                                                            ]
                                                        )
                                                }
                                            )
                                    }
                                )
                            )
                )
            , Test.test "function with -> instead of ="
                (\() ->
                    """main ->
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithComments ElmSyntaxParserLenient.declaration
                            { syntax =
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
            , Test.test "type with leading | before first variant"
                (\() ->
                    "type Color=| Blue String | Red | Green"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
            , Test.test "type with extra | between variants"
                (\() ->
                    "type Color = Blue String ||Red | Green"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                    """type D = C B
a"""
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "type with GenericType"
                (\() ->
                    "type Maybe a = Just a | Nothing"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
            , Test.test "allow type with variant name without indentation"
                (\() ->
                    """type Maybe a = Just a |
Nothing"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "fail if declarations not on module-level"
                (\() ->
                    """a = f
    3
    b = 4"""
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "fail if function declaration argument is `as` without parenthesis"
                (\() ->
                    """a foo as bar = f3"""
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "regression test for disallowing ( +)"
                (\() ->
                    "a = ( +)"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "regression test for disallowing (+ )"
                (\() ->
                    "a = (+ )"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "right infix"
                (\() ->
                    "infix right 7 (</>) = slash"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.declaration
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
        , Test.describe "type"
            [ Test.test "unitTypeReference"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "unitTypeReference with spaces"
                (\() ->
                    "( )"
                        |> expectFailsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "tupledTypeReference"
                (\() ->
                    "( (), ())"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectFailsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "tupledTypeReference 2"
                (\() ->
                    "( () )"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } Elm.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "tupledTypeReference 3"
                (\() ->
                    "( () , Maybe m )"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                            )
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                            )
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.TypeAnnotation.Record []))
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
            , Test.test "recordTypeReference one field with name-value separator ="
                (\() ->
                    "{color= String }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
            , Test.test "recordTypeReference one field with name-value separator empty"
                (\() ->
                    "{color  String }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
            , Test.test "type record with prefixed comma"
                (\() ->
                    "{ , a : Int, b : Int }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.TypeAnnotation.Record
                                    [ Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , Elm.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "type record with extra comma between fields"
                (\() ->
                    "{   a : Int,,b : Int }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.TypeAnnotation.Record
                                    [ Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , Elm.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (Elm.Syntax.TypeAnnotation.Typed
                                                (Elm.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with generic"
                (\() ->
                    "{ attr | position : Vec2, texture : Vec2 }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
            , Test.test "record with generic with extra comma between fields"
                (\() ->
                    "{ attr | position : Vec2,,texture : Vec2 }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
            , Test.test "record with generic, with name-value separator = and name-value separator empty"
                (\() ->
                    "{ attr | position   Vec2, texture = Vec2 }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectFailsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "recordTypeReference nested record"
                (\() ->
                    "{color: {r : Int, g :Int, b: Int } }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
            , Test.test "function type reference multiple with consecutive ->"
                (\() ->
                    "Foo->->Bar -> baz"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectFailsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "parseTypeWith good indent"
                (\() ->
                    "Maybe\n a"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.type_
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
                    "a = (--)"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 14"
                (\() ->
                    "a = (=)"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 15"
                (\() ->
                    "a = (?)"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "empty"
                (\() ->
                    "a = "
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.describe "number"
                [ Test.test "long hex"
                    (\() ->
                        "0x03FFFFFF"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Hex 67108863))
                    )
                , Test.test "hex FF"
                    (\() ->
                        "0xFF"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Hex 255))
                    )
                , Test.test "hex"
                    (\() ->
                        "0x2A"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Hex 42))
                    )
                , Test.test "Hex integer literal"
                    (\() ->
                        "0x56"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.Hex 86))
                    )
                , Test.test "Integer literal"
                    (\() ->
                        "101"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 101))
                    )
                , Test.test "float"
                    (\() ->
                        "2.0"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0))
                    )
                , Test.test "integer with negative exponent"
                    (\() ->
                        "2e-2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "integer with negative exponent (uppercase E)"
                    (\() ->
                        "2E-2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "integer with positive exponent"
                    (\() ->
                        "2e+2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 200.0))
                    )
                , Test.test "float with negative exponent"
                    (\() ->
                        "2.0e-2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "float with negative exponent (uppercase E)"
                    (\() ->
                        "2.0E-2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "float with positive exponent"
                    (\() ->
                        "2.0e+2"
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 200.0))
                    )
                ]
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.Literal "Bar"))
                )
            , Test.test "multiline string"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "multiline string escape"
                (\() ->
                    """\"\"\" \\\"\"\" \"\"\""""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal """ \"\"\" """))
                )
            , Test.test "character escaped"
                (\() ->
                    "'\\''"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\''))
                )
            , Test.test "character escaped - 2"
                (\() ->
                    "'\\r'"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char"
                (\() ->
                    "'\\u{000D}'"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char with lowercase hex"
                (\() ->
                    "'\\u{000d}'"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "string escaped 3"
                (\() ->
                    "\"\\\"\""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal "\""))
                )
            , Test.test "string escaped"
                (\() ->
                    "\"foo\\\\\""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.Literal "foo\\"))
                )
            , Test.test "character escaped 3"
                (\() ->
                    "'\\n'"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                        |> Expect.equal (Just (Elm.Syntax.Expression.CharLiteral '\n'))
                )
            , Test.test "long string"
                (\() ->
                    longString
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "long multi line string"
                (\() ->
                    longMultiLineString
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "character literal"
                (\() ->
                    "'c'"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.CharLiteral 'c'))
                )
            , Test.test "tuple expression"
                (\() ->
                    "(1,2)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "String literal multiline"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Elm.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Elm.Syntax.Expression.Literal "\\{\\}"))
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Elm.Syntax.Expression.Literal "\\a-blablabla-\\b"))
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "Bar"))
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [ "Bar" ] "foo"))
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Elm.Syntax.Expression.ParenthesizedExpression
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "ifBlockExpression"
                (\() ->
                    "if True then foo else bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (Elm.Syntax.Expression.IfBlock
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "if-then-else with -> instead of then"
                (\() ->
                    "if True ->   foo else bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithComments ElmSyntaxParserLenient.expression
                            { syntax =
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[ class \"a\",,text \"Foo\"]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithComments ElmSyntaxParserLenient.expression
                            { syntax =
                                Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                    (Elm.Syntax.Expression.ListExpr
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 1)
                                        ]
                                    )
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                            }
                )
            , Test.test "list with extra prefix comma and comment"
                (\() ->
                    "[,1 {- Foo-} ]"
                        |> expectSyntaxWithComments ElmSyntaxParserLenient.expression
                            { syntax =
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
                        |> expectSyntaxWithComments ElmSyntaxParserLenient.expression
                            { syntax = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.ListExpr [])
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                            }
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [ "Html" ] "text"))
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.test "record with name-value separator : and name-value separator = and name-value separator empty and punned fields with each of those"
                (\() ->
                    "{ a 1, b = 2, c : 3, aPunned, bPunned =, cPunned : }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { end = { column = 53, row = 1 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.Expression.RecordExpr
                                    [ Elm.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 3, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } } "a"
                                        , Elm.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 1)
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } "b"
                                        , Elm.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 2)
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 20, row = 1 }, start = { column = 15, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 15, row = 1 } } "c"
                                        , Elm.Syntax.Node.Node { end = { column = 20, row = 1 }, start = { column = 19, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 3)
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } } "aPunned"
                                        , Elm.Syntax.Node.Node { end = { column = 29, row = 1 }, start = { column = 29, row = 1 } }
                                            (Elm.Syntax.Expression.FunctionOrValue [] "aPunned")
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 40, row = 1 }, start = { column = 31, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 38, row = 1 }, start = { column = 31, row = 1 } } "bPunned"
                                        , Elm.Syntax.Node.Node { end = { column = 38, row = 1 }, start = { column = 38, row = 1 } }
                                            (Elm.Syntax.Expression.FunctionOrValue [] "bPunned")
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 52, row = 1 }, start = { column = 42, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 49, row = 1 }, start = { column = 42, row = 1 } } "cPunned"
                                        , Elm.Syntax.Node.Node { end = { column = 49, row = 1 }, start = { column = 49, row = 1 } }
                                            (Elm.Syntax.Expression.FunctionOrValue [] "cPunned")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with prefix comma"
                (\() ->
                    "{ , a = 1, b = 2 }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.Expression.RecordExpr
                                    [ Elm.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Elm.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 1)
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } "b"
                                        , Elm.Syntax.Node.Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 2)
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with extra comma between fields"
                (\() ->
                    "{   a = 1,,b = 2 }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                                (Elm.Syntax.Expression.RecordExpr
                                    [ Elm.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Elm.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 1)
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                        ( Elm.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } "b"
                                        , Elm.Syntax.Node.Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                            (Elm.Syntax.Expression.Integer 2)
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update"
                (\() ->
                    "{ model | count = 1, loading = True }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.test "record update with extra comma between fields"
                (\() ->
                    "{ model | count = 1,,loading = True }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.test "record update with name-value separator : and name-value separator empty"
                (\() ->
                    "{ model | count : 1, loading   True }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "expression ending with an operator should not be valid"
                (\() ->
                    "a = 1++"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple < in a row should not be valid"
                (\() ->
                    "z = a < b < c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple > in a row should not be valid"
                (\() ->
                    "z = a > b > c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple == in a row should not be valid"
                (\() ->
                    "z = a == b == c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple /= in a row should not be valid"
                (\() ->
                    "z = a /= b /= c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple >= in a row should not be valid"
                (\() ->
                    "z = a >= b >= c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple <= in a row should not be valid"
                (\() ->
                    "z = a <= b <= c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "mixing comparison operators without parenthesis should not be valid"
                (\() ->
                    "z = a < b == c"
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , -- TODO introduce validation step for
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
              Test.test "prefix notation"
                (\() ->
                    "(::) x"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                                (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")))
                            )
                )
            , Test.test "negated expression in application"
                (\() ->
                    "toFloat -5"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.describe "lambda"
                [ Test.test "unit lambda"
                    (\() ->
                        "\\() -> foo"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                , Test.test "lambda with => instead of ->"
                    (\() ->
                        "\\() => foo"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (Elm.Syntax.Expression.LambdaExpression
                                        { args = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } Elm.Syntax.Pattern.UnitPattern ]
                                        , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "lambda with . instead of ->"
                    (\() ->
                        "\\().   foo"
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (Elm.Syntax.Expression.LambdaExpression
                                        { args = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } Elm.Syntax.Pattern.UnitPattern ]
                                        , expression = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                ]
            , Test.describe "let-in"
                [ Test.test "let expression with multiple declarations"
                    (\() ->
                        """let
  foo = bar

  john n = n in 1"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse if declarations are not indented the same way"
                    (\() ->
                        """  let
    bar = 1
      foo = 2
  in
  bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "let with deindented expression in in"
                    (\() ->
                        """let
  bar = 1
 in
   bar"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                , Test.test "Let function with type annotation, signature does not repeat the name"
                    (\() ->
                        """let
    : Int
    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                    (Elm.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                                (Elm.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                                                { name = Elm.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 5 } } "bar"
                                                                , typeAnnotation =
                                                                    Elm.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } }
                                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } ( [], "Int" )) [])
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
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation earlier)"
                    (\() ->
                        """let
    bar : Int
      bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation later)"
                    (\() ->
                        """let
       bar : Int
    bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse `as` pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar n as m = 1
        in
        bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "correctly parse variant + args pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar Bar m = 1
        in
        bar"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with `as` not surrounded by parentheses"
                    (\() ->
                        """let
    foo as bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with variant + arguments not surrounded by parentheses"
                    (\() ->
                        """let
    Foo bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "allow let destructuring = to be top indented"
                    (\() ->
                        """let
    (bar)
    =     1
  in
  bar"""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow let destructuring with top indented expression"
                    (\() ->
                        """let
    (bar) =
    1
  in
  bar"""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "should not parse let type annotation without a declaration"
                    (\() ->
                        """let
    bar : Int
  in
  bar"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "letterbox"))
                    )
                ]
            , Test.describe "case-of"
                [ Test.test "allow the matched expression to be top indented"
                    (\() ->
                        """case
True
  of
    A -> 1"""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow the `of` keyword to be top indented"
                    (\() ->
                        """case True
of
               A -> 1"""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow a case first branch pattern to be top indented"
                    (\() ->
                        """case True of
True -> 1"""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow case branch result to be top indented"
                    (\() ->
                        """case f of
  True ->
1"""
                            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "case expression"
                    (\() ->
                        """case f of
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                , Test.test "case expression with `case` after cased expression"
                    (\() ->
                        """f case
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (Elm.Syntax.Expression.CaseExpression
                                        { expression =
                                            Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                                (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( Elm.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                              , Elm.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    (Elm.Syntax.Expression.Integer 1)
                                              )
                                            , ( Elm.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                              , Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    (Elm.Syntax.Expression.Integer 2)
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        expectFailsToParse ElmSyntaxParserLenient.expression
                            """case f of
  True -> 1
 False -> 2"""
                    )
                , Test.test "should fail to parse case expression with second branch indented differently than the first line (after)"
                    (\() ->
                        """case f of
  True -> 1
   False -> 2
"""
                            |> expectFailsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should parse case expression when "
                    (\() ->
                        """case msg of
  Increment ->
    1

  Decrement ->
    2"""
                            |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.test "allow if condition to be top indented"
                (\() ->
                    """a =
    let
        x =
            if
        f y then  1 else 0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow `then` indentation to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True
        then  1 else 0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if `then` to not be positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """a =
    let
        x =
            if True
       then  1 else 0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if-true-branch to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then
        1   else 0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if `else` to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then 1
        else 0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if `else` to not positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """a =
    let
        x =
            if True then 1
       else 0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if if-false-branch to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then 1 else
        0
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record closing curly to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b = 1
        }
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record field value to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b =
        1 }
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record field name to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0,
        b       = 1 }
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record field `=` to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b
        =         1 }
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow tuple closing parens to be top indented"
                (\() ->
                    """a =
    let
        x =
            ( 0, 1
        )
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow first tuple part to be top indented"
                (\() ->
                    """a =
    let
        x =
            (
        0   , 1
            )
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow second tuple part to be top indented"
                (\() ->
                    """a =
    let
        x =
            ( 0,
        1   )
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow infix operator to be top indented"
                (\() ->
                    """a =
    let
        x =
            0
        + 1
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "fail if function call argument is top indented"
                (\() ->
                    """a =
    let
        x =
            f 0
        1
    in
    x"""
                        |> expectFailsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "allow lambda result to be top indented"
                (\() ->
                    """a =
    let
        x =
            \\y ->
        y
    in
    x"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow case branch result call argument to be top indented"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a <|
        \\_ -> ()"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if case branch result call argument to not positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a
  b"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "glsl block"
                (\() ->
                    "[glsl| precision mediump float; |]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (Elm.Syntax.Expression.GLSLExpression " precision mediump float; ")
                            )
                )
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.Pattern.UnitPattern)
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> expectSyntaxWithComments ElmSyntaxParserLenient.pattern
                            { syntax = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } } Elm.Syntax.Pattern.UnitPattern
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } } "-- comment" ]
                            }
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.StringPattern "Foo"))
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.CharPattern 'f'))
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } Elm.Syntax.Pattern.AllPattern)
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Elm.Syntax.Pattern.ParenthesizedPattern
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.VarPattern "x"))
                                )
                            )
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.IntPattern 1))
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.HexPattern 1))
                )
            , Test.test "Float should not be valid" (\() -> expectFailsToParse ElmSyntaxParserLenient.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.VarPattern "n"))
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } (Elm.Syntax.Pattern.VarPattern "tail"))
                                )
                            )
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "list with prefix extra comma"
                (\() ->
                    "[,1]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[1,,2]"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (Elm.Syntax.Pattern.ListPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                        (Elm.Syntax.Pattern.IntPattern 1)
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                        (Elm.Syntax.Pattern.IntPattern 2)
                                    ]
                                )
                            )
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with extra comma between fields"
                (\() ->
                    "{a,, b}"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with prefixed comma"
                (\() ->
                    "{ , a , b }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "a"
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with trailing whitespace"
                (\() ->
                    "{a }"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                                )
                            )
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                                )
                            )
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                            )
                )
            , Test.test "Named pattern without and with spacing should parse to the same"
                (\() ->
                    "Bar "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.pattern
                        |> Expect.equal
                            ("Bar"
                                |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.pattern
                            )
                )
            , Test.test "Qualified named"
                (\() ->
                    "Basics.True"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Elm.Syntax.Pattern.NamedPattern { moduleName = [ "Basics" ], name = "True" } [])
                            )
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "Nested tuple"
                (\() ->
                    "(a,{b,c},())"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse consecutive as"
                (\() ->
                    "x as y as z"
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as"
                (\() ->
                    "x as y :: z"
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as even when :: was already used before"
                (\() ->
                    "w :: x as y :: z"
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is an invalid variable name"
                (\() ->
                    "x as _y"
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is not a variable name"
                (\() ->
                    "x as 1"
                        |> expectFailsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "Record as"
                (\() ->
                    "{model,context} as appState"
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> expectSyntaxWithoutComments ElmSyntaxParserLenient.pattern
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
        , Test.describe "misc comments and operators"
            [ Test.test "function with documentation comment, without signature"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The docs
-}
bar = 1
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 7, column = 8 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Just (Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } } "{-| The docs\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 8 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 8 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "function with documentation and signature"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The docs
-}
bar : Int
bar = 1
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 8, column = 8 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation =
                                                Just (Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } } "{-| The docs\n-}")
                                            , signature =
                                                Just
                                                    (Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 10 } }
                                                        { name = Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } } "bar"
                                                        , typeAnnotation =
                                                            Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 8 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 8, column = 7 }, end = { row = 8, column = 8 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "function with single line comment before"
                (\() ->
                    """
module Bar exposing (..)

--The Doc
bar = 1
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "--The Doc" ]
                                }
                            )
                )
            , Test.test "file with multiple comments"
                (\() ->
                    """
-- comment 1
module Bar exposing (..)

-- comment 2
bar = {- comment 3 -} 1 -- comment 4
 -- comment 5
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments =
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } "-- comment 1"
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 13 } } "-- comment 2"
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 7 }, end = { row = 5, column = 22 } } "{- comment 3 -}"
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 37 } } "-- comment 4"
                                    , Elm.Syntax.Node.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 14 } } "-- comment 5"
                                    ]
                                }
                            )
                )
            , Test.test "function with multi-line comment before"
                (\() ->
                    """
module Bar exposing (..)

{- The Doc -}
bar = 1
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } (Elm.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } } "{- The Doc -}" ]
                                }
                            )
                )
            , Test.test "type alias with documentation"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The Doc -}
type alias Foo
   = { name : String }
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 7, column = 23 } }
                                        (Elm.Syntax.Declaration.AliasDeclaration
                                            { documentation =
                                                Just (Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } } "{-| The Doc -}")
                                            , name = Elm.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 15 } } "Foo"
                                            , generics = []
                                            , typeAnnotation =
                                                Elm.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 23 } }
                                                    (Elm.Syntax.TypeAnnotation.Record
                                                        [ Elm.Syntax.Node.Node { start = { row = 7, column = 8 }, end = { row = 7, column = 21 } }
                                                            ( Elm.Syntax.Node.Node { start = { row = 7, column = 8 }, end = { row = 7, column = 12 } } "name"
                                                            , Elm.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } } ( [], "String" )) [])
                                                            )
                                                        ]
                                                    )
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "choice type with documentation comment"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The Doc -}
type Foo
   = Red
   | Blue
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } (Elm.Syntax.Module.NormalModule { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ], exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }) })
                                , imports =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 8, column = 10 } }
                                        (Elm.Syntax.Declaration.CustomTypeDeclaration
                                            { documentation =
                                                Just
                                                    (Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                                                        "{-| The Doc -}"
                                                    )
                                            , name = Elm.Syntax.Node.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 9 } } "Foo"
                                            , generics = []
                                            , constructors =
                                                [ Elm.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } } "Red", arguments = [] }
                                                , Elm.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } } "Blue", arguments = [] }
                                                ]
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "max call stack size failure"
                (\() ->
                    """module Simplify.AstHelpers exposing (log)


log : Int -> Int
log a =
    Debug.log "ok" a
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 42 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 27 } } [ "Simplify", "AstHelpers" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 42 } }
                                                    (Elm.Syntax.Exposing.Explicit
                                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 38 }, end = { row = 1, column = 41 } } (Elm.Syntax.Exposing.FunctionExpose "log")
                                                        ]
                                                    )
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 6, column = 21 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Just (Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "log", typeAnnotation = Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 17 } } (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } } ( [], "Int" )) [])) (Elm.Syntax.Node.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } } (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } } ( [], "Int" )) []))) })
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 21 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "log"
                                                    , arguments = [ Elm.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (Elm.Syntax.Pattern.VarPattern "a") ]
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 21 } }
                                                            (Elm.Syntax.Expression.Application
                                                                [ Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (Elm.Syntax.Expression.FunctionOrValue [ "Debug" ] "log")
                                                                , Elm.Syntax.Node.Node { start = { row = 6, column = 15 }, end = { row = 6, column = 19 } } (Elm.Syntax.Expression.Literal "ok")
                                                                , Elm.Syntax.Node.Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (Elm.Syntax.Expression.FunctionOrValue [] "a")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "parenthesized infix operations"
                (\() ->
                    """
module Bar exposing (..)

bar = (x + 1) * (2 * y)
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "*"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                            (Elm.Syntax.Expression.OperatorApplication "+"
                                                                                Elm.Syntax.Infix.Left
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (Elm.Syntax.Expression.FunctionOrValue [] "x"))
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Elm.Syntax.Expression.Integer 1))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } }
                                                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } }
                                                                            (Elm.Syntax.Expression.OperatorApplication "*"
                                                                                Elm.Syntax.Infix.Left
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } } (Elm.Syntax.Expression.Integer 2))
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } } (Elm.Syntax.Expression.FunctionOrValue [] "y"))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators consecutive with different associativity loose then tight"
                (\() ->
                    """
module Bar exposing (..)

bar = x + 1 * 2
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "+"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "x"))
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 16 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "*"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Elm.Syntax.Expression.Integer 1))
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } (Elm.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators consecutive with different associativity tight then loose"
                (\() ->
                    """
module Bar exposing (..)

bar = x * 1 + 2
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node
                                        { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "+"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "*"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [] "x"))
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Elm.Syntax.Expression.Integer 1))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } (Elm.Syntax.Expression.Integer 2))
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "negated infix operation"
                (\() ->
                    """
module Bar exposing (..)

bar = -(1 * 2)
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 15 } }
                                                            (Elm.Syntax.Expression.Negation
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 15 } }
                                                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 14 } }
                                                                            (Elm.Syntax.Expression.OperatorApplication
                                                                                "*"
                                                                                Elm.Syntax.Infix.Left
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 10 } } (Elm.Syntax.Expression.Integer 1))
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Elm.Syntax.Expression.Integer 2))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operation into record access"
                (\() ->
                    """
module Bar exposing (..)

bar = (1 * 2).x
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Elm.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Elm.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (Elm.Syntax.Expression.RecordAccess
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                    (Elm.Syntax.Expression.ParenthesizedExpression
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                            (Elm.Syntax.Expression.OperatorApplication
                                                                                "*"
                                                                                Elm.Syntax.Infix.Left
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (Elm.Syntax.Expression.Integer 1))
                                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Elm.Syntax.Expression.Integer 2))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } "x")
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators regression https://github.com/stil4m/elm-syntax/issues/41"
                (\() ->
                    """
module A exposing (..)

bool1 = True && True || True
bool2 = True || True && True

numeric1 = 1 ^ 2 * 3 + 4
numeric2 = 1 + 2 * 3 ^ 4
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (Elm.Syntax.Exposing.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } } "bool1"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 29 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "||"
                                                                Elm.Syntax.Infix.Right
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 21 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "&&"
                                                                        Elm.Syntax.Infix.Right
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 13 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 21 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 25 }, end = { row = 3, column = 29 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 6 } } "bool2"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 29 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "||"
                                                                Elm.Syntax.Infix.Right
                                                                (Elm.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 13 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                                                (Elm.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 29 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "&&"
                                                                        Elm.Syntax.Infix.Right
                                                                        (Elm.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 21 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                                                        (Elm.Syntax.Node.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 29 } } (Elm.Syntax.Expression.FunctionOrValue [] "True"))
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 9 } } "numeric1"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 25 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "+"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 21 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "*"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 17 } }
                                                                            (Elm.Syntax.Expression.OperatorApplication "^"
                                                                                Elm.Syntax.Infix.Right
                                                                                (Elm.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 13 } } (Elm.Syntax.Expression.Integer 1))
                                                                                (Elm.Syntax.Node.Node { start = { row = 6, column = 16 }, end = { row = 6, column = 17 } } (Elm.Syntax.Expression.Integer 2))
                                                                            )
                                                                        )
                                                                        (Elm.Syntax.Node.Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (Elm.Syntax.Expression.Integer 3))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node
                                                                    { start = { row = 6, column = 24 }, end = { row = 6, column = 25 } }
                                                                    (Elm.Syntax.Expression.Integer 4)
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 9 } } "numeric2"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 7, column = 12 }, end = { row = 7, column = 25 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "+"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 7, column = 12 }, end = { row = 7, column = 13 } } (Elm.Syntax.Expression.Integer 1))
                                                                (Elm.Syntax.Node.Node { start = { row = 7, column = 16 }, end = { row = 7, column = 25 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "*"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 7, column = 16 }, end = { row = 7, column = 17 } } (Elm.Syntax.Expression.Integer 2))
                                                                        (Elm.Syntax.Node.Node { start = { row = 7, column = 20 }, end = { row = 7, column = 25 } }
                                                                            (Elm.Syntax.Expression.OperatorApplication "^"
                                                                                Elm.Syntax.Infix.Right
                                                                                (Elm.Syntax.Node.Node { start = { row = 7, column = 20 }, end = { row = 7, column = 21 } } (Elm.Syntax.Expression.Integer 3))
                                                                                (Elm.Syntax.Node.Node { start = { row = 7, column = 24 }, end = { row = 7, column = 25 } } (Elm.Syntax.Expression.Integer 4))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators associativity https://github.com/stil4m/elm-syntax/issues/87"
                (\() ->
                    """
module A exposing (..)

numeric1 = 1 + 2 - 3

pipeline1 = 1 |> 2 |> 3
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (Elm.Syntax.Exposing.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } } "numeric1"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 21 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "-"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 17 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Elm.Syntax.Expression.Integer 1))
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 16 }, end = { row = 3, column = 17 } } (Elm.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } } (Elm.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "|>"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "|>"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (Elm.Syntax.Expression.Integer 1))
                                                                        (Elm.Syntax.Node.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (Elm.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (Elm.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "| is equivalent to |>"
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 | 2 | 3

pipeline1 = 1 |> 2 |> 3
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { moduleName = Elm.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                Elm.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (Elm.Syntax.Exposing.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 22 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 22 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "pipeline0"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 22 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "|>"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 18 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "|>"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Elm.Syntax.Expression.Integer 1))
                                                                        (Elm.Syntax.Node.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 18 } } (Elm.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 3, column = 21 }, end = { row = 3, column = 22 } } (Elm.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = Elm.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                                                    , arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "|>"
                                                                Elm.Syntax.Infix.Left
                                                                (Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "|>"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (Elm.Syntax.Expression.Integer 1))
                                                                        (Elm.Syntax.Node.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (Elm.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Elm.Syntax.Node.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (Elm.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "!= is equivalent to /="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 != 2

pipeline1 = 1 /= 2
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "/="
                                                                Elm.Syntax.Infix.Non
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "/="
                                                                Elm.Syntax.Infix.Non
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { exposingList =
                                                Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Elm.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            , Test.test "!== is equivalent to /="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 !== 2

pipeline1 = 1 /= 2
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "/="
                                                                Elm.Syntax.Infix.Non
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "/="
                                                                Elm.Syntax.Infix.Non
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { exposingList =
                                                Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Elm.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            , Test.test "=== is equivalent to =="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 === 2

pipeline1 = 1 == 2
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "=="
                                                                Elm.Syntax.Infix.Non
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "=="
                                                                Elm.Syntax.Infix.Non
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { exposingList =
                                                Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Elm.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            , Test.test "** is equivalent to ^"
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 ** 2

pipeline1 = 1 ^ 2
"""
                        |> String.trim
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "^"
                                                                Elm.Syntax.Infix.Right
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                        (Elm.Syntax.Declaration.FunctionDeclaration
                                            { declaration =
                                                Elm.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Elm.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Elm.Syntax.Expression.OperatorApplication "^"
                                                                Elm.Syntax.Infix.Right
                                                                (Elm.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 1)
                                                                )
                                                                (Elm.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 17, row = 5 } }
                                                                    (Elm.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Elm.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (Elm.Syntax.Module.NormalModule
                                            { exposingList =
                                                Elm.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Elm.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Elm.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            ]
        ]


parseSource : String -> ElmSyntaxParserLenient.Parser a -> Maybe a
parseSource source parser =
    ElmSyntaxParserLenient.run parser source


moduleExpectInvalid : String -> Expect.Expectation
moduleExpectInvalid source =
    case ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ source of
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


expectSyntaxWithoutComments :
    ElmSyntaxParserLenient.Parser { comments : ElmSyntaxParserLenient.Comments, syntax : a }
    -> a
    -> String
    -> Expect.Expectation
expectSyntaxWithoutComments parser expected source =
    case ElmSyntaxParserLenient.run parser source of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            Expect.all
                [ \() -> actual.syntax |> Expect.equal expected
                , \() ->
                    actual.comments
                        |> ElmSyntaxParserLenient.commentsToList
                        |> Expect.equalLists []
                        |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                ]
                ()


expectSyntaxWithComments :
    ElmSyntaxParserLenient.Parser { comments : ElmSyntaxParserLenient.Comments, syntax : a }
    -> { syntax : a, comments : List (Elm.Syntax.Node.Node String) }
    -> String
    -> Expect.Expectation
expectSyntaxWithComments parser expected source =
    case ElmSyntaxParserLenient.run parser source of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            Expect.all
                [ \() -> actual.syntax |> Expect.equal expected.syntax
                , \() -> actual.comments |> ElmSyntaxParserLenient.commentsToList |> Expect.equal expected.comments
                ]
                ()


expectFailsToParse :
    ElmSyntaxParserLenient.Parser { comments : ElmSyntaxParserLenient.Comments, syntax : a_ }
    -> String
    -> Expect.Expectation
expectFailsToParse parser source =
    case source |> ElmSyntaxParserLenient.run parser of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
