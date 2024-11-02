module Elm.Parser.FileTests exposing (all)

import Elm.Parser
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
import ParserWithCommentsExpect
import Rope exposing (Rope)
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
                Elm.Parser.TestUtil.parse "Foo" ElmSyntaxParserLenient.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo" ])
            )
        , Test.test "moduleNameDir"
            (\() ->
                Elm.Parser.TestUtil.parse "Foo.Bar" ElmSyntaxParserLenient.moduleName
                    |> Maybe.map Elm.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo", "Bar" ])
            )
        , Test.describe "layout"
            [ Test.test "empty"
                (\() ->
                    "."
                        |> ElmSyntaxParserLenient.run
                            (ParserFast.symbolFollowedBy "."
                                ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                            )
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "just whitespace"
                (\() ->
                    " "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "spaces followed by new line"
                (\() ->
                    " \n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Expect.equal Nothing
                )
            , Test.test "with newline and higher indent 2"
                (\() ->
                    "\n  "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "with newline and higher indent 3"
                (\() ->
                    " \n "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "maybeLayout with multiline comment"
                (\() ->
                    "\n--x\n{- foo \n-}\n "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "maybeLayout with documentation comment fails"
                (\() ->
                    "\n--x\n{-| foo \n-}\n "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Expect.equal Nothing
                )
            , Test.test "with newline and higher indent 4"
                (\() ->
                    " \n  "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "newlines spaces and single line comments"
                (\() ->
                    "\n\n      --time\n  "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented"
                (\() ->
                    " \n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented multi line"
                (\() ->
                    " \n      \n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented too much"
                (\() ->
                    " \n "
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Expect.equal Nothing
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented with comments"
                (\() ->
                    "-- foo\n  --bar\n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented with comments 2"
                (\() ->
                    "\n--x\n{- foo \n-}\n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented with documentation comment fails"
                (\() ->
                    "\n--x\n{-| foo \n-}\n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Expect.equal Nothing
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented some"
                (\() ->
                    "..\n  \n  "
                        |> ElmSyntaxParserLenient.run
                            (ParserFast.symbolFollowedBy ".."
                                (ParserFast.withIndentSetToColumn ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented)
                            )
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented with comments multi empty line preceding"
                (\() ->
                    "\n\n --bar\n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented with multiple new lines"
                (\() ->
                    "..\n  \n    \n\n  "
                        |> ElmSyntaxParserLenient.run
                            (ParserFast.symbolFollowedBy ".."
                                (ParserFast.withIndentSetToColumn ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented)
                            )
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "whitespaceAndCommentsEndsTopIndented with multiline comment plus trailing whitespace"
                (\() ->
                    "\n{- some note -}    \n"
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.whitespaceAndCommentsEndsTopIndented
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.describe "comment"
                [ Test.test "singleLineComment"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment state"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment including 2-part utf-16 char range"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.singleLineComment "--bar🔧"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "--bar🔧"))
                    )
                , Test.test "singleLineComment does not include new line"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.singleLineComment "--bar\n"
                            |> Expect.equal Nothing
                    )
                , Test.test "multilineComment parse result"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.multilineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment range"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.multilineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment including 2-part utf-16 char range"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.multilineComment "{-foo\nbar🔧-}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } } "{-foo\nbar🔧-}"))
                    )
                , Test.test "nested multilineComment only open"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.multilineComment "{- {- -}"
                            |> Expect.equal Nothing
                    )
                , Test.test "nested multilineComment open and close"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.multilineComment "{- {- -} -}"
                            |> Expect.equal
                                (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
                    )
                , Test.test "multilineComment on module documentation"
                    (\() ->
                        ParserFast.run ElmSyntaxParserLenient.multilineComment "{-|foo\nbar-}"
                            |> Expect.equal Nothing
                    )
                ]
            ]
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
                    "module \nFoo \n exposing  (..)"
                        |> ParserFast.run ElmSyntaxParserLenient.moduleHeader
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
            , Test.describe "exposing"
                [ Test.test "Exposing all"
                    (\() ->
                        "exposing (..)"
                            |> ParserWithCommentsExpect.syntaxWithoutComments exposeDefinition (Elm.Syntax.Exposing.All { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } })
                    )
                , Test.test "Exposing all with spacing and comment"
                    (\() ->
                        """exposing (
  .. -- foo
  )"""
                            |> ParserWithCommentsExpect.syntaxWithComments exposeDefinition
                                { syntax = Elm.Syntax.Exposing.All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } }
                                , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } } "-- foo" ]
                                }
                    )
                , Test.test "should fail to parse multi-line exposing all when closing parens is at the end of a line"
                    (\() ->
                        """exposing (
  ..
)"""
                            |> ParserWithCommentsExpect.failsToParse exposeDefinition
                    )
                , Test.test "should fail to parse empty with just 1 `.`"
                    (\() ->
                        "exposing ( . )"
                            |> ParserWithCommentsExpect.failsToParse exposeDefinition
                    )
                , Test.test "should fail to parse empty with just 3 `...`"
                    (\() ->
                        "exposing ( ... )"
                            |> ParserWithCommentsExpect.failsToParse exposeDefinition
                    )
                , Test.test "should fail to parse empty with 2 spaced `.`"
                    (\() ->
                        "exposing (. .)"
                            |> ParserWithCommentsExpect.failsToParse exposeDefinition
                    )
                , Test.test "should fail to parse empty exposing list"
                    (\() ->
                        "exposing ()"
                            |> ParserWithCommentsExpect.failsToParse exposeDefinition
                    )
                , Test.test "Explicit exposing list"
                    (\() ->
                        "exposing (Model,Msg(..),Info(..),init,(::))"
                            |> ParserWithCommentsExpect.syntaxWithoutComments exposeDefinition
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 24 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Msg"
                                            , open = Just { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 33 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 39 }, end = { row = 1, column = 43 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with spacing on one line"
                    (\() ->
                        "exposing (Model, Msg, Info   (..)   ,init,(::) )"
                            |> ParserWithCommentsExpect.syntaxWithoutComments exposeDefinition
                                (Elm.Syntax.Exposing.Explicit
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 21 } } (Elm.Syntax.Exposing.TypeOrAliasExpose "Msg")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 34 } }
                                        (Elm.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } }
                                            }
                                        )
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 38 }, end = { row = 1, column = 42 } } (Elm.Syntax.Exposing.FunctionExpose "init")
                                    , Elm.Syntax.Node.Node { start = { row = 1, column = 43 }, end = { row = 1, column = 47 } } (Elm.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Explicit exposing list with spaces and newlines"
                    (\() ->
                        """exposing
    ( A
    , B(..)
    , Info (..)
         , init    ,
 (::)
    )"""
                            |> ParserWithCommentsExpect.syntaxWithoutComments exposeDefinition
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
                        "exposing (foo\n --bar\n )"
                            |> ParserWithCommentsExpect.syntaxWithComments exposeDefinition
                                { syntax =
                                    Elm.Syntax.Exposing.Explicit
                                        [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } }
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.import_
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.import_
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
                "{-|foo\nbar-}"
                    |> ParserFast.run ElmSyntaxParserLenient.documentationComment
                    |> Expect.equal
                        (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}"))
            )
        , Test.test "documentation comment can handle nested comments"
            (\() ->
                "{-| {- hello -} -}"
                    |> ParserFast.run ElmSyntaxParserLenient.documentationComment
                    |> Expect.equal
                        (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| {- hello -} -}"))
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "unitTypeReference with spaces"
                (\() ->
                    "( )"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "tupledTypeReference"
                (\() ->
                    "( (), ())"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "tupledTypeReference 2"
                (\() ->
                    "( () )"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } Elm.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "tupledTypeReference 3"
                (\() ->
                    "( () , Maybe m )"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                            )
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                            )
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.TypeAnnotation.Record []))
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "recordTypeReference nested record"
                (\() ->
                    "{color: {r : Int, g :Int, b: Int } }"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.type_
                )
            , Test.test "parseTypeWith good indent"
                (\() ->
                    "Maybe\n a"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.type_
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.describe "number"
                [ Test.test "long hex"
                    (\() ->
                        "0x03FFFFFF"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Hex 67108863))
                    )
                , Test.test "hex FF"
                    (\() ->
                        "0xFF"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Hex 255))
                    )
                , Test.test "hex"
                    (\() ->
                        "0x2A"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Hex 42))
                    )
                , Test.test "Hex integer literal"
                    (\() ->
                        "0x56"
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.Hex 86))
                    )
                , Test.test "Integer literal"
                    (\() ->
                        "101"
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.Integer 101))
                    )
                , Test.test "float"
                    (\() ->
                        "2.0"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0))
                    )
                , Test.test "integer with negative exponent"
                    (\() ->
                        "2e-2"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
                    )
                , Test.test "integer with negative exponent (uppercase E)"
                    (\() ->
                        "2E-2"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
                    )
                , Test.test "integer with positive exponent"
                    (\() ->
                        "2e+2"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0e2))
                    )
                , Test.test "float with negative exponent"
                    (\() ->
                        "2.0e-2"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
                    )
                , Test.test "float with negative exponent (uppercase E)"
                    (\() ->
                        "2.0E-2"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
                    )
                , Test.test "float with positive exponent"
                    (\() ->
                        "2.0e+2"
                            |> ParserFast.run ElmSyntaxParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Elm.Syntax.Expression.Floatable 2.0e2))
                    )
                ]
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Expression.Literal "Bar"))
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.CharLiteral 'c'))
                )
            , Test.test "tuple expression"
                (\() ->
                    "(1,2)"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "String literal multiline"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Elm.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Elm.Syntax.Expression.Literal "\\{\\}"))
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Elm.Syntax.Expression.Literal "\\a-blablabla-\\b"))
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "Bar"))
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Elm.Syntax.Expression.FunctionOrValue [ "Bar" ] "foo"))
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Elm.Syntax.Expression.ParenthesizedExpression
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (Elm.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Expression.FunctionOrValue [] "foo"))
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "ifBlockExpression"
                (\() ->
                    "if True then foo else bar"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithComments ElmSyntaxParserLenient.expression
                            { syntax = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Elm.Syntax.Expression.ListExpr [])
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                            }
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [ "Html" ] "text"))
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "expression ending with an operator should not be valid"
                (\() ->
                    "a = 1++"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple < in a row should not be valid"
                (\() ->
                    "z = a < b < c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple > in a row should not be valid"
                (\() ->
                    "z = a > b > c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple == in a row should not be valid"
                (\() ->
                    "z = a == b == c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple /= in a row should not be valid"
                (\() ->
                    "z = a /= b /= c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple >= in a row should not be valid"
                (\() ->
                    "z = a >= b >= c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "multiple <= in a row should not be valid"
                (\() ->
                    "z = a <= b <= c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "mixing comparison operators without parenthesis should not be valid"
                (\() ->
                    "z = a < b == c"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                                (Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Expression.FunctionOrValue [] "x")))
                            )
                )
            , Test.test "negated expression in application"
                (\() ->
                    "toFloat -5"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.describe "let-in"
                [ Test.test "let expression with multiple declarations"
                    (\() ->
                        """let
  foo = bar

  john n = n in 1"""
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse if declarations are not indented the same way"
                    (\() ->
                        """  let
    bar = 1
      foo = 2
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "let with deindented expression in in"
                    (\() ->
                        """let
  bar = 1
 in
   bar"""
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation later)"
                    (\() ->
                        """let
       bar : Int
    bar = 1
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse `as` pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar n as m = 1
        in
        bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "correctly parse variant + args pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar Bar m = 1
        in
        bar"""
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with `as` not surrounded by parentheses"
                    (\() ->
                        """let
    foo as bar = 1
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with variant + arguments not surrounded by parentheses"
                    (\() ->
                        """let
    Foo bar = 1
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with non-positive layout before ="
                    (\() ->
                        """let
    (bar)
    =     1
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with non-positive layout before expression"
                    (\() ->
                        """let
    (bar) =
    1
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should not parse let type annotation without a declaration"
                    (\() ->
                        """let
    bar : Int
  in
  bar"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
                                (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Elm.Syntax.Expression.FunctionOrValue [] "letterbox"))
                    )
                ]
            , Test.describe "case-of"
                [ Test.test "should fail to parse when the matched expression has the wrong indentation"
                    (\() ->
                        """case
True
  of
    A -> 1"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse when the `of` keyword has the wrong indentation"
                    (\() ->
                        """case True
of
               A -> 1"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse a branch at the start of a line"
                    (\() ->
                        """case True of
True -> 1"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should fail to parse when branch body starts at the start of a line"
                    (\() ->
                        """case f of
  True ->
1"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "case expression"
                    (\() ->
                        """case f of
  True -> 1
  False -> 2"""
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
                        ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression """case f of
  True -> 1
 False -> 2"""
                    )
                , Test.test "should fail to parse case expression with second branch indented differently than the first line (after)"
                    (\() ->
                        """case f of
  True -> 1
   False -> 2
"""
                            |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.expression
                    )
                , Test.test "should parse case expression when "
                    (\() ->
                        """case msg of
  Increment ->
    1

  Decrement ->
    2"""
                            |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
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
            , Test.test "fail if condition not positively indented"
                (\() ->
                    """a =
    let
        x =
            if
        f y then  1 else 0
    in
    x"""
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "fail if case branch result call argument not positively indented"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a
  b"""
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.declaration
                )
            , Test.test "glsl block"
                (\() ->
                    "[glsl| precision mediump float; |]"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.expression
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (Elm.Syntax.Expression.GLSLExpression " precision mediump float; ")
                            )
                )
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Elm.Syntax.Pattern.UnitPattern)
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> ParserWithCommentsExpect.syntaxWithComments ElmSyntaxParserLenient.pattern
                            { syntax = Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } } Elm.Syntax.Pattern.UnitPattern
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } } "-- comment" ]
                            }
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Elm.Syntax.Pattern.StringPattern "Foo"))
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.CharPattern 'f'))
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } Elm.Syntax.Pattern.AllPattern)
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Elm.Syntax.Pattern.ParenthesizedPattern
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.VarPattern "x"))
                                )
                            )
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.IntPattern 1))
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.HexPattern 1))
                )
            , Test.test "Float should not be valid" (\() -> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Elm.Syntax.Pattern.VarPattern "n"))
                                    (Elm.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } (Elm.Syntax.Pattern.VarPattern "tail"))
                                )
                            )
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Elm.Syntax.Pattern.ListPattern [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Elm.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Elm.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                                )
                            )
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.RecordPattern
                                    [ Elm.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                                )
                            )
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                            )
                )
            , Test.test "Named pattern without and with spacing should parse to the same"
                (\() ->
                    "Bar "
                        |> ParserFast.run ElmSyntaxParserLenient.pattern
                        |> Expect.equal
                            ("Bar"
                                |> ParserFast.run ElmSyntaxParserLenient.pattern
                            )
                )
            , Test.test "Qualified named"
                (\() ->
                    "Basics.True"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
                            (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Elm.Syntax.Pattern.NamedPattern { moduleName = [ "Basics" ], name = "True" } [])
                            )
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "Nested tuple"
                (\() ->
                    "(a,{b,c},())"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse consecutive as"
                (\() ->
                    "x as y as z"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as"
                (\() ->
                    "x as y :: z"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as even when :: was already used before"
                (\() ->
                    "w :: x as y :: z"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is an invalid variable name"
                (\() ->
                    "x as _y"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is not a variable name"
                (\() ->
                    "x as 1"
                        |> ParserWithCommentsExpect.failsToParse ElmSyntaxParserLenient.pattern
                )
            , Test.test "Record as"
                (\() ->
                    "{model,context} as appState"
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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
                        |> ParserWithCommentsExpect.syntaxWithoutComments ElmSyntaxParserLenient.pattern
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


exposeDefinition : ParserFast.Parser { comments : Rope (Elm.Syntax.Node.Node String), syntax : Elm.Syntax.Exposing.Exposing }
exposeDefinition =
    ParserFast.map2
        (\commentsAfterExposing exposingListInnerResult ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing" ElmSyntaxParserLenient.whitespaceAndCommentsEndsPositivelyIndented)
        ElmSyntaxParserLenient.exposing_


moduleHeaderExpectAst : Elm.Syntax.Module.Module -> String -> Expect.Expectation
moduleHeaderExpectAst =
    ParserWithCommentsExpect.syntaxWithoutComments (ParserFast.map (\mod -> { comments = mod.comments, syntax = Elm.Syntax.Node.value mod.syntax }) ElmSyntaxParserLenient.moduleHeader)


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
