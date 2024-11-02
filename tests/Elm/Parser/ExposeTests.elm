module Elm.Parser.ExposeTests exposing (all)

import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Exposing
import Elm.Syntax.Node
import ElmSyntaxParserLenient
import Expect
import ParserFast
import ParserWithComments exposing (WithComments)
import Rope
import Test exposing (Test)


all : Test
all =
    Test.describe "ExposeTests"
        [ Test.test "Exposing all"
            (\() ->
                "exposing (..)"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst exposeDefinition (Elm.Syntax.Exposing.All { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } })
            )
        , Test.test "Exposing all with spacing and comment"
            (\() ->
                """exposing (
  .. -- foo
  )"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments exposeDefinition
                        { ast = Elm.Syntax.Exposing.All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } }
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } } "-- foo" ]
                        }
            )
        , Test.test "should fail to parse multi-line exposing all when closing parens is at the end of a line"
            (\() ->
                """exposing (
  ..
)"""
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid exposeDefinition
            )
        , Test.test "should fail to parse empty with just 1 `.`"
            (\() ->
                "exposing ( . )"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid exposeDefinition
            )
        , Test.test "should fail to parse empty with just 3 `...`"
            (\() ->
                "exposing ( ... )"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid exposeDefinition
            )
        , Test.test "should fail to parse empty with 2 spaced `.`"
            (\() ->
                "exposing (. .)"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid exposeDefinition
            )
        , Test.test "should fail to parse empty exposing list"
            (\() ->
                "exposing ()"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectInvalid exposeDefinition
            )
        , Test.test "Explicit exposing list"
            (\() ->
                "exposing (Model,Msg(..),Info(..),init,(::))"
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst exposeDefinition
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
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst exposeDefinition
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
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAst exposeDefinition
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
                    |> Elm.Parser.ParserWithCommentsTestUtil.expectAstWithComments exposeDefinition
                        { ast =
                            Elm.Syntax.Exposing.Explicit
                                [ Elm.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } }
                                    (Elm.Syntax.Exposing.FunctionExpose "foo")
                                ]
                        , comments = [ Elm.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 7 } } "--bar" ]
                        }
            )
        ]


exposeDefinition : ParserFast.Parser (WithComments Elm.Syntax.Exposing.Exposing)
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
