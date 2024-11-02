module Elm.Parser.LayoutTests exposing (all)

import Elm.Parser.Layout
import Elm.Syntax.Node
import ElmSyntaxParserLenient
import Expect
import ParserFast
import Test


all : Test.Test
all =
    Test.describe "LayoutTests"
        [ Test.test "empty"
            (\() ->
                "."
                    |> ElmSyntaxParserLenient.run
                        (ParserFast.symbolFollowedBy "."
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        )
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "just whitespace"
            (\() ->
                " "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "spaces followed by new line"
            (\() ->
                " \n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Expect.equal Nothing
            )
        , Test.test "with newline and higher indent 2"
            (\() ->
                "\n  "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "with newline and higher indent 3"
            (\() ->
                " \n "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "maybeLayout with multiline comment"
            (\() ->
                "\n--x\n{- foo \n-}\n "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "maybeLayout with documentation comment fails"
            (\() ->
                "\n--x\n{-| foo \n-}\n "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Expect.equal Nothing
            )
        , Test.test "with newline and higher indent 4"
            (\() ->
                " \n  "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "newlines spaces and single line comments"
            (\() ->
                "\n\n      --time\n  "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented"
            (\() ->
                " \n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented multi line"
            (\() ->
                " \n      \n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented too much"
            (\() ->
                " \n "
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Expect.equal Nothing
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented with comments"
            (\() ->
                "-- foo\n  --bar\n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented with comments 2"
            (\() ->
                "\n--x\n{- foo \n-}\n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented with documentation comment fails"
            (\() ->
                "\n--x\n{-| foo \n-}\n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Expect.equal Nothing
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented some"
            (\() ->
                "..\n  \n  "
                    |> ElmSyntaxParserLenient.run
                        (ParserFast.symbolFollowedBy ".."
                            (ParserFast.withIndentSetToColumn Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented)
                        )
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented with comments multi empty line preceding"
            (\() ->
                "\n\n --bar\n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented with multiple new lines"
            (\() ->
                "..\n  \n    \n\n  "
                    |> ElmSyntaxParserLenient.run
                        (ParserFast.symbolFollowedBy ".."
                            (ParserFast.withIndentSetToColumn Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented)
                        )
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.test "whitespaceAndCommentsEndsTopIndented with multiline comment plus trailing whitespace"
            (\() ->
                "\n{- some note -}    \n"
                    |> ElmSyntaxParserLenient.run Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                    |> Maybe.map (\_ -> ())
                    |> Expect.equal (Just ())
            )
        , Test.describe "comment"
            [ Test.test "singleLineComment"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.singleLineComment "--bar"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                )
            , Test.test "singleLineComment state"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.singleLineComment "--bar"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                )
            , Test.test "singleLineComment including 2-part utf-16 char range"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.singleLineComment "--bar🔧"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "--bar🔧"))
                )
            , Test.test "singleLineComment does not include new line"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.singleLineComment "--bar\n"
                        |> Expect.equal Nothing
                )
            , Test.test "multilineComment parse result"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.multilineComment "{-foo\nbar-}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                )
            , Test.test "multilineComment range"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.multilineComment "{-foo\nbar-}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                )
            , Test.test "multilineComment including 2-part utf-16 char range"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.multilineComment "{-foo\nbar🔧-}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } } "{-foo\nbar🔧-}"))
                )
            , Test.test "nested multilineComment only open"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.multilineComment "{- {- -}"
                        |> Expect.equal Nothing
                )
            , Test.test "nested multilineComment open and close"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.multilineComment "{- {- -} -}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
                )
            , Test.test "multilineComment on module documentation"
                (\() ->
                    ParserFast.run Elm.Parser.Layout.multilineComment "{-|foo\nbar-}"
                        |> Expect.equal Nothing
                )
            ]
        ]
