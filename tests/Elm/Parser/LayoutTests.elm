module Elm.Parser.LayoutTests exposing (all)

import Elm.Parser.Layout
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Node
import Expect
import ParserFast
import ParserWithComments
import Test


all : Test.Test
all =
    Test.describe "LayoutTests"
        [ Test.test "empty"
            (\() ->
                parse "." (ParserFast.symbolFollowedBy "." Elm.Parser.Layout.maybeLayout)
                    |> Expect.equal (Just ())
            )
        , Test.test "just whitespace"
            (\() ->
                parse " " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal (Just ())
            )
        , Test.test "spaces followed by new line"
            (\() ->
                parse " \n" Elm.Parser.Layout.maybeLayout
                    |> Expect.equal Nothing
            )
        , Test.test "with newline and higher indent 2"
            (\() ->
                parse "\n  " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal (Just ())
            )
        , Test.test "with newline and higher indent 3"
            (\() ->
                parse " \n " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal (Just ())
            )
        , Test.test "maybeLayout with multiline comment"
            (\() ->
                parse "\n--x\n{- foo \n-}\n " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal (Just ())
            )
        , Test.test "maybeLayout with documentation comment fails"
            (\() ->
                parse "\n--x\n{-| foo \n-}\n " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal Nothing
            )
        , Test.test "with newline and higher indent 4"
            (\() ->
                parse " \n  " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal (Just ())
            )
        , Test.test "newlines spaces and single line comments"
            (\() ->
                parse "\n\n      --time\n  " Elm.Parser.Layout.maybeLayout
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict"
            (\() ->
                parse " \n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict multi line"
            (\() ->
                parse " \n      \n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict too much"
            (\() ->
                parse " \n " Elm.Parser.Layout.layoutStrict
                    |> Expect.equal Nothing
            )
        , Test.test "layoutStrict with comments"
            (\() ->
                parse "-- foo\n  --bar\n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict with comments 2"
            (\() ->
                parse "\n--x\n{- foo \n-}\n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict with documentation comment fails"
            (\() ->
                parse "\n--x\n{-| foo \n-}\n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal Nothing
            )
        , Test.test "layoutStrict some"
            (\() ->
                parse "..\n  \n  "
                    (ParserFast.symbolFollowedBy ".."
                        (ParserFast.withIndentSetToColumn Elm.Parser.Layout.layoutStrict)
                    )
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict with comments multi empty line preceding"
            (\() ->
                parse "\n\n --bar\n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict with multiple new lines"
            (\() ->
                parse "..\n  \n    \n\n  "
                    (ParserFast.symbolFollowedBy ".."
                        (ParserFast.withIndentSetToColumn Elm.Parser.Layout.layoutStrict)
                    )
                    |> Expect.equal (Just ())
            )
        , Test.test "layoutStrict with multiline comment plus trailing whitespace"
            (\() ->
                parse "\n{- some note -}    \n" Elm.Parser.Layout.layoutStrict
                    |> Expect.equal (Just ())
            )
        , Test.describe "comment"
            [ Test.test "singleLineComment"
                (\() ->
                    parseSingleLineComment "--bar"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                )
            , Test.test "singleLineComment state"
                (\() ->
                    parseSingleLineComment "--bar"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                )
            , Test.test "singleLineComment including 2-part utf-16 char range"
                (\() ->
                    parseSingleLineComment "--bar🔧"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "--bar🔧"))
                )
            , Test.test "singleLineComment does not include new line"
                (\() ->
                    parseSingleLineComment "--bar\n"
                        |> Expect.equal Nothing
                )
            , Test.test "multilineComment parse result"
                (\() ->
                    parseMultiLineComment "{-foo\nbar-}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                )
            , Test.test "multilineComment range"
                (\() ->
                    parseMultiLineComment "{-foo\nbar-}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                )
            , Test.test "multilineComment including 2-part utf-16 char range"
                (\() ->
                    parseMultiLineComment "{-foo\nbar🔧-}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } } "{-foo\nbar🔧-}"))
                )
            , Test.test "nested multilineComment only open"
                (\() ->
                    parseMultiLineComment "{- {- -}"
                        |> Expect.equal Nothing
                )
            , Test.test "nested multilineComment open and close"
                (\() ->
                    parseMultiLineComment "{- {- -} -}"
                        |> Expect.equal
                            (Just (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
                )
            , Test.test "multilineComment on module documentation"
                (\() ->
                    parseMultiLineComment "{-|foo\nbar-}"
                        |> Expect.equal Nothing
                )
            ]
        ]


parseSingleLineComment : String -> Maybe (Elm.Syntax.Node.Node String)
parseSingleLineComment source =
    ParserFast.run
        Elm.Parser.Layout.singleLineComment
        source


parseMultiLineComment : String -> Maybe (Elm.Syntax.Node.Node String)
parseMultiLineComment source =
    ParserFast.run
        Elm.Parser.Layout.multilineComment
        source


parse : String -> ParserFast.Parser ParserWithComments.Comments -> Maybe ()
parse source parser =
    Elm.Parser.ParserWithCommentsTestUtil.parseWithState source
        (parser |> ParserFast.map (\c -> { comments = c, syntax = () }))
        |> Maybe.map .syntax
