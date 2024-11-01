module Elm.Parser.CommentTest exposing (all)

import Elm.Parser.Comments
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Node
import Expect
import ParserFast
import Rope
import Test


all : Test.Test
all =
    Test.describe "CommentTests"
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
        , Test.test "module documentation"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parseWithState "{-|foo\nbar-}" (Elm.Parser.Comments.moduleDocumentation |> ParserFast.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}" ])
            )
        , Test.test "module documentation can handle nested comments"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parseWithState "{-| {- hello -} -}" (Elm.Parser.Comments.moduleDocumentation |> ParserFast.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| {- hello -} -}" ])
            )
        ]


parseSingleLineComment : String -> Maybe (Elm.Syntax.Node.Node String)
parseSingleLineComment source =
    ParserFast.run
        Elm.Parser.Comments.singleLineComment
        source


parseMultiLineComment : String -> Maybe (Elm.Syntax.Node.Node String)
parseMultiLineComment source =
    ParserFast.run
        Elm.Parser.Comments.multilineComment
        source
