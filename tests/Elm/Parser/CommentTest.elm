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
        [ Test.test "documentation comment"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parseWithState "{-|foo\nbar-}" (Elm.Parser.Comments.documentationComment |> ParserFast.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}" ])
            )
        , Test.test "documentation comment can handle nested comments"
            (\() ->
                Elm.Parser.ParserWithCommentsTestUtil.parseWithState "{-| {- hello -} -}" (Elm.Parser.Comments.documentationComment |> ParserFast.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| {- hello -} -}" ])
            )
        ]
