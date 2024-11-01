module Elm.Parser.ParserWithCommentsTestUtil exposing (expectAst, expectAstWithComments, expectAstWithIndent1, expectInvalid, parse, parseWithState)

import Elm.Syntax.Node exposing (Node)
import Expect
import ParserFast
import ParserWithComments exposing (WithComments)
import Rope


parseWithState : String -> ParserFast.Parser (WithComments a) -> Maybe { comments : List (Node String), syntax : a }
parseWithState s p =
    case ParserFast.run p s of
        Nothing ->
            Nothing

        Just commentsAndSyntax ->
            { comments = commentsAndSyntax.comments |> Rope.toList
            , syntax = commentsAndSyntax.syntax
            }
                |> Just


parse : String -> ParserFast.Parser (WithComments a) -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map .syntax


parseWithFailure : String -> ParserFast.Parser (WithComments a) -> Maybe a
parseWithFailure s p =
    case ParserFast.run p s of
        Nothing ->
            Nothing

        Just commentsAndSyntax ->
            Just commentsAndSyntax.syntax


expectAstWithIndent1 : ParserFast.Parser (WithComments a) -> a -> String -> Expect.Expectation
expectAstWithIndent1 parser =
    \expected source ->
        case ParserFast.run parser source of
            Nothing ->
                Expect.fail "Expected the source to be parsed correctly."

            Just actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected
                    , \() ->
                        actual.comments
                            |> Rope.toList
                            |> Expect.equalLists []
                            |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                    ]
                    ()


expectAst : ParserFast.Parser (WithComments a) -> a -> String -> Expect.Expectation
expectAst parser =
    \expected source ->
        case ParserFast.run parser source of
            Nothing ->
                Expect.fail "Expected the source to be parsed correctly"

            Just actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected
                    , \() ->
                        actual.comments
                            |> Rope.toList
                            |> Expect.equalLists []
                            |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                    ]
                    ()


expectAstWithComments : ParserFast.Parser (WithComments a) -> { ast : a, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments parser =
    \expected source ->
        case ParserFast.run parser source of
            Nothing ->
                Expect.fail "Expected the source to be parsed correctly"

            Just actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected.ast
                    , \() -> actual.comments |> Rope.toList |> Expect.equal expected.comments
                    ]
                    ()


expectInvalid : ParserFast.Parser (WithComments a) -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Nothing ->
                Expect.pass

            Just actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
