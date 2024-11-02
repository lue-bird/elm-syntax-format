module ParserWithCommentsExpect exposing (failsToParse, syntaxWithComments, syntaxWithoutComments)

import Elm.Syntax.Node
import Expect
import ParserFast
import Rope exposing (Rope)


type alias WithComments res =
    { comments : Comments, syntax : res }


type alias Comments =
    Rope (Elm.Syntax.Node.Node String)


syntaxWithoutComments : ParserFast.Parser (WithComments a) -> a -> String -> Expect.Expectation
syntaxWithoutComments parser expected source =
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


syntaxWithComments : ParserFast.Parser (WithComments a) -> { syntax : a, comments : List (Elm.Syntax.Node.Node String) } -> String -> Expect.Expectation
syntaxWithComments parser expected source =
    case ParserFast.run parser source of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            Expect.all
                [ \() -> actual.syntax |> Expect.equal expected.syntax
                , \() -> actual.comments |> Rope.toList |> Expect.equal expected.comments
                ]
                ()


failsToParse : ParserFast.Parser (WithComments a_) -> String -> Expect.Expectation
failsToParse parser source =
    case source |> ParserFast.run parser of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
