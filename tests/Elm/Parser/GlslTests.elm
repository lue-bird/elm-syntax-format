module Elm.Parser.GlslTests exposing (all)

import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Expression
import Elm.Syntax.Node
import ElmSyntaxParserLenient
import Expect
import Test


all : Test.Test
all =
    Test.describe "GlslTests"
        [ Test.test "case block"
            (\() ->
                "[glsl| precision mediump float; |]"
                    |> expectAst
                        (Elm.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            (Elm.Syntax.Expression.GLSLExpression " precision mediump float; ")
                        )
            )
        ]


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst ElmSyntaxParserLenient.expressionFollowedByOptimisticLayout
