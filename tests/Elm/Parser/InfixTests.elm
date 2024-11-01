module Elm.Parser.InfixTests exposing (all)

import Elm.Parser.Declarations
import Elm.Parser.ParserWithCommentsTestUtil
import Elm.Syntax.Declaration
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Expect
import Test


all : Test.Test
all =
    Test.describe "InfixTests"
        [ Test.test "right infix"
            (\() ->
                "infix right 7 (</>) = slash"
                    |> expectAst
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
                    |> expectAst
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
                    |> expectAst
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


expectAst : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> String -> Expect.Expectation
expectAst =
    Elm.Parser.ParserWithCommentsTestUtil.expectAst Elm.Parser.Declarations.declaration
