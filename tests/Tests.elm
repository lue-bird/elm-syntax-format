module Tests exposing (suite)

import Elm.Parser
import ElmSyntaxFormat
import Expect
import Fuzz
import Print
import Test exposing (Test)


expectPrintedAs : String -> String -> Expect.Expectation
expectPrintedAs expected actual =
    case actual |> Elm.Parser.parseToFile of
        Err deadEnds ->
            Expect.fail ("failed to parse actual source: " ++ (deadEnds |> Debug.toString))

        Ok parsed ->
            parsed
                |> ElmSyntaxFormat.module_
                |> Print.toString
                |> Expect.equal expected


suite : Test
suite =
    Test.describe "Bits"
        [ Test.describe "import"
            [ Test.test "only name, already same line"
                (\() ->
                    """module A exposing (..)
import List"""
                        |> expectPrintedAs
                            """import List"""
                )
            , Test.test "only name, multiline"
                (\() ->
                    """module A exposing (..)
import
    List"""
                        |> expectPrintedAs
                            """import List"""
                )
            , Test.test "name + alias, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList"""
                        |> expectPrintedAs
                            """import List as CoreList"""
                )
            , Test.test "name + alias, multiline"
                (\() ->
                    """module A exposing (..)
import List 
    as CoreList"""
                        |> expectPrintedAs
                            """import List as CoreList"""
                )
            ]
        ]
