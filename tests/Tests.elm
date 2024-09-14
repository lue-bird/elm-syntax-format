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
                            """module A exposing (..)

import List


"""
                )
            , Test.test "only name, multiline"
                (\() ->
                    """module A exposing (..)
import
    List"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List


"""
                )
            , Test.test "name + alias, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList


"""
                )
            , Test.test "name + alias, multiline"
                (\() ->
                    """module A exposing (..)
import List 
    as CoreList"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList


"""
                )
            , Test.test "name + alias + exposing all, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (..)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (..)


"""
                )
            , Test.test "name + alias + exposing all, multiline"
                (\() ->
                    """module A exposing (..)
import List
    as CoreList
        exposing (..
            )"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (..)


"""
                )
            , Test.test "name + alias + exposing one, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (map)


"""
                )
            , Test.test "name + alias + exposing one, multiline"
                (\() ->
                    """module A exposing (..)
import List as CoreList
    exposing (map
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (map)


"""
                )
            , Test.test "name + alias + exposing multiple, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (filter, map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (filter, map)


"""
                )
            , Test.test "name + alias + exposing multiple, multiline"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (filter,
    map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList
    exposing
        ( filter
        , map
        )


"""
                )
            , Test.test "exposes get sorted"
                (\() ->
                    """module A exposing (..)
import List exposing (map, filter)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List exposing (filter, map)


"""
                )
            , Test.test "exposes get deduplicated"
                (\() ->
                    """module A exposing (..)
import List exposing (List, filter, map, filter, List)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List exposing (List, filter, map)


"""
                )
            , Test.test "open type exposes get deduplicated"
                (\() ->
                    """module A exposing (..)
import Maybe exposing (Maybe(..), map, Maybe)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Maybe exposing (Maybe(..), map)


"""
                )
            , Test.test "open type exposes get deduplicated across imports"
                (\() ->
                    """module A exposing (..)
import Maybe exposing (Maybe(..), map)
import Maybe exposing (Maybe)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Maybe exposing (Maybe(..), map)


"""
                )
            , Test.test "exposes get deduplicated across imports, preserving line offset of higher"
                (\() ->
                    """module A exposing (..)
import Maybe exposing (Maybe)
import Maybe exposing (Maybe(..),
    map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Maybe
    exposing
        ( Maybe(..)
        , map
        )


"""
                )
            , Test.test "import aliases get deduplicated across imports"
                (\() ->
                    """module A exposing (..)
import List exposing (map)
import List as CoreList"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (map)


"""
                )
            , Test.test "imports get sorted"
                (\() ->
                    """module A exposing (..)
import A
import C
import B"""
                        |> expectPrintedAs
                            """module A exposing (..)

import A
import B
import C


"""
                )
            ]
        , Test.describe "module header"
            [ Test.test "exposing all, already on same line"
                (\() ->
                    """module A exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy


"""
                )
            , Test.test "port exposing all, already on same line"
                (\() ->
                    """port module A exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """port module A exposing (..)

import Dummy


"""
                )
            , Test.test "effect where command exposing all, already on same line"
                (\() ->
                    """effect module A where { command = MyCmd } exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """effect module A where { command = MyCmd } exposing (..)

import Dummy


"""
                )
            , Test.test "effect where subscription exposing all, already on same line"
                (\() ->
                    """effect module A where { subscription = MySub } exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """effect module A where { subscription = MySub } exposing (..)

import Dummy


"""
                )
            , Test.test "effect where command, subscription exposing all, already on same line"
                (\() ->
                    """effect module A where { command = MyCmd, subscription = MySub } exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """effect module A where { command = MyCmd, subscription = MySub } exposing (..)

import Dummy


"""
                )
            , Test.test "exposing all, multiline"
                (\() ->
                    """module A
    exposing (
        ..)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy


"""
                )
            , Test.test "exposing one, already on same line"
                (\() ->
                    """module A exposing (a)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (a)

import Dummy


"""
                )
            , Test.test "exposing one, multiline"
                (\() ->
                    """module A
    exposing (
        a)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (a)

import Dummy


"""
                )
            , Test.test "exposing multiple, one line"
                (\() ->
                    """module A exposing ((||), B, C(..), a)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing ((||), B, C(..), a)

import Dummy


"""
                )
            , Test.test "exposing multiple, multiline"
                (\() ->
                    """module A exposing ((||), B, C(..), a
    )
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing
    ( (||)
    , B
    , C(..)
    , a
    )

import Dummy


"""
                )
            ]
        ]
