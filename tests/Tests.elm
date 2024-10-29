module Tests exposing (suite)

import Elm.Parser
import ElmSyntaxFormat
import Expect
import Print
import Test exposing (Test)


suite : Test
suite =
    Test.describe "elm-syntax-format"
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
                -- This is in important difference between module exposing and import exposing.
                (\() ->
                    """module A exposing (..)
import List as CoreList
    exposing (map
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList
    exposing
        ( map
        )



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
            , Test.test "comments in imports (except in exposing list)"
                (\() ->
                    -- elm-format eats comments of earlier duplicate imports
                    """module A exposing (..)
import -- -1
    A
import -- 0
    A -- 1
import B as
    -- 2
    B2
import C as C2
    -- 3
    exposing
    -- 4
    (..)
"""
                        |> expectPrintedAs
                            """module A exposing (..)

-- 1

import
    -- 0
    A
import
    B
        as
            -- 2
            B2
import C as C2
    exposing
        -- 3
        -- 4
        (..)



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
            , Test.test "exposing multiple, invalid @docs, multiline"
                (\() ->
                    """module A exposing ((||), B, C(..), a
    )
{-| A

@docs (&&)
@docs b
@docs
@docsa

-}
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing
    ( (||)
    , B
    , C(..)
    , a
    )

{-| A

@docs (&&)
@docs b
@docs
@docsa

-}

import Dummy



"""
                )
            , Test.test "exposing multiple with @docs tags, not covering all exposes"
                (\() ->
                    """module A exposing ((||), B, C(..), a)
{-| A

@docs (&&)
@docs b
@docs a, B

-}
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing
    ( a, B
    , (||), C(..)
    )

{-| A

@docs (&&)
@docs b
@docs a, B

-}

import Dummy



"""
                )
            ]
        , Test.describe "module documentation"
            [ Test.test "before imports"
                (\() ->
                    """module A exposing (..)
{-| A module about A.
-}
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| A module about A.
-}

import Dummy



"""
                )
            , Test.test "before declarations when no import exists"
                (\() ->
                    """module A exposing (..)
{-| A module about A.
-}
a =
    "a\""""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| A module about A.
-}

a =
    "a"
"""
                )
            ]
        , Test.describe "module-level comments"
            [ Test.test "before imports without module documentation"
                (\() ->
                    """module A exposing (..)
-- A module about A.
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

-- A module about A.

import Dummy



"""
                )
            , Test.test "between module documentation and imports"
                (\() ->
                    """module A exposing (..)
{-| The module about A.
-}
-- A module about A.
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| The module about A.
-}

-- A module about A.

import Dummy



"""
                )
            , Test.test "between module header and module documentation"
                (\() ->
                    """module A exposing (..)
-- A module about A.
{-| The module about A.
-}
import Dummy"""
                        |> expectPrintedAs
                            -- these comments are moved to _after_ the module documentation
                            """module A exposing (..)

{-| The module about A.
-}

-- A module about A.

import Dummy



"""
                )
            , Test.test "between imports and first declaration"
                (\() ->
                    """module A exposing (..)
import Dummy
-- A module about A.
zero = 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy



-- A module about A.


zero =
    0
"""
                )
            , Test.test "between declaration documentation and declaration"
                (\() ->
                    """module A exposing (..)
import Dummy
{-| 0
-}
-- not one
zero = 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy


{-| 0
-}



-- not one


zero =
    0
"""
                )
            , Test.test "between last declaration and end of file"
                (\() ->
                    """module A exposing (..)
import Dummy
zero = 0
-- A module about A."""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy


zero =
    0



-- A module about A.
"""
                )
            ]
        , Test.describe "type alias declaration"
            [ Test.test "multiple parameters"
                (\() ->
                    """module A exposing (..)
type alias T a b =
    ( a, b )"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T a b =
    ( a, b )
"""
                )
            , Test.test "one parameter"
                (\() ->
                    """module A exposing (..)
type alias T a =
    List a"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T a =
    List a
"""
                )
            , Test.test "no parameter"
                (\() ->
                    """module A exposing (..)
type alias T =
    String"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T =
    String
"""
                )
            , Test.test "comments between parameters"
                (\() ->
                    """module A exposing (..)
type alias A parameterA {--} parameterB =
    (parameterA,parameterB)"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias
    A
        parameterA
        {--}
        parameterB
    =
    ( parameterA, parameterB )
"""
                )
            , Test.test "comments before first parameter"
                (\() ->
                    """module A exposing (..)
type alias A {--} parameterA parameterB =
    (parameterA,parameterB)"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias
    A
        {--}
        parameterA
        parameterB
    =
    ( parameterA, parameterB )
"""
                )
            , Test.test "comments between last parameter and type"
                (\() ->
                    """module A exposing (..)
type alias A parameter {- 0 -} =
    {- 1 -}
    parameter"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A parameter =
    {- 0 -}
    {- 1 -}
    parameter
"""
                )
            , Test.test "comments between name and type"
                (\() ->
                    """module A exposing (..)
type alias A {- 0 -} =
    {- 1 -}
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    {- 0 -}
    {- 1 -}
    Int
"""
                )
            ]
        , Test.describe "choice type declaration"
            [ Test.test "multiple parameters, one variant with one parameter"
                (\() ->
                    """module A exposing (..)
type T a b
    = T ( a, b )"""
                        |> expectPrintedAs
                            """module A exposing (..)

type T a b
    = T ( a, b )
"""
                )
            , Test.test "one parameter, one variant with multiple parameters"
                (\() ->
                    """module A exposing (..)
type T a
    = T a a"""
                        |> expectPrintedAs
                            """module A exposing (..)

type T a
    = T a a
"""
                )
            , Test.test "no parameter, one variant without parameters"
                (\() ->
                    """module A exposing (..)
type T
    = T"""
                        |> expectPrintedAs
                            """module A exposing (..)

type T
    = T
"""
                )
            , Test.test "no parameter, one variant with multiple multiline parameters"
                (\() ->
                    """module A exposing (..)
type T a
    = T a (a
    -> a)"""
                        |> expectPrintedAs
                            """module A exposing (..)

type T a
    = T
        a
        (a
         -> a
        )
"""
                )
            , Test.test "no parameter, multiple variants without parameters"
                (\() ->
                    """module A exposing (..)
type T
    = X
    | Y"""
                        |> expectPrintedAs
                            """module A exposing (..)

type T
    = X
    | Y
"""
                )
            , Test.test "comments between type parameters"
                (\() ->
                    """module A exposing (..)
type A parameterA {--} parameterB
    = A (parameterA,parameterB)"""
                        |> expectPrintedAs
                            """module A exposing (..)

type
    A
        parameterA
        {--}
        parameterB
    = A ( parameterA, parameterB )
"""
                )
            , Test.test "comments before first parameter"
                (\() ->
                    """module A exposing (..)
type A {--} parameterA parameterB
    = A (parameterA,parameterB)"""
                        |> expectPrintedAs
                            """module A exposing (..)

type
    A
        {--}
        parameterA
        parameterB
    = A ( parameterA, parameterB )
"""
                )
            , Test.test "comments between last parameter and first variant"
                (\() ->
                    """module A exposing (..)
type A parameter {- 0 -}
    = {- 1 -} A parameter"""
                        |> expectPrintedAs
                            """module A exposing (..)

type A parameter
    = {- 0 -}
      {- 1 -}
      A parameter
"""
                )
            , Test.test "comments between name and first variant"
                (\() ->
                    """module A exposing (..)
type A {- 0 -}
    = {- 1 -} A Int"""
                        |> expectPrintedAs
                            """module A exposing (..)

type A
    = {- 0 -}
      {- 1 -}
      A Int
"""
                )
            , Test.test "comments between variants"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -}
    | {- 1 -} B String"""
                        |> expectPrintedAs
                            """module A exposing (..)

type A
    = A Int
    | {- 0 -}
      {- 1 -}
      B String
"""
                )
            , Test.test "comments before first variant parameter"
                (\() ->
                    """module A exposing (..)
type A
    = A {- 0 -} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)

type A
    = A
        {- 0 -}
        Int
"""
                )
            , Test.test "comments between variant parameters"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)

type A
    = A
        Int
        {- 0 -}
        Int
"""
                )
            ]
        , Test.describe "declaration port"
            [ Test.test "already single-line"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)

port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "documentation comments"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| :blushes:
-}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)

port sendMessage : String -> Cmd msg


{-| :blushes:
-}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "type on next should be single-line"
                (\() ->
                    """port module A exposing (..)
port sendMessage :
    (String -> Cmd msg
    )"""
                        |> expectPrintedAs
                            """port module A exposing (..)

port sendMessage : String -> Cmd msg
"""
                )
            ]
        , Test.describe "declaration infix"
            [ Test.test "type on next should be single-line"
                (\() ->
                    """module A exposing (..)
infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<)  = lt
infix non   4 (>)  = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+)  = add
infix left  6 (-)  = sub
infix left  7 (*)  = mul
infix left  7 (/)  = fdiv
infix left  7 (//) = idiv
infix right 8 (^)  = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR"""
                        |> expectPrintedAs
                            """module A exposing (..)

infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<) = lt
infix non   4 (>) = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+) = add
infix left  6 (-) = sub
infix left  7 (*) = mul
infix left  7 (/) = fdiv
infix left  7 (//) = idiv
infix right 8 (^) = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR
"""
                )
            ]
        , Test.describe "declaration expression"
            [ Test.test "value, single-line annotation as multiline"
                (\() ->
                    """module A exposing (..)
a
    :
 Int
a =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a : Int
a =
    0
"""
                )
            , Test.test "value, comment before single-line annotation"
                (\() ->
                    """module A exposing (..)
a : {- will always be 0 -} Int
a =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a :
    {- will always be 0 -}
    Int
a =
    0
"""
                )
            , Test.test "function, comment before multi-line annotation"
                (\() ->
                    """module A exposing (..)
a : {- will always return 0 -} Int
 -> Int
a _ =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a :
    {- will always return 0 -}
    Int
    -> Int
a _ =
    0
"""
                )
            ]
        , Test.describe "type"
            [ Test.test "all kinds, single-line"
                (\() ->
                    """module A exposing (..)
type alias T a =
    ({a : Basics.Int, b : ()}, { a | v : List String }, {}->a  ->(Int ,Int))"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T a =
    ( { a : Basics.Int, b : () }, { a | v : List String }, {} -> a -> ( Int, Int ) )
"""
                )
            , Test.test "all kinds, multi-line"
                (\() ->
                    """module A exposing (..)
type alias T a =
    ({a : Basics.Int, b : ()}, { a | v : List String }, {}->a  ->(Int ,Int
    ))"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T a =
    ( { a : Basics.Int, b : () }
    , { a | v : List String }
    , {}
      -> a
      ->
        ( Int
        , Int
        )
    )
"""
                )
            , Test.test "function input function is parenthesized"
                (\() ->
                    """module A exposing (..)
type alias T a =
    (((Int -> Int))) -> a"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T a =
    (Int -> Int) -> a
"""
                )
            , Test.test "function comments between types"
                (\() ->
                    """module A exposing (..)
type alias T =
    Int -> -- integer
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T =
    Int
    ->
        -- integer
        Int
"""
                )
            , Test.test "consecutive function comments between types"
                (\() ->
                    """module A exposing (..)
type alias T =
    Int -> -- 0
    -- 1
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias T =
    Int
    ->
        -- 0
        -- 1
        Int
"""
                )
            , Test.test "comments before first record field"
                (\() ->
                    """module A exposing (..)
type alias A = { -- zero
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { -- zero
      zero : Int
    }
"""
                )
            , Test.test "consecutive comments before first record field"
                (\() ->
                    """module A exposing (..)
type alias A = { -- 0
    -- 1
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { -- 0
      -- 1
      zero : Int
    }
"""
                )
            , Test.test "comments between record field name and value"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : -- zero
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { zero :
        -- zero
        Int
    }
"""
                )
            , Test.test "consecutive comments between record field name and value"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : -- 0
    -- 1
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { zero :
        -- 0
        -- 1
        Int
    }
"""
                )
            , Test.test "comments between record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, -- zero
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { zero : Int
    , -- zero
      one : Int
    }
"""
                )
            , Test.test "consecutive comments between record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, -- 0
    -- 1
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { zero : Int
    , -- 0
      -- 1
      one : Int
    }
"""
                )
            , Test.test "comments after record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, one : Int
    -- zero
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { zero : Int
    , one : Int

    -- zero
    }
"""
                )
            , Test.test "consecutive comments after record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, one : Int
    -- 0
    -- 1
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A =
    { zero : Int
    , one : Int

    -- 0
    -- 1
    }
"""
                )
            , Test.test "comments before record extension record variable"
                (\() ->
                    """module A exposing (..)
type alias A r = { -- zero
    r | zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { -- zero
      r
        | zero : Int
    }
"""
                )
            , Test.test "consecutive comments before record extension record variable"
                (\() ->
                    """module A exposing (..)
type alias A r = { -- 0
    -- 1
    r | zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { -- 0
      -- 1
      r
        | zero : Int
    }
"""
                )
            , Test.test "comments before first record extension field"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | -- zero
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { r
        | -- zero
          zero : Int
    }
"""
                )
            , Test.test "consecutive comments before first record extension field"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | -- 0
    -- 1
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { r
        | -- 0
          -- 1
          zero : Int
    }
"""
                )
            , Test.test "comments between record extension field name and value"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : -- zero
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { r
        | zero :
            -- zero
            Int
    }
"""
                )
            , Test.test "comments between record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, -- zero
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { r
        | zero : Int
        , -- zero
          one : Int
    }
"""
                )
            , Test.test "comments after record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, one : Int
    -- zero
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)

type alias A r =
    { r
        | zero : Int
        , one : Int

        -- zero
    }
"""
                )
            ]
        , Test.describe "expression"
            [ Test.test "if-then-else with single-line condition"
                (\() ->
                    """module A exposing (..)
a =
    if
        (True
        )
    then 0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if True then
        0

    else
        1
"""
                )
            , Test.test "if-then-else with multi-line condition"
                (\() ->
                    """module A exposing (..)
a =
    if
        Basics.not
            True
    then 0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if
        Basics.not
            True
    then
        0

    else
        1
"""
                )
            , Test.test "if-then-else with comments before condition"
                (\() ->
                    """module A exposing (..)
a =
    if -- condition
        True then 0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if
        -- condition
        True
    then
        0

    else
        1
"""
                )
            , Test.test "if-then-else with comments before on True branch"
                (\() ->
                    """module A exposing (..)
a =
    if True then -- 0
        0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if True then
        -- 0
        0

    else
        1
"""
                )
            , Test.test "if-then-else with comments before on False branch"
                (\() ->
                    """module A exposing (..)
a =
    if True then 0 else -- 1
        1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if True then
        0

    else
        -- 1
        1
"""
                )
            , Test.test "if-then-else with another if-then-else in the else branch"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else
        (if False then
            1

         else
            2
        )"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if True then
        0

    else if False then
        1

    else
        2
"""
                )
            , Test.test "if-then-else with another parenthesized if-then-else in the else branch with comments"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else
        (-- on False
         if False then
            1

         else
            2
        )"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if True then
        0

    else
        (-- on False
         if False then
            1

         else
            2
        )
"""
                )
            , Test.test "if-then-else with another parenthesized if-then-else in the else branch with comments and comments before parens"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else
        -- on False
        (-- in parens
         if False then
            1

         else
            2
        )"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    if True then
        0

    else
        -- on False
        (-- in parens
         if False then
            1

         else
            2
        )
"""
                )
            , Test.test "if-then-else with comments before if-then-else in the else branch without comments itself (!)"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else -- on False
        ((if False then
            1

        else
            2))"""
                        |> expectPrintedAs
                            -- elm-format prints this really weirdly
                            """module A exposing (..)

a =
    if True then
        0

    else
    -- on False
    if
        False
    then
        1

    else
        2
"""
                )
            , Test.test "case-of with one case"
                (\() ->
                    """module A exposing (..)
a =
    case () of
        () -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    case () of
        () ->
            0
"""
                )
            , Test.test "case-of with consecutive comments before cased expression"
                (\() ->
                    """module A exposing (..)
a =
    case -- 0
    -- 1
    () of
        () -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    case
        -- 0
        -- 1
        ()
    of
        () ->
            0
"""
                )
            , Test.test "case-of with consecutive comments before case pattern"
                (\() ->
                    """module A exposing (..)
a =
    case () of -- 0
        -- 1
        () -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    case () of
        -- 0
        -- 1
        () ->
            0
"""
                )
            , Test.test "case-of with consecutive comments before case result"
                (\() ->
                    """module A exposing (..)
a =
    case () of
        () -> -- 0
            -- 1
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    case () of
        () ->
            -- 0
            -- 1
            0
"""
                )
            , Test.test "case-of with multiple cases"
                (\() ->
                    """module A exposing (..)
a =
    case 0 == 1 of
        True -> 0
        Basics.False -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    case 0 == 1 of
        True ->
            0

        Basics.False ->
            1
"""
                )
            , Test.test "lambda, consecutive comments before result"
                (\() ->
                    """module A exposing (..)
a =
    \\b -> -- 0
    -- 1
    b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \\b ->
        -- 0
        -- 1
        b
"""
                )
            , Test.test "lambda, consecutive comments before first parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\ -- 0
    -- 1
    b -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \\
     -- 0
     -- 1
     b
    ->
        b
"""
                )
            , Test.test "lambda, consecutive comments between parameters"
                (\() ->
                    """module A exposing (..)
a =
    \\b -- 0
    -- 1
    c -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \\
     b
     -- 0
     -- 1
     c
    ->
        b
"""
                )
            , Test.test "let-in with one destructuring declaration"
                (\() ->
                    """module A exposing (..)
a =
    let { b } = {b=0} in b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        { b } =
            { b = 0 }
    in
    b
"""
                )
            , Test.test "let-in with one function declaration without type"
                (\() ->
                    """module A exposing (..)
a =
    let b () = 0 in b ()"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        b () =
            0
    in
    b ()
"""
                )
            , Test.test "let-in with one value declaration without type"
                (\() ->
                    """module A exposing (..)
a =
    let b = 0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration, comments before result"
                (\() ->
                    """module A exposing (..)
a =
    let b = 0 in {- 0 -} b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        b =
            0
    in
    {- 0 -}
    b
"""
                )
            , Test.test "let-in with one value declaration, comments before first let declaration"
                (\() ->
                    """module A exposing (..)
a =
    let -- 0
        b = 0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        -- 0
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration, comments between let declarations"
                (\() ->
                    """module A exposing (..)
a =
    let b = 0
        -- 0
        c = 1 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        b =
            0

        -- 0
        c =
            1
    in
    b
"""
                )
            , Test.test "let-in with one destructuring, consecutive comments before destructured expression"
                (\() ->
                    """module A exposing (..)
a =
    let b = -- 0
            0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        b =
            -- 0
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration with type"
                (\() ->
                    """module A exposing (..)
a =
    let b:Int

        b = 0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        b : Int
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with multiple destructuring declarations"
                (\() ->
                    """module A exposing (..)
a =
    let { b } = {b=0}
        { c } = {c=1}
    in b+c"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    let
        { b } =
            { b = 0 }

        { c } =
            { c = 1 }
    in
    b + c
"""
                )
            , Test.test "|> pipeline, multi-line"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |>
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity
        |> identity identity
        |> identity
"""
                )
            , Test.test "|> pipeline with multi-line function"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity
    identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity
        |> identity
            identity
        |> identity
"""
                )
            , Test.test "|> pipeline with parenthesized multi-line function"
                (\() ->
                    """module A exposing (..)
a =
    identity
        |> (if True then
                identity

            else
                identity
           )
        |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity
        |> (if True then
                identity

            else
                identity
           )
        |> identity
"""
                )
            , Test.test "|> pipeline, single-line"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity |> identity identity |> identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity
        |> identity identity
        |> {- 0 -}
           {- 1 -}
           identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments before not rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> {- 0 -} {- 1 -} identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity
        |> {- 0 -}
           {- 1 -}
           identity identity
        |> identity
"""
                )
            , Test.test "<| pipeline, multi-line"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <|
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <|
        identity identity <|
            identity
"""
                )
            , Test.test "<| pipeline, multi-line, consecutive comments before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| -- 0
    -- 1
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <|
        identity identity <|
            -- 0
            -- 1
            identity
"""
                )
            , Test.test "<| pipeline, multi-line, consecutive comments before non-rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| -- 0
    -- 1
    identity identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <|
        -- 0
        -- 1
        identity identity <|
            identity
"""
                )
            , Test.test "<| pipeline with multi-line application part"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity
    identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <|
        identity
            identity
        <|
            identity
"""
                )
            , Test.test "<| pipeline with multi-line lambda part"
                (\() ->
                    """module A exposing (..)
a =
    identity <| (\\_ -> (identity
    )) <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <|
        (\\_ ->
            identity
        )
        <|
            identity
"""
                )
            , Test.test "<| pipeline, single-line"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <| identity identity <| identity
"""
                )
            , Test.test "<| pipeline, lambda as the rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| (\\_ -> identity)"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <| identity identity <| \\_ -> identity
"""
                )
            , Test.test "<| pipeline, lambda as not the rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| ((\\_ -> identity)) <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    identity <| (\\_ -> identity) <| identity
"""
                )
            , Test.test "various operations, single-line"
                (\() ->
                    """module A exposing (..)
a =
    3 + (4 * 5 // 3) // 5 - 0 |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    3 + (4 * 5 // 3) // 5 - 0 |> identity
"""
                )
            , Test.test "various operations, multi-line"
                (\() ->
                    """module A exposing (..)
a =
    3 + (4 * 5 // 3) // 5 - 0 |>
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    3
        + (4 * 5 // 3)
        // 5
        - 0
        |> identity
"""
                )
            , Test.test "parenthesized with comments before"
                (\() ->
                    """module A exposing (..)
a = ((-- zero
    (0)))"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    (-- zero
     0
    )
"""
                )
            , Test.test "parenthesized with consecutive comments before"
                (\() ->
                    """module A exposing (..)
a = ((-- 0
    -- 1
    (0)))"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    (-- 0
     -- 1
     0
    )
"""
                )
            , Test.test "parenthesized with comments after"
                (\() ->
                    """module A exposing (..)
a = (((0)
    -- zero
   ))"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    (0
     -- zero
    )
"""
                )
            , Test.test "parenthesized with consecutive comments after"
                (\() ->
                    """module A exposing (..)
a = (((0)
    -- 0
    -- 1
   ))"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    (0
     -- 0
     -- 1
    )
"""
                )
            , Test.test "parenthesized with comments before and after"
                (\() ->
                    """module A exposing (..)
a = (-- before
  ((0)
    -- after
   ))"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    (-- before
     0
     -- after
    )
"""
                )
            , Test.test "comments before first list element"
                (\() ->
                    """module A exposing (..)
a = [ -- zero
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    [ -- zero
      0
    ]
"""
                )
            , Test.test "consecutive comments before first list element"
                (\() ->
                    """module A exposing (..)
a = [ -- 0
    -- 1
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    [ -- 0
      -- 1
      0
    ]
"""
                )
            , Test.test "comments between list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, -- zero
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    [ 0
    , -- zero
      0
    ]
"""
                )
            , Test.test "consecutive comments between list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, -- 0
    -- 1
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    [ 0
    , -- 0
      -- 1
      0
    ]
"""
                )
            , Test.test "comments after list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, 0
    -- zero
    ]"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    [ 0
    , 0

    -- zero
    ]
"""
                )
            , Test.test "consecutive comments after list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, 0
    -- 0
    -- 1
    ]"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    [ 0
    , 0

    -- 0
    -- 1
    ]
"""
                )
            , Test.test "comments before first record field"
                (\() ->
                    """module A exposing (..)
a = { -- zero
    zero = 0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    { -- zero
      zero = 0
    }
"""
                )
            , Test.test "comments between record field name and value"
                (\() ->
                    """module A exposing (..)
a = { zero = -- zero
    0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    { zero =
        -- zero
        0
    }
"""
                )
            , Test.test "comments between record fields"
                (\() ->
                    """module A exposing (..)
a = { zero = 0, -- zero
    one = 1 }"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    { zero = 0
    , -- zero
      one = 1
    }
"""
                )
            , Test.test "comments after record fields"
                (\() ->
                    """module A exposing (..)
a = { zero = 0, one = 1
    -- zero
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    { zero = 0
    , one = 1

    -- zero
    }
"""
                )
            , Test.test "char without escapes"
                (\() ->
                    """module A exposing (..)
a = 'n' """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    'n'
"""
                )
            , Test.test "char with escape"
                (\() ->
                    """module A exposing (..)
a = '\\u{000D}' """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    '\\u{000D}'
"""
                )
            , Test.test "single double quote string without escapes"
                (\() ->
                    """module A exposing (..)
a = "normal text" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    "normal text"
"""
                )
            , Test.test "single double quote string with escapes"
                (\() ->
                    """module A exposing (..)
a = "\\"\\\\\\t\\u{000D}" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    "\\"\\\\\\t\\u{000D}"
"""
                )
            , Test.test "triple double quote string single-line without escapes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \"\"\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string multi-line without escapes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"first line
second line
    \"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \"\"\"first line
second line
    \"\"\"
"""
                )
            , Test.test "triple double quote string un-escapes double quote after first before last char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal \\" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \"\"\"normal " text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as first char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \"\"\"\\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as last char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text\\\"\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    \"\"\"normal text\\\"\"\"\"
"""
                )
            , Test.test "comments between parameters"
                (\() ->
                    """module A exposing (..)
a parameterA {--} parameterB =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    parameterA
    {--}
    parameterB
    =
    0
"""
                )
            , Test.test "comments before first parameter"
                (\() ->
                    """module A exposing (..)
a {--} parameterA parameterB =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    {--}
    parameterA
    parameterB
    =
    0
"""
                )
            , Test.test "comments between last parameter and result"
                (\() ->
                    """module A exposing (..)
a parameter {- 0 -} =
    {- 1 -}
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a parameter =
    {- 0 -}
    {- 1 -}
    0
"""
                )
            , Test.test "comments between implementation name and result"
                (\() ->
                    """module A exposing (..)
a {- 0 -} =
    {- 1 -}
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    {- 0 -}
    {- 1 -}
    0
"""
                )
            ]
        , Test.describe "pattern"
            [ Test.test "various patterns"
                (\() ->
                    """module A exposing (..)
a x =
    case x of
        (({y,z}::tail), Maybe.Nothing as nothing, (Just[""],0)) ->
            0
        _ ->
            1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a x =
    case x of
        ( { y, z } :: tail, Maybe.Nothing as nothing, ( Just [ "" ], 0 ) ) ->
            0

        _ ->
            1
"""
                )
            , Test.test "parenthesized with comments only before"
                (\() ->
                    """module A exposing (..)
a ({--}_ as argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    ({--}
     _ as argument
    )
    =
    0
"""
                )
            , Test.test "parenthesized with comments only after"
                (\() ->
                    """module A exposing (..)
a (_ as argument{--}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    (_ as argument
     {--}
    )
    =
    0
"""
                )
            , Test.test "parenthesized with comments before and after"
                (\() ->
                    """module A exposing (..)
a ({--}_ as argument{--}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    ({--}
     _ as argument
     {--}
    )
    =
    0
"""
                )
            , Test.test "as with comments before name"
                (\() ->
                    """module A exposing (..)
a (_ as -- argument
  argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    (_
     as
        -- argument
        argument
    )
    =
    0
"""
                )
            , Test.test "as with multi-line pattern"
                (\() ->
                    """module A exposing (..)
a ([-- in list
    _ 
   ] as argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    ([ -- in list
       _
     ]
     as
        argument
    )
    =
    0
"""
                )
            , Test.test "record with comments before first field"
                (\() ->
                    """module A exposing (..)
a { -- 0
  -- 1
  zero } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    { -- 0
      -- 1
      zero
    }
    =
    0
"""
                )
            , Test.test "record with comments between fields"
                (\() ->
                    """module A exposing (..)
a { zero, -- 0
  -- 1
  one } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    { zero
    , -- 0
      -- 1
      one
    }
    =
    0
"""
                )
            , Test.test "record with comments after fields"
                (\() ->
                    """module A exposing (..)
a { zero,
  one -- 0
  -- 1
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a
    { zero
    , one
      -- 0
      -- 1
    }
    =
    0
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        b :: Just c :: {- 0 -} {- 1 -} d ->
            b

        _ ->
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    case [] of
        b
            :: (Just c)
            :: {- 0 -}
               {- 1 -}
               d
        ->
            b

        _ ->
            0
"""
                )
            ]
        , Test.describe "comment"
            [ Test.test "module level {--} has new lines in front if preceded by other comments"
                (\() ->
                    """module A exposing (..)
a =
    0
--
{--}
--
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



--


{--}
--


b =
    1
"""
                )
            , Test.test "module level {--} eats linebreaks after if not followed by comments"
                (\() ->
                    """module A exposing (..)
a =
    0
--
{--}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



--


{--}
b =
    1
"""
                )
            , Test.test "{- -} trimmed same-line"
                (\() ->
                    """module A exposing (..)
a =
    0
{-x
-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



{- x -}


b =
    1
"""
                )
            , Test.test "{- -} only one linebreak"
                (\() ->
                    """module A exposing (..)
a =
    0
{-
-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



{-  -}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-

-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



{-

-}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks, some characters and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-   x

a-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



{- x

   a
-}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks, indented characters and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-   x
        
   a-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



{- x

   a
-}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks, multiple differently-indented characters and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-   x
        
    a
        b-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)

a =
    0



{- x

   a
       b
-}


b =
    1
"""
                )
            ]
        , Test.describe "full module samples"
            [ Test.test "elm-syntax-sscce"
                (\() ->
                    expectPrintedAsSame
                        -- copied from https://github.com/pdamoc/elm-syntax-sscce (and slightly edited)
                        -- big thanks!
                        """port module Main exposing (Msg(..), Natural, main)

{-| The above declaration shows how to:

  - declare a port module
  - expose all the tags of a custom type (Msg)
  - expose only the type (Natural)

-}

-- Shows how to import everthing from a module (Html). It is recommended to avoid this syntax.
-- Shows how to create an alias for a module name (Events)
-- Shows how to import multiple modules into a single namespace (Math). Use this with great care as it can create confusion about the source of a function.

import Browser
import Html exposing (..)
import Html.Events as Events exposing (onClick)
import Math.Matrix4 as Math
import Math.Vector2 as Math
import Math.Vector3 as Math
import WebGL



-- CUSTOM TYPES


{-| Shows how to define a single variant custom type.
Exposing only the type and not the tags ensures that the values of the type are created only through functions that can enforce constrains.
-}
type Natural
    = Natural Int


{-| Shows how to define a function that creates a value for the above custom type
-}
fromInt : Int -> Natural
fromInt intValue =
    -- Tags of the custom types are also acting as functions that create the custom type.
    -- max function is defined in the Basics module from elm/core and is imported by default. See elm/core for a list of default imports.
    Natural (max intValue 0)


{-| Shows how to unpack custom type parameters. Works only if the type has a single variant.
-}
toInt : Natural -> Int
toInt (Natural value) =
    value


{-| Shows how to define a function in a pointfree style by composing two functions.
-}
toString : Natural -> String
toString =
    -- String.fromInt shows how to use a module that is imported by default.
    toInt >> String.fromInt


{-| Shows how to control the operations on the your custom type.
In this case the code makes sure you are not storing negative values inside the custom type
-}
addInt : Natural -> Int -> Natural
addInt natural intValue =
    let
        -- One ca unpack / destructure a custom type inside a let..in too
        (Natural value) =
            natural
    in
    fromInt (value + intValue)


{-| Adds 42 (the value is an Int written with HEX notation).
-}
addMeaning : Natural -> Natural
addMeaning (Natural value) =
    fromInt (value + 0x2A)


{-| Shows how to create a type alias for a type that extends records. This alias will extend any other record with the field `name`.
-}
type alias Named a =
    { a | name : String }


{-| Shows how to use the above type alias.
-}
type alias NamedValue a =
    Named { value : a }


{-| Shows how to use the values from an extensible record alias fields
-}
namedToHtml : Named a -> Html msg
namedToHtml { name } =
    text name


namedNaturalToHtml : NamedValue Natural -> Html msg
namedNaturalToHtml namedValue =
    div []
        [ namedToHtml namedValue
        , text ": "
        , text (toString namedValue.value)
        ]


{-| Shows how to create a phantom type
-}
type Unit a
    = Unit Int


{-| When adding two units, the type parameter must be the same.
-}
addUnit : Unit a -> Unit a -> Unit a
addUnit (Unit first) (Unit second) =
    Unit (first + second)


{-| A type to be used with the above Unit type
-}
type Meter
    = Meter


{-| A second type to be used with the above Unit type
-}
type Gram
    = Gram


twoMeters : Unit Meter
twoMeters =
    Unit 2


threeMeters : Unit Meter
threeMeters =
    Unit 3


fewGrams : Unit Gram
fewGrams =
    Unit 21


someMeters : Unit Meter
someMeters =
    -- This works because the two units match
    addUnit twoMeters threeMeters



{- This value will throw an error if uncommented
   impossibleAdd : Unit Meter
   impossibleAdd =
       -- This doesn't work because the types don't match
       addUnit fewGrams someMeters
-}
-- MODEL


{-| Shows how to tie a name to a record type.
-}
type alias Model =
    { count : Natural
    , namedCount : NamedValue Natural
    }


{-| Shows how to ignore a parameter you are not using
This purposefully shows a function without a type signature although top level functions and values should have type signatures.
-}
init _ =
    ( { count = Natural 0
      , namedCount =
            { name = "Natural", value = Natural 0 }
      }
    , Cmd.none
    )



-- UPDATE


{-| Shows how to give a new name to a more complex type
-}
type alias Naturals =
    List Natural


{-| Shows how to define a custom type with multiple variants
-}
type Msg
    = Increment
    | Decrement
    | AddNextTen
    | OnSubscription (Result String Naturals)


{-| Shows how to unpack a record parameter while still keeping the full parameter.
You can use a subset of the fields in the record if you only need certain fields.
This function type signature has been purpusefully spread over multiple lines to show that complex signatures need not be single line.
-}
update :
    Msg
    -> Model
    -> ( Model, Cmd msg )
update msg ({ count } as model) =
    case msg of
        Increment ->
            ( { model | count = addInt count 1 }, Cmd.none )

        Decrement ->
            -- Shows how to create a new scope with a let..in expression
            let
                -- values and function defined inside let..in can have type signatures although they usually don't
                newValue : Natural
                newValue =
                    addInt count -1

                -- this is how to use the Debug.log to check for a value
                _ =
                    Debug.log "newValue" newValue

                -- this shows that you can declare multiple _ values without the compiler complaining.
                -- attempting to use a named declaration multiple times will result in a compiler error
                _ =
                    newValue
                        |> -- adding the next line at the end of a declaration with result in it being logged to the JS console
                           Debug.log "newValue"
            in
            if newValue == count then
                -- Shows how to call a port
                ( model, reportError "There are no negative Natural numbers" )

            else
                ( { model | count = newValue }, Cmd.none )

        AddNextTen ->
            let
                addIntToCount =
                    -- Shows how to partially apply a function.
                    -- This is very useful in scopes where the first arguments stay the same
                    addInt count

                intCount =
                    toInt count

                addTen =
                    -- Shows how to use an infix operator as a prefix function.
                    -- This is useful when you want to partially apply the operator and use
                    -- the resulting function in a higher order function.
                    (+) 10

                nextTen =
                    -- Shows how to use an infix operator as an argument in a higher order function.
                    List.foldl (+) 0 (List.range intCount (addTen intCount))
            in
            ( { model | count = addIntToCount nextTen }, Cmd.none )

        -- Shows how to unpack a variant by matching against the contained variants
        OnSubscription (Ok naturals) ->
            case naturals of
                -- Shows how to pattern match on a List
                [] ->
                    -- Shows how to recursively call update in order to avoid duplicating code.
                    update (OnSubscription (Err "Received an empty list")) model

                -- Shows how to pattern match on a list with a fixed number of elements
                [ first ] ->
                    ( { model | count = first }, Cmd.none )

                -- Shows how to pattern match on a list with at least two elements.
                first :: second :: _ ->
                    ( { model | count = first }, Cmd.none )

        OnSubscription (Err error) ->
            ( model, reportError error )



-- VIEW


{-| Shows how to declare a String that spans multiple lines
-}
multiline : String
multiline =
    \"\"\"
    This is a multiline string. 
    It will be displayed by spliting the lines into separate paragraphs. 
\"\"\"


{-| Shows how to define a tuple.
-}
initials : ( Char, Char )
initials =
    -- Show how to declare a Char type.
    ( 'J', 'D' )


view : Model -> Html Msg
view model =
    let
        namedCount =
            -- Shows how to access a field from a record by using a field accessor function
            .namedCount model

        -- Shows how to pattern match a tuple
        ( first, last ) =
            initials

        -- a helper function
        named value =
            { name = value }

        -- shows that record field accessors work on expressions too
        nameFromExpression =
            (named "Foo").name
    in
    main_ []
        [ button [ onClick Increment ] [ text "+1" ]
        , -- Shows how to avoid parentheses by using the backwards pipe operator
          div [] [ text <| toString model.count ]
        , -- Shows how to used a function from a module without having to expose it in the import section.
          div [] [ button [ Events.onClick Decrement ] [ text "-1" ] ]
        , button [ onClick AddNextTen ] [ text "Add Next Ten" ]
        , div [] [ namedNaturalToHtml namedCount ]
        , String.lines multiline
            |> List.map (\\line -> p [] [ text line ])
            |> div []
        , footer [] [ text (String.fromChar first), text (String.fromChar last) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Listen to the incomming port only if condition in the model is met
    if toInt model.count < 5 then
        -- Show how to use an anonymous function (lambda expression)
        fromJS
            (\\value ->
                case List.map String.toInt value of
                    [] ->
                        OnSubscription (Err "Received an empty list")

                    (Just int) :: _ ->
                        let
                            output =
                                -- This shows how to prepend an element to a list
                                fromInt int :: []
                        in
                        if int >= 0 then
                            OnSubscription (Ok output)

                        else
                            -- Shows how to structure a complex function application by using the "pipe" operator
                            ("Received a negative number: " ++ String.fromInt int)
                                |> Err
                                |> OnSubscription

                    -- Shows how to catch all remaining variants. Watch out for this pattern as it can create troubles.
                    _ ->
                        OnSubscription (Err "Received a list that started with a non-integer ")
            )

    else
        Sub.none



-- WIRING


{-| Signature for Browser.element is Program flags model msg.
The flags type argument here is the Unit type: ()
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


{-| Shows how to define an outgoing port
-}
port reportError : String -> Cmd msg


{-| Shows how to define an incomming port.
The first parameter is a function that takes the data received from JS and produces a message that the app understands.
-}
port fromJS : (List String -> msg) -> Sub msg



-- ADVANCED SYNTAX


{-| Elm also has special syntax for declaring WebGL shaders. See more about this at: <https://github.com/elm-explorations/webgl/>
-}
vertexShader : WebGL.Shader { a | coord : Math.Vec3, position : Math.Vec3 } { b | view : Math.Mat4 } { vcoord : Math.Vec2 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform   mat4 view;
varying   vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}
|]
"""
                )

            -- below examples are from elm-review-simplify whose license is
            {- Copyright (c) 2020, Jeroen Engels
               All rights reserved.

               Redistribution and use in source and binary forms, with or without
               modification, are permitted provided that the following conditions are met:

               * Redistributions of source code must retain the above copyright notice, this
                 list of conditions and the following disclaimer.

               * Redistributions in binary form must reproduce the above copyright notice,
                 this list of conditions and the following disclaimer in the documentation
                 and/or other materials provided with the distribution.

               * Neither the name of elm-review-simplify nor the names of its
                 contributors may be used to endorse or promote products derived from
                 this software without specific prior written permission.

               THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
               AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
               IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
               DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
               FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
               DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
               SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
               CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
               OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
               OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
            -}
            , Test.test "Simplify.Evaluate"
                (\() ->
                    expectPrintedAsSame
                        """module Simplify.Evaluate exposing (getBoolean, getInt, getNumber)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize


getBoolean : Infer.Resources a -> Node Expression -> Match Bool
getBoolean resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "True" ->
            case ModuleNameLookupTable.moduleNameFor resources.lookupTable node of
                Just [ "Basics" ] ->
                    Determined True

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ "False" ->
            case ModuleNameLookupTable.moduleNameFor resources.lookupTable node of
                Just [ "Basics" ] ->
                    Determined False

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor resources.lookupTable node
                    |> Maybe.andThen (\\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.FunctionOrValue [ "Basics" ] "True") ->
                    Determined True

                Just (Expression.FunctionOrValue [ "Basics" ] "False") ->
                    Determined False

                Just _ ->
                    Undetermined

                Nothing ->
                    Undetermined

        _ ->
            case
                Infer.isBoolean
                    (Node.value (Normalize.normalize resources node))
                    (Tuple.first resources.inferredConstants)
            of
                Just bool ->
                    Determined bool

                Nothing ->
                    Undetermined


getInt : Infer.Resources a -> Node Expression -> Maybe Int
getInt resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.Integer n ->
            Just n

        Expression.Hex n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getInt resources expr)

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor resources.lookupTable node
                    |> Maybe.andThen (\\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing


getNumber : Infer.Resources a -> Node Expression -> Maybe Float
getNumber resources baseNode =
    let
        unparenthesized : Node Expression
        unparenthesized =
            AstHelpers.removeParens baseNode
    in
    case getInt resources unparenthesized of
        Just int ->
            Just (Basics.toFloat int)

        Nothing ->
            case unparenthesized of
                Node _ (Expression.Floatable float) ->
                    Just float

                Node variableRange (Expression.FunctionOrValue _ name) ->
                    case
                        ModuleNameLookupTable.moduleNameAt resources.lookupTable variableRange
                            |> Maybe.andThen (\\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
                    of
                        Just (Expression.Floatable float) ->
                            Just float

                        _ ->
                            Nothing

                _ ->
                    Nothing
"""
                )
            , Test.test "Simplify.Infer"
                (\() ->
                    expectPrintedAsSame
                        """module Simplify.Infer exposing
    ( DeducedValue(..)
    , Fact(..)
    , Inferred(..)
    , Resources
    , deduceNewFacts
    , empty
    , falseExpr
    , fromList
    , get
    , infer
    , inferForIfCondition
    , isBoolean
    , trueExpr
    )

{-| Infers values from `if` conditions.

This is meant to simplify expressions like the following:

```diff
if a then
   -- we know that `a` is True
-  if a && b then
+  if b then
```


### Mechanism

The way that this is done is by collecting "facts" about the conditions we've found. Given the following expression:

    if a && b == 1 then
        1

    else
        2

we can infer that in the `then` branch, the following facts are true:

  - `a && b == 1` is True
  - `a` is True
  - `b == 1` is True
  - `b` equals `1`

and for the `else` branch, that:

  - `a && b == 1` is False
  - `a` is False OR `b == 1` is False (or that `b` does not equal `1`, not sure how we represent this at the moment)

For a condition like `a || b`, we know that in the `then` branch:

  - `a` is True OR `b` is True

and that in the `else` branch:

  - `a || b` is `False`
  - `a` is `False`
  - `b` is `False`

Whenever we get a new fact from a new `if` condition, we then go through all the previously known facts and see if the
new one can simplify some of the old ones to generate new facts.

For instance, if we knew that `a` is True OR `b` is True, and we encounter `if a then`, then we can infer that for the `else` branch `a` is False.
When comparing that to `a` is True OR `b` is True, we can infer that `b` is True.

Every new fact that we uncover from this comparison will also repeat the process of going through the previous list of facts.

Another thing that we do whenever we encounter a new fact os to try and "deduce" a value from it, which we add to a list
of "deduced" values. A few examples:

  - `a` -> `a` is True
  - `a == 1` -> `a` is equal to `1`
  - `a /= 1` -> Can't infer individual values when this is True
  - `a` OR `b` -> Can't infer individual values when this is True

(with the exception that we can infer that the whole expression is `True` or `False`)

Before we do all of this analysis, we normalize the AST, so we have a more predictable AST and don't have to do as many checks.


### Application

This data is then used in `Normalize` to change the AST, so that a reference to `a` whose value we have "deduced" is
replaced by that value. Finally, that data is also used in functions like `Evaluate.getBoolean`.
(Note: This might be a bit redundant but that's a simplification for later on)

Whenever we see a boolean expression, we will look at whether we can simplify it, and report an error when that happens.


### Limits

The current system has a few holes meaning some things we could infer aren't properly handled, and I'd love help with that.
From the top of my mind, I think that `if x /= 1 then (if x == 1 then ...)` (or some variation) does not get simplified when it could.

We are making special exception for numbers for equality, but we could do more: handling `String`, `Char` and probably more.

The system does not currently handle `case` expressions. While handling pattern matching against literals should not be
too hard with the current system, storing "shapes" of the value (the value is a `Just` of something) probably requires
some work.

-}

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type Inferred
    = Inferred
        { facts : List Fact
        , deduced : AssocList.Dict Expression DeducedValue
        }


type DeducedValue
    = DTrue
    | DFalse
    | DNumber Float
    | DString String


type Fact
    = Equals Expression Expression
    | NotEquals Expression Expression
    | Or (List Fact) (List Fact)


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : ( Inferred, List Inferred )
    }


empty : Inferred
empty =
    Inferred
        { facts = []
        , deduced = AssocList.empty
        }


fromList : List ( Expression, DeducedValue ) -> Inferred
fromList list =
    Inferred
        { facts = []
        , deduced = AssocList.fromList list
        }


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.map
            (\\value ->
                case value of
                    DTrue ->
                        trueExpr

                    DFalse ->
                        falseExpr

                    DNumber float ->
                        Expression.Floatable float

                    DString str ->
                        Expression.Literal str
            )


isBoolean : Expression -> Inferred -> Maybe Bool
isBoolean expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.andThen
            (\\value ->
                case value of
                    DTrue ->
                        Just True

                    DFalse ->
                        Just False

                    DNumber _ ->
                        Nothing

                    DString _ ->
                        Nothing
            )


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] True inferred )
    , ( falseBranchRange, infer [ condition ] False inferred )
    ]


trueExpr : Expression
trueExpr =
    Expression.FunctionOrValue [ "Basics" ] "True"


falseExpr : Expression
falseExpr =
    Expression.FunctionOrValue [ "Basics" ] "False"


convertToFact : Expression -> Bool -> List Fact
convertToFact expr shouldBe =
    if shouldBe then
        [ Equals expr trueExpr, NotEquals expr falseExpr ]

    else
        [ Equals expr falseExpr, NotEquals expr trueExpr ]


infer : List Expression -> Bool -> Inferred -> Inferred
infer nodes shouldBe acc =
    List.foldl (inferHelp shouldBe) acc nodes


inferHelp : Bool -> Expression -> Inferred -> Inferred
inferHelp shouldBe node acc =
    let
        dict : Inferred
        dict =
            injectFacts (convertToFact node shouldBe) acc
    in
    case node of
        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
            inferHelp (not shouldBe) (Node.value expression) dict

        Expression.OperatorApplication "&&" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                infer [ left, right ] shouldBe dict

            else
                injectFacts
                    [ Or
                        (convertToFact left False)
                        (convertToFact right False)
                    ]
                    dict

        Expression.OperatorApplication "||" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                injectFacts
                    [ Or
                        (convertToFact left True)
                        (convertToFact right True)
                    ]
                    dict

            else
                infer [ left, right ] shouldBe dict

        Expression.OperatorApplication "==" inf left right ->
            dict
                |> (if shouldBe then
                        injectFacts [ NotEquals (Expression.OperatorApplication "/=" inf left right) trueExpr ]

                    else
                        identity
                   )
                |> inferOnEquality left right shouldBe
                |> inferOnEquality right left shouldBe

        Expression.OperatorApplication "/=" inf left right ->
            dict
                |> (if shouldBe then
                        injectFacts [ NotEquals (Expression.OperatorApplication "==" inf left right) trueExpr ]

                    else
                        identity
                   )
                |> inferOnEquality left right (not shouldBe)
                |> inferOnEquality right left (not shouldBe)

        _ ->
            dict


injectFacts : List Fact -> Inferred -> Inferred
injectFacts newFacts (Inferred inferred) =
    case newFacts of
        [] ->
            Inferred inferred

        newFact :: restOfFacts ->
            if List.member newFact inferred.facts then
                injectFacts
                    restOfFacts
                    (Inferred inferred)

            else
                let
                    newFactsToVisit : List Fact
                    newFactsToVisit =
                        deduceNewFacts newFact inferred.facts

                    deducedFromNewFact : Maybe ( Expression, DeducedValue )
                    deducedFromNewFact =
                        case newFact of
                            Equals a b ->
                                equalsFact a b

                            NotEquals a b ->
                                equalsFact a b
                                    |> Maybe.andThen notDeduced

                            Or _ _ ->
                                -- TODO Add "a || b || ..."?
                                Nothing
                in
                injectFacts
                    (newFactsToVisit ++ restOfFacts)
                    (Inferred
                        { facts = newFact :: inferred.facts
                        , deduced =
                            case deducedFromNewFact of
                                Just ( a, b ) ->
                                    AssocList.insert a b inferred.deduced

                                Nothing ->
                                    inferred.deduced
                        }
                    )


deduceNewFacts : Fact -> List Fact -> List Fact
deduceNewFacts newFact facts =
    case newFact of
        Equals factTarget factValue ->
            case expressionToDeduced factValue of
                Just value ->
                    List.concatMap (mergeEqualFacts ( factTarget, value )) facts

                Nothing ->
                    [ Equals factValue factTarget ]

        NotEquals _ _ ->
            []

        Or _ _ ->
            []


equalsFact : Expression -> Expression -> Maybe ( Expression, DeducedValue )
equalsFact a b =
    case expressionToDeduced a of
        Just deducedValue ->
            Just ( b, deducedValue )

        Nothing ->
            case expressionToDeduced b of
                Just deducedValue ->
                    Just ( a, deducedValue )

                Nothing ->
                    Nothing


expressionToDeduced : Expression -> Maybe DeducedValue
expressionToDeduced expression =
    case expression of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            Just DTrue

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            Just DFalse

        Expression.Floatable float ->
            Just (DNumber float)

        Expression.Literal string ->
            Just (DString string)

        _ ->
            Nothing


notDeduced : ( a, DeducedValue ) -> Maybe ( a, DeducedValue )
notDeduced ( a, deducedValue ) =
    case deducedValue of
        DTrue ->
            Just ( a, DFalse )

        DFalse ->
            Just ( a, DTrue )

        _ ->
            Nothing


mergeEqualFacts : ( Expression, DeducedValue ) -> Fact -> List Fact
mergeEqualFacts equalFact fact =
    case fact of
        Or left right ->
            List.filterMap (ifSatisfy equalFact)
                (List.map (\\cond -> ( cond, right )) left
                    ++ List.map (\\cond -> ( cond, left )) right
                )
                |> List.concat

        _ ->
            []


ifSatisfy : ( Expression, DeducedValue ) -> ( Fact, a ) -> Maybe a
ifSatisfy ( target, value ) ( targetFact, otherFact ) =
    case targetFact of
        Equals factTarget factValue ->
            if factTarget == target && areIncompatible value factValue then
                Just otherFact

            else
                Nothing

        NotEquals factTarget factValue ->
            if factTarget == target && areCompatible value factValue then
                Just otherFact

            else
                Nothing

        _ ->
            Nothing


areIncompatible : DeducedValue -> Expression -> Bool
areIncompatible value factValue =
    case ( value, factValue ) of
        ( DTrue, Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DFalse, Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DNumber valueFloat, Expression.Floatable factFloat ) ->
            valueFloat /= factFloat

        ( DString valueString, Expression.Literal factString ) ->
            valueString /= factString

        _ ->
            False


areCompatible : DeducedValue -> Expression -> Bool
areCompatible value factValue =
    case ( value, factValue ) of
        ( DTrue, Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DFalse, Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DNumber valueFloat, Expression.Floatable factFloat ) ->
            valueFloat == factFloat

        ( DString valueString, Expression.Literal factString ) ->
            valueString == factString

        _ ->
            False


inferOnEquality : Node Expression -> Node Expression -> Bool -> Inferred -> Inferred
inferOnEquality (Node _ expr) (Node _ other) shouldBe dict =
    case expr of
        Expression.Integer int ->
            if shouldBe then
                injectFacts
                    [ Equals other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

            else
                injectFacts
                    [ NotEquals other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

        Expression.Floatable float ->
            if shouldBe then
                injectFacts
                    [ Equals other (Expression.Floatable float) ]
                    dict

            else
                injectFacts
                    [ NotEquals other (Expression.Floatable float) ]
                    dict

        Expression.Literal str ->
            if shouldBe then
                injectFacts
                    [ Equals other (Expression.Literal str) ]
                    dict

            else
                injectFacts
                    [ NotEquals other (Expression.Literal str) ]
                    dict

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            injectFacts
                [ Equals other
                    (if shouldBe then
                        trueExpr

                     else
                        falseExpr
                    )
                ]
                dict

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            injectFacts
                [ Equals other
                    (if shouldBe then
                        falseExpr

                     else
                        trueExpr
                    )
                ]
                dict

        _ ->
            dict
"""
                )
            , Test.test "Simplify.AstHelpers"
                (\() ->
                    expectPrintedAsSame
                        """module Simplify.AstHelpers exposing
    ( subExpressions
    , removeParens, removeParensFromPattern
    , getValueOrFnOrFnCall
    , getSpecificFnCall, getSpecificValueOrFn
    , isIdentity, getAlwaysResult, isSpecificUnappliedBinaryOperation
    , isTupleFirstAccess, isTupleSecondAccess
    , getAccessingRecord, getRecordAccessFunction
    , getOrder, getSpecificBool, getBool, getBoolPattern, getUncomputedNumberValue
    , getCollapsedCons, getListLiteral, getListSingleton
    , getTuple2, getTuple2Literal
    , boolToString, orderToString, emptyStringAsString
    , moduleNameFromString, qualifiedName, qualifiedModuleName, qualifiedToString
    , declarationListBindings, letDeclarationListBindings, patternBindings, patternListBindings
    , nameOfExpose
    )

{-|


## look deeper

@docs subExpressions


### remove parens

@docs removeParens, removeParensFromPattern


### value/function/function call/composition

@docs getValueOrFnOrFnCall
@docs getSpecificFnCall, getSpecificValueOrFn


### certain kind

@docs isIdentity, getAlwaysResult, isSpecificUnappliedBinaryOperation
@docs isTupleFirstAccess, isTupleSecondAccess
@docs getAccessingRecord, getRecordAccessFunction
@docs getOrder, getSpecificBool, getBool, getBoolPattern, getUncomputedNumberValue
@docs getCollapsedCons, getListLiteral, getListSingleton
@docs getTuple2, getTuple2Literal


### literal as string

@docs boolToString, orderToString, emptyStringAsString


### qualification

@docs moduleNameFromString, qualifiedName, qualifiedModuleName, qualifiedToString


### misc

@docs declarationListBindings, letDeclarationListBindings, patternBindings, patternListBindings
@docs nameOfExpose

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Fn.Basics
import Fn.List
import Fn.Tuple
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Set exposing (Set)
import Simplify.Infer as Infer
import Simplify.Normalize as Normalize


{-| Keep removing parens from the outside until we have something different from a `ParenthesizedExpression`
-}
removeParens : Node Expression -> Node Expression
removeParens expressionNode =
    case Node.value expressionNode of
        Expression.ParenthesizedExpression expressionInsideOnePairOfParensNode ->
            removeParens expressionInsideOnePairOfParensNode

        _ ->
            expressionNode


{-| Keep removing parens from the outside until we have something different from a `ParenthesizedPattern`
-}
removeParensFromPattern : Node Pattern -> Node Pattern
removeParensFromPattern patternNode =
    case Node.value patternNode of
        Pattern.ParenthesizedPattern patternInsideOnePairOfParensNode ->
            removeParensFromPattern patternInsideOnePairOfParensNode

        _ ->
            patternNode


{-| Get all immediate child expressions of an expression
-}
subExpressions : Expression -> List (Node Expression)
subExpressions expression =
    case expression of
        Expression.LetExpression letBlock ->
            letBlock.expression
                :: (letBlock.declarations
                        |> List.map
                            (\\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Expression.LetFunction letFunction ->
                                        letFunction.declaration |> Node.value |> .expression

                                    Expression.LetDestructuring _ expression_ ->
                                        expression_
                            )
                   )

        Expression.ListExpr expressions ->
            expressions

        Expression.TupledExpression expressions ->
            expressions

        Expression.RecordExpr fields ->
            fields |> List.map (\\(Node _ ( _, value )) -> value)

        Expression.RecordUpdateExpression (Node recordVariableRange recordVariable) setters ->
            Node recordVariableRange (Expression.FunctionOrValue [] recordVariable)
                :: (setters |> List.map (\\(Node _ ( _, newValue )) -> newValue))

        Expression.RecordAccess recordToAccess _ ->
            [ recordToAccess ]

        Expression.Application applicationElements ->
            applicationElements

        Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\\( _, caseExpression ) -> caseExpression))

        Expression.OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        Expression.IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Expression.ParenthesizedExpression expressionInParens ->
            [ expressionInParens ]

        Expression.Negation expressionInNegation ->
            [ expressionInNegation ]

        Expression.UnitExpr ->
            []

        Expression.Integer _ ->
            []

        Expression.Hex _ ->
            []

        Expression.Floatable _ ->
            []

        Expression.Literal _ ->
            []

        Expression.CharLiteral _ ->
            []

        Expression.GLSLExpression _ ->
            []

        Expression.RecordAccessFunction _ ->
            []

        Expression.FunctionOrValue _ _ ->
            []

        Expression.Operator _ ->
            []

        Expression.PrefixOperator _ ->
            []


{-| Parse an expression of type list that contains only a single element.
Could be a call to `List.singleton` or a list literal with one element: `[ a ]`
-}
getListSingleton : ModuleNameLookupTable -> Node Expression -> Maybe { element : Node Expression }
getListSingleton lookupTable expressionNode =
    case getListLiteral expressionNode of
        Just (element :: []) ->
            Just { element = element }

        Just _ ->
            Nothing

        Nothing ->
            case getSpecificFnCall Fn.List.singleton lookupTable expressionNode of
                Just singletonCall ->
                    case singletonCall.argsAfterFirst of
                        [] ->
                            Just { element = singletonCall.firstArg }

                        _ :: _ ->
                            Nothing

                Nothing ->
                    Nothing


{-| Parses calls and lambdas that are reducible to a call of a function with the given name
-}
getSpecificFnCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            }
getSpecificFnCall ( moduleName, name ) lookupTable expressionNode =
    case getValueOrFnOrFnCall expressionNode of
        Just call ->
            case call.args of
                firstArg :: argsAfterFirst ->
                    if
                        (call.fnName /= name)
                            || (ModuleNameLookupTable.moduleNameAt lookupTable call.fnRange /= Just moduleName)
                    then
                        Nothing

                    else
                        Just
                            { nodeRange = call.nodeRange
                            , fnRange = call.fnRange
                            , firstArg = firstArg
                            , argsAfterFirst = argsAfterFirst
                            }

                [] ->
                    Nothing

        Nothing ->
            Nothing


{-| Parse a value or the collapsed function or a lambda fully reduced to a function
-}
getValueOrFnOrFnCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getValueOrFnOrFnCall expressionNode =
    case getCollapsedUnreducedValueOrFunctionCall expressionNode of
        Just valueOrCall ->
            Just valueOrCall

        Nothing ->
            case getReducedLambda expressionNode of
                Just reducedLambda ->
                    case ( reducedLambda.lambdaPatterns, reducedLambda.callArguments ) of
                        ( [], args ) ->
                            Just
                                { nodeRange = reducedLambda.nodeRange
                                , fnName = reducedLambda.fnName
                                , fnRange = reducedLambda.fnRange
                                , args = args
                                }

                        ( _ :: _, _ ) ->
                            Nothing

                Nothing ->
                    Nothing


{-| Parses either a value reference with the given name,
a function reference with the given name without arguments
or a lambda that is reducible to a function with the given name without arguments
-}
getSpecificValueOrFn : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificValueOrFn ( moduleName, name ) lookupTable expressionNode =
    case getValueOrFunction expressionNode of
        Just normalFn ->
            if
                (normalFn.name /= name)
                    || (ModuleNameLookupTable.moduleNameAt lookupTable normalFn.range /= Just moduleName)
            then
                Nothing

            else
                Just normalFn.range

        Nothing ->
            Nothing


{-| Parses either a value reference, a function reference without arguments or a lambda that is reducible to a function without arguments
-}
getValueOrFunction : Node Expression -> Maybe { name : String, range : Range }
getValueOrFunction expressionNode =
    case removeParens expressionNode of
        Node rangeInParens (Expression.FunctionOrValue _ foundName) ->
            Just { range = rangeInParens, name = foundName }

        nonFunctionOrValueNode ->
            case getReducedLambda nonFunctionOrValueNode of
                Just reducedLambdaToFn ->
                    case ( reducedLambdaToFn.lambdaPatterns, reducedLambdaToFn.callArguments ) of
                        ( [], [] ) ->
                            Just { range = reducedLambdaToFn.fnRange, name = reducedLambdaToFn.fnName }

                        ( _ :: _, _ ) ->
                            Nothing

                        ( _, _ :: _ ) ->
                            Nothing

                Nothing ->
                    Nothing


getCollapsedUnreducedValueOrFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getCollapsedUnreducedValueOrFunctionCall baseNode =
    let
        step :
            { firstArg : Node Expression, argsAfterFirst : List (Node Expression), fed : Node Expression }
            -> Maybe { nodeRange : Range, fnRange : Range, fnName : String, args : List (Node Expression) }
        step layer =
            Maybe.map
                (\\fed ->
                    { nodeRange = Node.range baseNode
                    , fnRange = fed.fnRange
                    , fnName = fed.fnName
                    , args = fed.args ++ (layer.firstArg :: layer.argsAfterFirst)
                    }
                )
                (getCollapsedUnreducedValueOrFunctionCall layer.fed)
    in
    case removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ fnName) ->
            Just
                { nodeRange = Node.range baseNode
                , fnRange = fnRange
                , fnName = fnName
                , args = []
                }

        Node _ (Expression.Application (fed :: firstArg :: argsAfterFirst)) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = argsAfterFirst
                }

        Node _ (Expression.OperatorApplication "|>" _ firstArg fed) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = []
                }

        Node _ (Expression.OperatorApplication "<|" _ fed firstArg) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = []
                }

        _ ->
            Nothing


{-| Whether it's a function that accesses a tuple's first part.
Either a function reducible to `Tuple.first` or `\\( first, ... ) -> first`.
-}
isTupleFirstAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleFirstAccess lookupTable expressionNode =
    case getSpecificValueOrFn Fn.Tuple.first lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleFirstPatternLambda expressionNode


isTupleFirstPatternLambda : Node Expression -> Bool
isTupleFirstPatternLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.TuplePattern [ Node _ (Pattern.VarPattern firstVariableName), _ ]) ] ->
                    Node.value lambda.expression == Expression.FunctionOrValue [] firstVariableName

                _ ->
                    False

        _ ->
            False


{-| Whether it's a function that accesses a tuple's second part.
Either a function reducible to `Tuple.second` or `\\( ..., second ) -> second`.
-}
isTupleSecondAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleSecondAccess lookupTable expressionNode =
    case getSpecificValueOrFn Fn.Tuple.second lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleSecondPatternLambda expressionNode


isTupleSecondPatternLambda : Node Expression -> Bool
isTupleSecondPatternLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.TuplePattern [ _, Node _ (Pattern.VarPattern firstVariableName) ]) ] ->
                    Node.value lambda.expression == Expression.FunctionOrValue [] firstVariableName

                _ ->
                    False

        _ ->
            False


{-| Parse a record access or call of a record access function.
The resulting `range` refers to the unparenthesized range of the access/function application.
-}
getAccessingRecord : Node Expression -> Maybe { range : Range, record : Node Expression, field : String }
getAccessingRecord expressionNode =
    case removeParens expressionNode of
        Node range (Expression.RecordAccess record (Node _ fieldName)) ->
            Just { field = fieldName, record = record, range = range }

        Node range (Expression.Application (function :: record :: [])) ->
            Maybe.map (\\fieldName -> { field = fieldName, record = record, range = range }) (getRecordAccessFunction function)

        Node range (Expression.OperatorApplication "|>" _ record function) ->
            Maybe.map (\\fieldName -> { field = fieldName, record = record, range = range }) (getRecordAccessFunction function)

        Node range (Expression.OperatorApplication "<|" _ function record) ->
            Maybe.map (\\fieldName -> { field = fieldName, record = record, range = range }) (getRecordAccessFunction function)

        _ ->
            Nothing


{-| Parse a function that accesses a specific field and is therefore equivalent to `.field`.
The resulting String is the field name without the leading dot.
-}
getRecordAccessFunction : Node Expression -> Maybe String
getRecordAccessFunction expressionNode =
    case expressionNode of
        Node _ (Expression.RecordAccessFunction fieldName) ->
            Just (String.replace "." "" fieldName)

        _ ->
            Nothing


getUncomputedNumberValue : Node Expression -> Maybe Float
getUncomputedNumberValue expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.Integer n ->
            Just (toFloat n)

        Expression.Hex n ->
            Just (toFloat n)

        Expression.Floatable n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getUncomputedNumberValue expr)

        _ ->
            Nothing


{-| Whether it's a function that returns any given input unchanged.
Either a function reducible to `Basics.identity` or `\\a -> a`.
-}
isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable baseExpressionNode =
    case getSpecificValueOrFn Fn.Basics.identity lookupTable baseExpressionNode of
        Just _ ->
            True

        Nothing ->
            case removeParens baseExpressionNode of
                Node _ (Expression.LambdaExpression lambda) ->
                    case lambda.args of
                        arg :: [] ->
                            variableMatchesPattern lambda.expression arg

                        _ ->
                            False

                _ ->
                    False


{-| Parse a function that returns the same for any given input and return the result expression node.
Either a function reducible to `Basics.always x`, `\\_ -> x` or even for example `\\_ a -> a x` where the result expression node would be `\\a -> a x`.
-}
getAlwaysResult : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
getAlwaysResult lookupTable expressionNode =
    case getSpecificFnCall Fn.Basics.always lookupTable expressionNode of
        Just alwaysCall ->
            Just alwaysCall.firstArg

        Nothing ->
            getIgnoreFirstLambdaResult expressionNode


getIgnoreFirstLambdaResult : Node Expression -> Maybe (Node Expression)
getIgnoreFirstLambdaResult expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                (Node _ Pattern.AllPattern) :: [] ->
                    Just lambda.expression

                (Node _ Pattern.AllPattern) :: pattern1 :: pattern2Up ->
                    Just
                        (Node (Node.range expressionNode)
                            (Expression.LambdaExpression
                                { args = pattern1 :: pattern2Up
                                , expression = lambda.expression
                                }
                            )
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


getReducedLambda :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , callArguments : List (Node Expression)
            , lambdaPatterns : List (Node Pattern)
            }
getReducedLambda expressionNode =
    -- maybe a version of this is better located in Normalize?
    case getCollapsedLambda expressionNode of
        Just lambda ->
            case getCollapsedUnreducedValueOrFunctionCall lambda.expression of
                Just call ->
                    let
                        ( reducedCallArguments, reducedLambdaPatterns ) =
                            drop2EndingsWhile
                                (\\( argument, pattern ) -> variableMatchesPattern argument pattern)
                                ( call.args
                                , lambda.patterns
                                )
                    in
                    Just
                        { nodeRange = Node.range expressionNode
                        , fnName = call.fnName
                        , fnRange = call.fnRange
                        , callArguments = reducedCallArguments
                        , lambdaPatterns = reducedLambdaPatterns
                        }

                Nothing ->
                    Nothing

        _ ->
            Nothing


variableMatchesPattern : Node Expression -> Node Pattern -> Bool
variableMatchesPattern expression pattern =
    case ( removeParensFromPattern pattern, removeParens expression ) of
        ( Node _ (Pattern.VarPattern patternName), Node _ (Expression.FunctionOrValue [] argumentName) ) ->
            patternName == argumentName

        _ ->
            False


{-| Remove elements at the end of both given lists, then repeat for the previous elements until a given test returns False
-}
drop2EndingsWhile : (( a, b ) -> Bool) -> ( List a, List b ) -> ( List a, List b )
drop2EndingsWhile shouldDrop ( aList, bList ) =
    let
        ( reducedArgumentsReverse, reducedPatternsReverse ) =
            drop2BeginningsWhile
                shouldDrop
                ( List.reverse aList
                , List.reverse bList
                )
    in
    ( List.reverse reducedArgumentsReverse, List.reverse reducedPatternsReverse )


drop2BeginningsWhile : (( a, b ) -> Bool) -> ( List a, List b ) -> ( List a, List b )
drop2BeginningsWhile shouldDrop listPair =
    case listPair of
        ( [], bList ) ->
            ( [], bList )

        ( aList, [] ) ->
            ( aList, [] )

        ( aHead :: aTail, bHead :: bTail ) ->
            if shouldDrop ( aHead, bHead ) then
                drop2BeginningsWhile shouldDrop ( aTail, bTail )

            else
                ( aHead :: aTail, bHead :: bTail )


getCollapsedLambda : Node Expression -> Maybe { patterns : List (Node Pattern), expression : Node Expression }
getCollapsedLambda expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case getCollapsedLambda lambda.expression of
                Nothing ->
                    Just
                        { patterns = lambda.args
                        , expression = lambda.expression
                        }

                Just innerCollapsedLambda ->
                    Just
                        { patterns = lambda.args ++ innerCollapsedLambda.patterns
                        , expression = innerCollapsedLambda.expression
                        }

        _ ->
            Nothing


patternListBindings : List (Node Pattern) -> Set String
patternListBindings patterns =
    List.foldl
        (\\(Node _ pattern) soFar -> Set.union soFar (patternBindings pattern))
        Set.empty
        patterns


{-| Recursively find all bindings in a pattern.
-}
patternBindings : Pattern -> Set String
patternBindings pattern =
    case pattern of
        Pattern.ListPattern patterns ->
            patternListBindings patterns

        Pattern.TuplePattern patterns ->
            patternListBindings patterns

        Pattern.RecordPattern patterns ->
            Set.fromList (List.map Node.value patterns)

        Pattern.NamedPattern _ patterns ->
            patternListBindings patterns

        Pattern.UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
            Set.union (patternBindings tailPattern) (patternBindings headPattern)

        Pattern.VarPattern name ->
            Set.singleton name

        Pattern.AsPattern (Node _ pattern_) (Node _ name) ->
            Set.insert name (patternBindings pattern_)

        Pattern.ParenthesizedPattern (Node _ inParens) ->
            patternBindings inParens

        Pattern.AllPattern ->
            Set.empty

        Pattern.UnitPattern ->
            Set.empty

        Pattern.CharPattern _ ->
            Set.empty

        Pattern.StringPattern _ ->
            Set.empty

        Pattern.IntPattern _ ->
            Set.empty

        Pattern.HexPattern _ ->
            Set.empty

        Pattern.FloatPattern _ ->
            Set.empty


declarationListBindings : List (Node Declaration) -> Set String
declarationListBindings declarationList =
    declarationList
        |> List.map (\\(Node _ declaration) -> declarationBindings declaration)
        |> List.foldl (\\bindings soFar -> Set.union soFar bindings) Set.empty


declarationBindings : Declaration -> Set String
declarationBindings declaration =
    case declaration of
        Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.map (\\(Node _ variant) -> Node.value variant.name)
                |> Set.fromList

        Declaration.FunctionDeclaration functionDeclaration ->
            Set.singleton
                (Node.value (Node.value functionDeclaration.declaration).name)

        _ ->
            Set.empty


letDeclarationBindings : Expression.LetDeclaration -> Set String
letDeclarationBindings letDeclaration =
    case letDeclaration of
        Expression.LetFunction fun ->
            Set.singleton
                (fun.declaration |> Node.value |> .name |> Node.value)

        Expression.LetDestructuring (Node _ pattern) _ ->
            patternBindings pattern


letDeclarationListBindings : List (Node Expression.LetDeclaration) -> Set String
letDeclarationListBindings letDeclarationList =
    letDeclarationList
        |> List.map
            (\\(Node _ declaration) -> letDeclarationBindings declaration)
        |> List.foldl (\\bindings soFar -> Set.union soFar bindings) Set.empty


getListLiteral : Node Expression -> Maybe (List (Node Expression))
getListLiteral expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.ListExpr list) ->
            Just list

        _ ->
            Nothing


getCollapsedCons : Node Expression -> Maybe { consed : List (Node Expression), tail : Node Expression }
getCollapsedCons expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.OperatorApplication "::" _ head tail ->
            let
                tailCollapsed : Maybe { consed : List (Node Expression), tail : Node Expression }
                tailCollapsed =
                    getCollapsedCons tail
            in
            case tailCollapsed of
                Nothing ->
                    Just { consed = [ head ], tail = tail }

                Just tailCollapsedList ->
                    Just { consed = head :: tailCollapsedList.consed, tail = tailCollapsedList.tail }

        _ ->
            Nothing


getBool : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBool lookupTable expressionNode =
    case getSpecificBool True lookupTable expressionNode of
        Just _ ->
            Just True

        Nothing ->
            case getSpecificBool False lookupTable expressionNode of
                Just _ ->
                    Just False

                Nothing ->
                    Nothing


getSpecificBool : Bool -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificBool specificBool lookupTable expressionNode =
    getSpecificValueOrFn ( [ "Basics" ], boolToString specificBool ) lookupTable expressionNode


getTuple2Literal : Node Expression -> Maybe { range : Range, first : Node Expression, second : Node Expression }
getTuple2Literal expressionNode =
    case Node.value expressionNode of
        Expression.TupledExpression (first :: second :: []) ->
            Just { range = Node.range expressionNode, first = first, second = second }

        _ ->
            Nothing


getTuple2 : ModuleNameLookupTable -> Node Expression -> Maybe { first : Node Expression, second : Node Expression }
getTuple2 lookupTable expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.TupledExpression (first :: second :: [])) ->
            Just { first = first, second = second }

        _ ->
            case getSpecificFnCall Fn.Tuple.pair lookupTable expressionNode of
                Just tuplePairCall ->
                    case tuplePairCall.argsAfterFirst of
                        second :: _ ->
                            Just { first = tuplePairCall.firstArg, second = second }

                        [] ->
                            Nothing

                Nothing ->
                    Nothing


getBoolPattern : ModuleNameLookupTable -> Node Pattern -> Maybe Bool
getBoolPattern lookupTable basePatternNode =
    case removeParensFromPattern basePatternNode of
        Node variantPatternRange (Pattern.NamedPattern variantPattern _) ->
            case variantPattern.name of
                "True" ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable variantPatternRange of
                        Just [ "Basics" ] ->
                            Just True

                        _ ->
                            Nothing

                "False" ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable variantPatternRange of
                        Just [ "Basics" ] ->
                            Just False

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getSpecificOrder : Order -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificOrder specificOrder lookupTable expression =
    getSpecificValueOrFn ( [ "Basics" ], orderToString specificOrder ) lookupTable expression


getOrder : ModuleNameLookupTable -> Node Expression -> Maybe Order
getOrder lookupTable expression =
    case getSpecificOrder LT lookupTable expression of
        Just _ ->
            Just LT

        Nothing ->
            case getSpecificOrder EQ lookupTable expression of
                Just _ ->
                    Just EQ

                Nothing ->
                    case getSpecificOrder GT lookupTable expression of
                        Just _ ->
                            Just GT

                        Nothing ->
                            Nothing


{-| Whether a given expression can be called with 2 operands and produces the same result as an operation with a given operator.
Is either a function reducible to the operator in prefix notation `(op)` or a lambda `\\a b -> a op b`.
-}
isSpecificUnappliedBinaryOperation : String -> Infer.Resources a -> Node Expression -> Bool
isSpecificUnappliedBinaryOperation symbol checkInfo expression =
    case expression |> Normalize.normalize checkInfo |> Node.value of
        Expression.PrefixOperator operatorSymbol ->
            operatorSymbol == symbol

        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.VarPattern element) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] argument) ] ->
                            (operatorSymbol == symbol)
                                && (argument == element)

                        -- no simple application
                        _ ->
                            False

                [ Node _ (Pattern.VarPattern element), Node _ (Pattern.VarPattern soFar) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] left), Node _ (Expression.FunctionOrValue [] right) ] ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        Expression.OperatorApplication operatorSymbol _ (Node _ (Expression.FunctionOrValue [] left)) (Node _ (Expression.FunctionOrValue [] right)) ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        _ ->
                            False

                _ ->
                    False

        -- not a known simple operator function
        _ ->
            False


nameOfExpose : Exposing.TopLevelExpose -> String
nameOfExpose topLevelExpose =
    case topLevelExpose of
        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.InfixExpose name ->
            name

        Exposing.TypeExpose typeExpose ->
            typeExpose.name
"""
                )
            ]
        ]


expectPrintedAs : String -> String -> Expect.Expectation
expectPrintedAs expected source =
    case source |> Elm.Parser.parseToFile of
        Err deadEnds ->
            Expect.fail ("failed to parse actual source: " ++ (deadEnds |> Debug.toString))

        Ok parsed ->
            let
                printed : String
                printed =
                    parsed
                        |> ElmSyntaxFormat.module_
                        |> Print.toString
            in
            if printed == expected then
                Expect.pass

            else
                Expect.fail
                    ("actual printed source is\n\n"
                        ++ printed
                        ++ "\n\nbut I expected\n\n"
                        ++ expected
                        ++ "\n\nThey differ in lines "
                        ++ (List.map2
                                (\actualLine expectedLine -> { actual = actualLine, expected = expectedLine })
                                (printed |> String.lines)
                                (expected |> String.lines)
                                |> List.indexedMap
                                    (\i lines ->
                                        if lines.actual == lines.expected then
                                            Nothing

                                        else
                                            Just (i |> String.fromInt)
                                    )
                                |> List.filterMap identity
                                |> String.join " "
                           )
                    )


expectPrintedAsSame : String -> Expect.Expectation
expectPrintedAsSame alreadyFormattedSource =
    alreadyFormattedSource |> expectPrintedAs alreadyFormattedSource
