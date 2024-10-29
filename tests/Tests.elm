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
                        -- copied from https://github.com/pdamoc/elm-syntax-sscce
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
                        -- adding the next line at the end of a declaration with result in it being logged to the JS console
                        |> Debug.log "newValue"
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

        -- Shows how to avoid parentheses by using the backwards pipe operator
        , div [] [ text <| toString model.count ]

        -- Shows how to used a function from a module without having to expose it in the import section.
        , div [] [ button [ Events.onClick Decrement ] [ text "-1" ] ]
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
|]"""
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
