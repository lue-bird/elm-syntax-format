module Tests exposing (suite)

import Elm.Parser
import ElmSyntaxFormat
import Expect
import Print
import Test exposing (Test)


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
            , Test.test "comments between parameters"
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
            ]
        , Test.describe "expression"
            [ Test.test "if-then-else with another if-then-else in the else branch"
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
            ]
        ]


expectPrintedAs : String -> String -> Expect.Expectation
expectPrintedAs expected actual =
    case actual |> Elm.Parser.parseToFile of
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
                    )
