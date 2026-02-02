module ElmSyntaxPrintTests exposing (suite)

import Elm.Parser
import ElmSyntaxParserLenient
import ElmSyntaxPrint
import Expect
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
            , Test.test "exposing multiple, invalid @docs, empty @docs, multiline"
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
            , Test.test "before comments"
                (\() ->
                    """module A exposing (..)
{-| A module about A.
-}
--
a =
    "a\""""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| A module about A.
-}

--


a =
    "a"
"""
                )
            , Test.test "closing -} not on a new line"
                (\() ->
                    """module A exposing (..)
{-| A module about A. -}
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
            , Test.test "consecutive collapsible comments before parameter"
                (\() ->
                    """module A exposing (..)
type alias A {- 0 -}
    {- 1 -} parameterA parameterB =
    (parameterA,parameterB)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A {- 0 -} {- 1 -} parameterA parameterB =
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
            , Test.test "consecutive collapsible comments before parameter"
                (\() ->
                    """module A exposing (..)
type A {- 0 -}
     {- 1 -} parameterA parameterB
    = A (parameterA,parameterB)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A {- 0 -} {- 1 -} parameterA parameterB
    = A ( parameterA, parameterB )
"""
                )
            , Test.test "consecutive collapsible comments between last parameter and single-line first variant"
                (\() ->
                    """module A exposing (..)
type A parameter {- 0 -}
    = {- 1 -} A parameter"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A parameter
    = {- 0 -} {- 1 -} A parameter
"""
                )
            , Test.test "consecutive collapsible comments between name and single-line first variant"
                (\() ->
                    """module A exposing (..)
type A {- 0 -}
    = {- 1 -} A Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = {- 0 -} {- 1 -} A Int
"""
                )
            , Test.test "comments between name and first variant"
                (\() ->
                    """module A exposing (..)
type A -- 0
    = {- 1 -} A Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = -- 0
      {- 1 -}
      A Int
"""
                )
            , Test.test "consecutive comments before non-first single-line variant"
                (\() ->
                    """module A exposing (..)
type A
    = A Int -- 0
    | {- 1 -} B String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int
    | -- 0
      {- 1 -}
      B String
"""
                )
            , Test.test "consecutive collapsible comments before non-first single-line variant"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -}
    | {- 1 -} B String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int
    | {- 0 -} {- 1 -} B String
"""
                )
            , Test.test "consecutive collapsible comments before non-first multi-line variant"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -}
    | {- 1 -} B {--} String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int
    | {- 0 -} {- 1 -}
      B
        {--}
        String
"""
                )
            , Test.test "comments before first variant parameter"
                (\() ->
                    """module A exposing (..)
type A
    = A {--} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A
        {--}
        Int
"""
                )
            , Test.test "consecutive collapsible comments before first variant parameter"
                (\() ->
                    """module A exposing (..)
type A
    = A {- 0 -} {- 1 -}
        Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A {- 0 -} {- 1 -} Int
"""
                )
            , Test.test "comments between variant parameters"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {--} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A
        Int
        {--}
        Int
"""
                )
            , Test.test "consecutive collapsible comments between variant parameters"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -} {- 1 -} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int {- 0 -} {- 1 -} Int
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
            , Test.test "documentation comments have the closing -} on a new line"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| :blushes: -}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


{-| :blushes:
-}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "documentation comments have the closing -} on a new line on w*ndows"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| :blushes: -}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> String.replace "\n" "\u{000D}\n"
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


{-| :blushes:
-}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "documentation comments don't have a linebreak before the closing -} when their content contains no blank lines"
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
            , Test.test "documentation comments have exactly one blank line + linebreak before the closing -} when their content contains blank lines"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| :blushes:

hi :3
-}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


{-| :blushes:

hi :3

-}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "documentation comments have exactly one blank line + linebreak before the closing -} when their content contains lines with only whitespace, and comments are stripped of ending whitespace"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| :blushes:
\t
hi :3\t
-}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


{-| :blushes:

hi :3

-}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "documentation comments consisting of whitespace only are printed as {-| -}"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| 


\t   
 \t 
-}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


{-| -}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "type on next should be single-line"
                (\() ->
                    """port module A exposing (..)
port sendMessage :
    String -> Cmd msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg
"""
                )
            ]
        , Test.describe "declaration infix"
            [ Test.test "from Basics"
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
            , Test.test "value, comment between signature and implementation name"
                (\() ->
                    """module A exposing (..)
a : Int {- 0 -}
a =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a : Int



{- 0 -}


a =
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
            , Test.test "function input function is parenthesized even in trailing argument"
                (\() ->
                    """module A exposing (..)
type alias T a =
    Int -> (((Int -> Int))) -> a"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a =
    Int -> (Int -> Int) -> a
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
            , Test.test "function consecutive collapsible comments between types"
                (\() ->
                    """module A exposing (..)
type alias T =
    Int -> {- 0 -} {- 1 -}
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T =
    Int
    -> {- 0 -} {- 1 -} Int
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
            , Test.test "consecutive collapsible comments before first record field"
                (\() ->
                    """module A exposing (..)
type alias A = { {- 0 -}
    {- 1 -}
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { {- 0 -} {- 1 -} zero : Int
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
            , Test.test "consecutive collapsible comments between record field name and single-line value on next line"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : {- 0 -} {- 1 -}
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero :
        {- 0 -} {- 1 -} Int
    }
"""
                )
            , Test.test "consecutive collapsible comments between record field name and multi-line value on same line"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : {- 0 -} {- 1 -} List
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero :
        {- 0 -} {- 1 -}
        List
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
            , Test.test "comments collapsible before record extension record variable"
                (\() ->
                    """module A exposing (..)
type alias A r = { {- zero -}
    r | zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { {- zero -} r
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
            , Test.test "comments collapsible before first record extension field"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | {- zero -}
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | {- zero -} zero : Int
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
            , Test.test "comments between record extension field name and value not on the same line"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : {- zero -}
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero :
            {- zero -} Int
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
            , Test.test "consecutive comments between record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, -- zero
 -- one
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero : Int
        , -- zero
          -- one
          one : Int
    }
"""
                )
            , Test.test "comments collapsed between record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, {- one -}
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero : Int
        , {- one -} one : Int
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
            , Test.test "single-line construct"
                (\() ->
                    """module A exposing (..)
type alias A = List Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List Int
"""
                )
            , Test.test "multi-line construct"
                (\() ->
                    """module A exposing (..)
type alias A = List
               Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List
        Int
"""
                )
            , Test.test "construct written in single line with consecutive comments before argument"
                (\() ->
                    """module A exposing (..)
type alias A = List {--}{--} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List
        {--}
        {--}
        Int
"""
                )
            , Test.test "single-line construct with comments collapsible before argument"
                (\() ->
                    """module A exposing (..)
type alias A = List {- 0 -} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List {- 0 -} Int
"""
                )
            , Test.test "construct with consecutive comments collapsible before multi-line argument"
                (\() ->
                    """module A exposing (..)
type alias A = List {- 0 -}{- 1 -} (List
                             Int)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List
        {- 0 -} {- 1 -}
        (List
            Int
        )
"""
                )
            , Test.test "multi-line construct with consecutive comments collapsible before single-line argument"
                (\() ->
                    """module A exposing (..)
type alias A = Result {- 0 -}{- 1 -} Int (List
                                          Int)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    Result
        {- 0 -} {- 1 -} Int
        (List
            Int
        )
"""
                )
            ]
        , Test.test "multi-line tuple but single line range"
            (\() ->
                """module A exposing (..)
type alias A = ( Int, List {--}() )"""
                    |> expectPrintedAs
                        """module A exposing (..)


type alias A =
    ( Int
    , List
        {--}
        ()
    )
"""
            )
        , Test.test "multi-line triple but single line range"
            (\() ->
                """module A exposing (..)
type alias A = ( Int, List {--}(), Char )"""
                    |> expectPrintedAs
                        """module A exposing (..)


type alias A =
    ( Int
    , List
        {--}
        ()
    , Char
    )
"""
            )
        , Test.describe "expression"
            [ Test.test "negate 0"
                (\() ->
                    """module A exposing (..)
a = -0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0
"""
                )
            , Test.test "negate 0, parenthesized"
                (\() ->
                    """module A exposing (..)
a = -((((((0))))))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0
"""
                )
            , Test.test "negate 0 multiple times, parenthesized"
                (\() ->
                    """module A exposing (..)
a = -(((-(((-0))))))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0
"""
                )
            , Test.test "negate 0x0"
                (\() ->
                    """module A exposing (..)
a = -0x0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0x00
"""
                )
            , Test.test "negate multi-line"
                (\() ->
                    """module A exposing (..)
a = -(negate
    0)"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    -(negate
        0
     )
"""
                )
            , Test.test "doubly negated not literal-zero"
                (\() ->
                    """module A exposing (..)
a = -(-1)"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    -(-1)
"""
                )
            , Test.test "doubly negated not literal-zero, parenthesized"
                (\() ->
                    """module A exposing (..)
a = -((-((1))))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    -(-1)
"""
                )
            , Test.test "if-then-else with single-line condition"
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
            , Test.test "lambda, consecutive {- -} comments before result do not get collapsed"
                (\() ->
                    """module A exposing (..)
a =
    \\b -> {- 0 -} {- 1 -} b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\b ->
        {- 0 -}
        {- 1 -}
        b
"""
                )
            , Test.test "lambda, multi-line result in single line"
                (\() ->
                    """module A exposing (..)
a =
    \\b -> if True then 0 else b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\b ->
        if True then
            0

        else
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
            , Test.test "single-line lambda, consecutive collapsible comments before single-line parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\ {- 0 -} {- 1 -}
    b -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\{- 0 -} {- 1 -} b ->
        b
"""
                )
            , Test.test "multi-line lambda, consecutive collapsible comments before single-line parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\{- 0 -} {- 1 -} b (c{--}) -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\
     {- 0 -} {- 1 -} b
     (c
      {--}
     )
    ->
        b
"""
                )
            , Test.test "lambda, consecutive collapsible comments before multi-line parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\{- 0 -} {- 1 -} (b{--}) -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\
     {- 0 -} {- 1 -}
     (b
      {--}
     )
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
            , Test.test "let-in with one value declaration with type, comment between signature and implementation name"
                (\() ->
                    """module A exposing (..)
a =
    let
        b : Int {- 0 -}
        b =
            0
    in
    b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b : Int
        {- 0 -}
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
            , Test.test "call, applied function is multi-line"
                (\() ->
                    """module A exposing (..)
a =
    (identity {--}) identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (identity
     {--}
    )
        identity
"""
                )
            , Test.test "call, multi-line comments before first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {--} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        {--}
        identity
"""
                )
            , Test.test "single-line call, consecutive collapsible comments before first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "multi-line call, consecutive collapsible comments before first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} (identity
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "call, consecutive collapsible comments before multi-line first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} (identity{--})"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        {- 0 -} {- 1 -}
        (identity
         {--}
        )
"""
                )
            , Test.test "call, consecutive collapsible comments before first single-line argument, multi-line follow-up argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} identity
        identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity {- 0 -} {- 1 -} identity
        identity
"""
                )
            , Test.test "single-line call, consecutive collapsible comments before last argument"
                (\() ->
                    """module A exposing (..)
a =
    identity identity {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity identity {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "call, consecutive collapsible comments before last multi-line argument"
                (\() ->
                    """module A exposing (..)
a =
    identity identity {- 0 -} {- 1 -} (identity{--})"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity identity
        {- 0 -} {- 1 -}
        (identity
         {--}
        )
"""
                )
            , Test.test "multi-line call, consecutive collapsible comments before last argument"
                (\() ->
                    """module A exposing (..)
a =
    identity identity {- 0 -} {- 1 -} (identity
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity identity
        {- 0 -} {- 1 -} identity
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
            , Test.test "++ pipeline with multi-line list"
                (\() ->
                    """module A exposing (..)
a =
    [] ++ [ 0
    ] ++ []"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    []
        ++ [ 0
           ]
        ++ []
"""
                )
            , Test.test "++ with left being multi-line but all only covering one line"
                (\() ->
                    """module A exposing (..)
a =
    (if True then [] else []) ++ []"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (if True then
        []

     else
        []
    )
        ++ []
"""
                )
            , Test.test "++ with middle being multi-line but all only covering one line"
                (\() ->
                    """module A exposing (..)
a =
    [] ++ (if True then [] else []) ++ []"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    []
        ++ (if True then
                []

            else
                []
           )
        ++ []
"""
                )
            , Test.test "++ with distant middle being multi-line but all only covering one line"
                (\() ->
                    """module A exposing (..)
a =
    [] ++ (if True then [] else []) ++ [] ++ []"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    []
        ++ (if True then
                []

            else
                []
           )
        ++ []
        ++ []
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
    identity |> identity identity |> {- 0 -} {--} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> identity identity
        |> {- 0 -}
           {--}
           identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments collapsible before single-line rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "|> pipeline, with consecutive comments collapsible before multi-line rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> identity identity
        |> {- 0 -} {- 1 -}
           identity
            identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments before not rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> {- 0 -} {--} identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> {- 0 -}
           {--}
           identity identity
        |> identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive collapsible comments before not rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> {- 0 -} {- 1 -} identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity |> {- 0 -} {- 1 -} identity identity |> identity
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
            , Test.test "<| pipeline, multi-line, consecutive comments collapsible before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| {- 0 -}
    {- 1 -}
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        identity identity <|
            {- 0 -} {- 1 -} identity
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
            , Test.test "<| pipeline, multi-line, consecutive comments collapsible before non-rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| {- 0 -}
    {- 1 -}
    identity identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        {- 0 -} {- 1 -} identity identity <|
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
            , Test.test "empty list with consecutive comments"
                (\() ->
                    """module A exposing (..)
a = [ {--}{- 0 -} ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [{--}
     {- 0 -}
    ]
"""
                )
            , Test.test "empty list with consecutive comments collapsible"
                (\() ->
                    """module A exposing (..)
a = [ {- 0 -}{- 1 -} ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [{- 0 -} {- 1 -}]
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
            , Test.test "consecutive comments collapsible before single-line list element"
                (\() ->
                    """module A exposing (..)
a = [ 0, {- 0 -}
    {- 1 -}
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , {- 0 -} {- 1 -} 0
    ]
"""
                )
            , Test.test "consecutive comments collapsible before multi-line list element"
                (\() ->
                    """module A exposing (..)
a = [ 0, {- 0 -}
    {- 1 -}
    (0{--}) ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , {- 0 -} {- 1 -}
      (0
       {--}
      )
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
            , Test.test "empty record with consecutive comments"
                (\() ->
                    """module A exposing (..)
a = { -- 0
      -- 1
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    {-- 0
     -- 1
    }
"""
                )
            , Test.test "empty record with consecutive comments collapsible"
                (\() ->
                    """module A exposing (..)
a = { {- 0 -}
      {- 1 -}
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    {{- 0 -} {- 1 -}}
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
            , Test.test "single-line record, comments collapsible before first field"
                (\() ->
                    """module A exposing (..)
a = {{- zero -} zero = 0}"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { {- zero -} zero = 0 }
"""
                )
            , Test.test "multi-line record, comments collapsible before first field"
                (\() ->
                    """module A exposing (..)
a = { {- zero -}
    zero = 0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { {- zero -} zero = 0
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
            , Test.test "multi-line record, comments between field name and value not on same line"
                (\() ->
                    """module A exposing (..)
a = { zero = {- zero -}
    0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero =
        {- zero -} 0
    }
"""
                )
            , Test.test "multi-line record, comments between field name and multi-line value"
                (\() ->
                    """module A exposing (..)
a = { zero = {- zero -} identity
    0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero =
        {- zero -}
        identity
            0
    }
"""
                )
            , Test.test "multi-line record, comments between field name and value on same line"
                (\() ->
                    """module A exposing (..)
a = { zero = {- zero -} 0
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero = {- zero -} 0
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
            , Test.test "multi-line record, comments collapsible between fields"
                (\() ->
                    """module A exposing (..)
a = { zero = 0, {- zero -}
    one = 1 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero = 0
    , {- zero -} one = 1
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
            , Test.test "single-line tuple"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1 )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0, 1 )
"""
                )
            , Test.test "multi-line tuple"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
    )
"""
                )
            , Test.test "multi-line tuple not by range"
                (\() ->
                    """module A exposing (..)
a = ( 0, let () = () in 1 )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , let
        () =
            ()
      in
      1
    )
"""
                )
            , Test.test "nested multi-line tuple"
                (\() ->
                    """module A exposing (..)
a = ( 0, ( 1, 2
    )    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , ( 1
      , 2
      )
    )
"""
                )
            , Test.test "tuple single-line with comments collapsed before parts"
                (\() ->
                    """module A exposing (..)
a = ( {- 0 -} 0, {- 1 -} 1 )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( {- 0 -} 0, {- 1 -} 1 )
"""
                )
            , Test.test "multi-line tuple with comments collapsed before parts"
                (\() ->
                    """module A exposing (..)
a = ( {- 0 -} 0, {- 1 -} 1
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( {- 0 -} 0
    , {- 1 -} 1
    )
"""
                )
            , Test.test "nested multi-line tuple with consecutive comments collapsed before multi-line part"
                (\() ->
                    """module A exposing (..)
a = ( 0, {- 1 -} {- 2 -} ( 1, 2
    )    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , {- 1 -} {- 2 -}
      ( 1
      , 2
      )
    )
"""
                )
            , Test.test "nested multi-line tuple with consecutive comments before multi-line part"
                (\() ->
                    """module A exposing (..)
a = ( 0, {--} {--} ( 1, 2
    )    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , {--}
      {--}
      ( 1
      , 2
      )
    )
"""
                )
            , Test.test "tuple with consecutive comments after last part"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1 {--} {--} )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
      {--}
      {--}
    )
"""
                )
            , Test.test "single-line triple"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1, 2 )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0, 1, 2 )
"""
                )
            , Test.test "multi-line triple"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1, 2
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
    , 2
    )
"""
                )
            , Test.test "multi-line triple not by range"
                (\() ->
                    """module A exposing (..)
a = ( 0, let () = () in 1, 2 )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , let
        () =
            ()
      in
      1
    , 2
    )
"""
                )
            , Test.test "nested multi-line triple"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1, ( 2, 3, 4
    )       )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
    , ( 2
      , 3
      , 4
      )
    )
"""
                )
            , Test.test "triple single-line with comments collapsed before parts"
                (\() ->
                    """module A exposing (..)
a = ( {- 0 -} 0, {- 1 -} 1, {- 2 -} 2 )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( {- 0 -} 0, {- 1 -} 1, {- 2 -} 2 )
"""
                )
            , Test.test "multi-line triple with comments collapsed before parts"
                (\() ->
                    """module A exposing (..)
a = ( {- 0 -} 0, {- 1 -} 1, {- 2 -} 2
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( {- 0 -} 0
    , {- 1 -} 1
    , {- 2 -} 2
    )
"""
                )
            , Test.test "nested multi-line triple with consecutive comments collapsed before multi-line part"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1, {- 2 -} {- 3 -} {- 4 -} ( 2, 3, 4
    )       )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
    , {- 2 -} {- 3 -} {- 4 -}
      ( 2
      , 3
      , 4
      )
    )
"""
                )
            , Test.test "nested multi-line triple with consecutive comments before multi-line part"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1, {--} {--} ( 2, 3, 4
    )       )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
    , {--}
      {--}
      ( 2
      , 3
      , 4
      )
    )
"""
                )
            , Test.test "triple with consecutive comments after last part"
                (\() ->
                    """module A exposing (..)
a = ( 0, 1, 2 {--} {--} )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    ( 0
    , 1
    , 2
      {--}
      {--}
    )
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
            , Test.test "char emoji (multiple codes, SymbolOther)"
                (\() ->
                    """module A exposing (..)
a = '🟩' """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    '🟩'
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
            , Test.test "single double quote string with escaped backslash followed by n"
                (\() ->
                    """module A exposing (..)
a = "\\\\n" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "\\\\n"
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
            , Test.test "single double quote string with emoji (multiple codes, SymbolOther)"
                (\() ->
                    """module A exposing (..)
a = "🟩" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "🟩"
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
            , Test.test "triple double quote string single-line with emoji (multiple codes, SymbolOther)"
                (\() ->
                    """module A exposing (..)
a = \"\"\"🟩\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"🟩\"\"\"
"""
                )
            , Test.test "triple double quote string with unicode escape ansi hide cursor"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\u{1B}[?25l\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\u{001B}[?25l\"\"\"
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
            , Test.test "triple double quote string does not escape double quote as first char if not followed by single double quote"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote as first and second char if not followed by single double quote"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\""normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\"\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as first and second and third char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\"\"\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\"\\"\\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as first and second and third and fourth char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\"\"\"\\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\"\\"\\"\\"normal text\"\"\"
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
            , Test.test "triple double quote string escapes double quote as second-last and last char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text"\\\"\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal text\\"\\\"\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote as second-last before last char not double quote"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text".\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal text".\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal " text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal " text\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape 2 consecutive double quotes not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal "" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal "" text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes 3 consecutive double quotes not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal \\""\\" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal \\"\\"\\" text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes 4 consecutive double quotes not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal \\""\\"" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal \\"\\"\\"\\" text\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by n"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\n\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\n\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by r"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\r\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\r\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by u{000D}"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\u{000D}\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\u{000D}\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by u{1234}"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\u{1234}\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\u{1234}\"\"\"
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
            , Test.test "parenthesized with consecutive collapsible comments only before single-line"
                (\() ->
                    """module A exposing (..)
a ({-0-}{-1-}
    _ as argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a ({- 0 -} {- 1 -} _ as argument) =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments only after single-line"
                (\() ->
                    """module A exposing (..)
a (_ as argument
   {-0-}{-1-}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a (_ as argument {- 0 -} {- 1 -}) =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments before and after single-line"
                (\() ->
                    """module A exposing (..)
a ({-0-}{-1-}
   _ as argument
   {-2-}{-3-}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a ({- 0 -} {- 1 -} _ as argument {- 2 -} {- 3 -}) =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments before and after multi-line"
                (\() ->
                    """module A exposing (..)
a ({-0-}{-1-}
   _ as -- line breaker
   argument
   {-2-}{-3-}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ({- 0 -} {- 1 -}
     _
     as
        -- line breaker
        argument
     {- 2 -} {- 3 -}
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
            , Test.test "single-line as with comments collapsible before name"
                (\() ->
                    """module A exposing (..)
a (_ as {- argument -}
  argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a (_ as {- argument -} argument) =
    0
"""
                )
            , Test.test "tuple written as multi-line with comments collapsed before parts"
                (\() ->
                    """module A exposing (..)
a ( {- 0 -} a0, {- 1 -} a1
  ) = a0 + a1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a ( {- 0 -} a0, {- 1 -} a1 ) =
    a0 + a1
"""
                )
            , Test.test "triple written as multi-line with comments collapsed before parts"
                (\() ->
                    """module A exposing (..)
a ( {- 0 -} a0, {- 1 -} a1, {- 2 -} a2
  ) = a0 + a1 + a2"""
                        |> expectPrintedAs
                            """module A exposing (..)


a ( {- 0 -} a0, {- 1 -} a1, {- 2 -} a2 ) =
    a0 + a1 + a2
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
            , Test.test "multi-line as with comments collapsible before name"
                (\() ->
                    """module A exposing (..)
a ([-- in list
    _ 
   ] as {- argument -}
  argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ([ -- in list
       _
     ]
     as
        {- argument -} argument
    )
    =
    0
"""
                )
            , Test.test "empty record with consecutive comments"
                (\() ->
                    """module A exposing (..)
a { -- 0
  -- 1
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    {-- 0
     -- 1
    }
    =
    0
"""
                )
            , Test.test "empty record with consecutive collapsible comments"
                (\() ->
                    """module A exposing (..)
a { {- 0 -}
  {- 1 -}
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a {{- 0 -} {- 1 -}} =
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
            , Test.test "record with consecutive collapsible comments before first field"
                (\() ->
                    """module A exposing (..)
a { {- 0 -}
  {- 1 -}
  zero } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a { {- 0 -} {- 1 -} zero } =
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
            , Test.test "record with collapsible comments between fields"
                (\() ->
                    """module A exposing (..)
a { zero, {- 0 -}
  {- 1 -}
  one } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a { zero, {- 0 -} {- 1 -} one } =
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
            , Test.test "consecutive comments in empty list"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ -- 0
         -- 1
         ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [-- 0
         -- 1
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive collapsible comments in empty list"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ {- 0 -}
         {- 1 -}
         ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [{- 0 -} {- 1 -}] ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments before first list element"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ -- 0
         -- 1
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ -- 0
          -- 1
          0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "comments between list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, -- zero
          0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ 0
        , -- zero
          0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments between list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, -- 0
         -- 1
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ 0
        , -- 0
          -- 1
          0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments collapsible before single-line list element"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, {- 0 -}
         {- 1 -}
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ 0, {- 0 -} {- 1 -} 0 ] ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments collapsible before single-line list element among multi-line list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ {--} 0, {- 0 -}
         {- 1 -}
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ {--}
          0
        , {- 0 -} {- 1 -} 0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments collapsible before multi-line list element"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, {- 0 -} {- 1 -} (0{--}) ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ 0
        , {- 0 -} {- 1 -}
          (0
           {--}
          )
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "comments after list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, 0
          -- zero
         ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ 0
        , 0
          -- zero
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments after list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, 0
          -- 0
          -- 1
          ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        [ 0
        , 0
          -- 0
          -- 1
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments before rightest tail pattern"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        b :: Just c :: {- 0 -} {--} d ->
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
               {--}
               d
        ->
            b

        _ ->
            0
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments collapsible before single-line tail pattern"
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
        b :: (Just c) :: {- 0 -} {- 1 -} d ->
            b

        _ ->
            0
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments collapsible before multi-line tail pattern"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (d --
         ) ->
            b

        _ ->
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    case [] of
        b
            :: (Just c)
            :: {- 0 -} {- 1 -}
               (d
                --
               )
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
            [ Test.test "example from readme"
                (\() ->
                    """
module   Sample  exposing(...)
plus2 (n)= {- this adds 2-} n
+
2
"""
                        |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
                        |> Maybe.map
                            (\syntaxModule ->
                                syntaxModule
                                    |> ElmSyntaxPrint.module_
                                    |> ElmSyntaxPrint.toString
                            )
                        |> Expect.equal
                            (Just
                                """module Sample exposing (..)


plus2 n =
    {- this adds 2 -}
    n
        + 2
"""
                            )
                )
            , Test.test "elm-syntax-sscce"
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
            , -- below examples are from elm-review-simplify whose license is
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
              Test.test "Simplify.Evaluate"
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

{-| ## look deeper

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
            , Test.test "Simplify"
                (\() ->
                    expectPrintedAsSame
                        """module Simplify exposing
    ( rule
    , Configuration, defaults, expectNaN, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Simplify.rule Simplify.defaults
        ]

@docs rule
@docs Configuration, defaults, expectNaN, ignoreCaseOfForTypes


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules Simplify
```


## Simplifications

Below is the list of all kinds of simplifications this rule applies.


### Booleans

    x || True
    --> True

    x || False
    --> x

    x && True
    --> x

    x && False
    --> False

    not True
    --> False

    not (not x)
    --> x

    -- for `<`, `>`, `<=`, `>=`, `==` and `/=`
    not (a < b)
    --> a >= b


### Comparisons

    x == True
    --> x

    x /= False
    --> x

    not x == not y
    --> x == y

    anything == anything
    --> True

    anything /= anything
    --> False

    { r | a = 1 } == { r | a = 2 }
    --> False


### If expressions

    if True then x else y
    --> x

    if False then x else y
    --> y

    if condition then x else x
    --> x

    if condition then True else False
    --> condition

    if condition then False else True
    --> not condition

    a =
        if condition then
            if not condition then
                1
            else
                2
        else
            3
    --> if condition then 2 else 3


### Case expressions

    case condition of
        True -> x
        False -> y
    --> if condition then x else y

    case condition of
        False -> y
        True -> x
    --> if not condition then x else y

    -- only when no variables are introduced in the pattern
    -- and no custom types defined in the project are referenced
    case value of
        Just _ -> x
        Nothing -> x
    --> x

    -- same with any variant, list or tuple containing either
    case Just value of
        Nothing -> a
        Just (Ok b) -> c
        Just (Err d) -> e
    --> case value of
    -->     Ok b -> c
    -->     Err d -> e

### Arrays

    Array.fromList []
    --> Array.empty

    Array.fromList (Array.toList array)
    --> array

    Array.toList (Array.fromList list)
    --> list

    Array.toList Array.empty
    --> []

    Array.toList (Array.repeat n a)
    --> List.repeat n a

    Array.map f Array.empty -- same for Array.filter
    --> Array.empty

    Array.map identity array
    --> array

    Array.indexedMap (\\_ value -> f value) array
    --> Array.map (\\value -> f value) array

    Array.isEmpty Array.empty
    --> True

    Array.repeat 0 x
    --> Array.empty

    Array.initialize 0 f
    --> Array.empty

    Array.length Array.empty
    --> 0

    Array.length (Array.fromList [ a, b, c ])
    --> 3

    Array.length (Array.repeat 3 x)
    --> 3

    Array.length (Array.initialize 3 f)
    --> 3

    Array.length (Array.repeat n x)
    --> max 0 n

    Array.length (Array.initialize n f)
    --> max 0 n

    Array.append Array.empty array
    --> array

    Array.append (Array.fromList [ a, b ]) (Array.fromList [ c, d ])
    --> Array.fromList [ a, b, c, d ]

    Array.slice n n array
    --> Array.empty

    Array.slice n 0 array
    --> Array.empty

    Array.slice a z Array.empty
    --> Array.empty

    Array.slice 2 1 array
    --> Array.empty

    Array.slice -1 -2 array
    --> Array.empty

    Array.get n Array.empty
    --> Nothing

    Array.get 1 (Array.fromList [ a, b, c ])
    --> Just b

    Array.get 100 (Array.fromList [ a, b, c ])
    --> Nothing

    Array.get -1 array
    --> Nothing

    Array.get 2 (Array.repeat 10 x)
    --> Just x

    Array.get 100 (Array.repeat 10 x)
    --> Nothing

    Array.get 2 (Array.initialize 10 f)
    --> Just (f 2)

    Array.get 100 (Array.initialize 10 f)
    --> Nothing

    Array.set n x Array.empty
    --> Array.empty

    Array.set -1 x array
    --> array

    Array.set 1 x (Array.fromList [ a, b, c ])
    --> Array.fromList [ a, x, c ]

    Array.set 100 x (Array.fromList [ a, b, c ])
    --> Array.fromList [ a, b, c ]

    -- The following simplifications for Array.foldl also work for Array.foldr
    Array.foldl f initial Array.empty
    --> initial

    Array.foldl (\\_ soFar -> soFar) initial array
    --> initial

    Array.toIndexedList Array.empty
    --> []

    List.map Tuple.second (Array.toIndexedList array)
    --> Array.toList array

    Array.length (Array.fromList list)
    --> List.length list

    -- The following simplification also works for Array.toIndexedList
    List.length (Array.toList array)
    --> Array.length array

    -- The following simplification also works for Array.toIndexedList
    List.isEmpty (Array.toList array)
    --> Array.isEmpty array


### Sets

    Set.fromList []
    --> Set.empty

    Set.fromList [ a ]
    --> Set.singleton a

    Set.fromList (Set.toList set)
    --> set

    Set.map f Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity set
    --> set

    Set.isEmpty Set.empty
    --> True

    Set.isEmpty (Set.fromList ([a] ++ list)
    --> False

    Set.member x Set.empty
    --> False

    Set.toList Set.empty
    --> []

    Set.length Set.empty
    --> 0

    Set.intersect Set.empty set
    --> Set.empty

    Set.intersect set set
    --> set

    Set.diff Set.empty set
    --> Set.empty

    Set.diff set Set.empty
    --> set

    Set.union set Set.empty
    --> set

    Set.union set set
    --> set

    Set.union (Set.fromList [ a, b ]) (Set.fromList [ c, d ])
    --> Set.fromList [ a, b, c, d]

    Set.insert x Set.empty
    --> Set.singleton x

    -- same for foldr
    List.foldl f x (Set.toList set)
    --> Set.foldl f x set

    Set.filter (\\_ -> True) set
    --> set

    Set.filter (\\_ -> False) set
    --> Set.empty

    Set.partition f Set.empty
    --> ( Set.empty, Set.empty )

    Set.partition (always True) set
    --> ( set, Set.empty )

    Tuple.first (Set.partition f set)
    --> Set.filter f set

    -- The following simplifications for Set.foldl also work for Set.foldr
    Set.foldl f initial Set.empty
    --> initial

    Set.foldl (\\_ soFar -> soFar) initial set
    --> initial

    List.length (Set.toList set)
    --> Set.size set

    List.isEmpty (Set.toList set)
    --> Set.isEmpty set


### Dict

    Dict.fromList []
    --> Dict.empty

    Dict.fromList (Dict.toList dict)
    --> dict

    Dict.isEmpty Dict.empty
    --> True

    Dict.toList Dict.empty
    --> []

    Dict.size Dict.empty
    --> 0

    Dict.member x Dict.empty
    --> False

    Dict.remove k Dict.empty
    --> Dict.empty

    Dict.filter f Dict.empty
    --> Dict.empty

    Dict.filter (\\_ _ -> True) dict
    --> dict

    Dict.filter (\\_ _ -> False) dict
    --> Dict.empty

    Dict.map f Dict.empty
    --> Dict.empty

    Dict.map (\\_ value -> value) dict
    --> dict

    Dict.intersect Dict.empty dict
    --> Dict.empty

    Dict.intersect dict dict
    --> dict

    Dict.diff Dict.empty dict
    --> Dict.empty

    Dict.diff dict Dict.empty
    --> dict

    Dict.union dict Dict.empty
    --> dict

    Dict.union dict dict
    --> dict

    Dict.union (Dict.fromList [ a, b ]) (Dict.fromList [ c, d ])
    --> Dict.fromList [ c, d, a, b ]

    Dict.partition f Dict.empty
    --> ( Dict.empty, Dict.empty )

    Dict.partition (\\_ _ -> True) dict
    --> ( dict, Dict.empty )

    Dict.partition (\\_ _ -> False) dict
    --> ( Dict.empty, dict )

    Tuple.first (Dict.partition f dict)
    --> Dict.filter f dict

    List.map Tuple.first (Dict.toList dict)
    --> Dict.keys dict

    List.map Tuple.second (Dict.toList dict)
    --> Dict.values dict

    -- same for foldr
    Dict.foldl f initial Dict.empty
    --> initial

    Dict.foldl (\\_ soFar -> soFar) initial dict
    --> initial

    -- The following simplification also works for Dict.keys, Dict.values
    List.length (Dict.toList dict)
    --> Dict.size dict

    -- The following simplification also works for Dict.keys, Dict.values
    List.isEmpty (Dict.toList dict)
    --> Dict.isEmpty dict


### Cmd / Sub

All of these also apply for `Sub`.

    Cmd.batch []
    --> Cmd.none

    Cmd.batch [ a ]
    --> a

    Cmd.batch [ a, Cmd.none, b ]
    --> Cmd.batch [ a, b ]

    Cmd.batch [ a, Cmd.batch [ b, c ], d ]
    --> Cmd.batch [ a, b, c, d ]

    Cmd.map identity cmd
    --> cmd

    Cmd.map f Cmd.none
    --> Cmd.none


### Task

    Task.map identity task
    --> task

    Task.map f (Task.fail x)
    --> Task.fail x

    Task.map f (Task.succeed a)
    --> Task.succeed (f a)

    -- the following simplifications for map3 work for all Task.mapN
    Task.map3 f (Task.succeed a) (Task.succeed b) (Task.succeed c)
    --> Task.succeed (f a b c)

    Task.map3 f (Task.succeed a) (Task.fail x) thirdTask
    --> Task.fail x

    Task.map3 f firstTask (Task.fail x) thirdTask
    --> Task.map2 f firstTask (Task.fail x)

    Task.andThen f (Task.fail x)
    --> Task.fail x

    Task.andThen f (Task.succeed a)
    --> f a

    Task.andThen Task.succeed task
    --> task

    Task.andThen (\\a -> Task.succeed b) task
    --> Task.map (\\a -> b) task

    Task.mapError identity task
    --> task

    Task.mapError f (Task.succeed a)
    --> Task.succeed a

    Task.mapError f (Task.fail x)
    --> Task.fail (f x)

    Task.onError f (Task.succeed a)
    --> Task.succeed a

    Task.onError f (Task.fail x)
    --> f x

    Task.onError Task.fail task
    --> task

    Task.onError (\\x -> Task.fail y) task
    --> Task.mapError (\\x -> y) x

    Task.sequence [ Task.succeed a, Task.succeed b ]
    --> Task.succeed [ a, b ]

    Task.sequence [ Task.succeed a, Task.fail x ]
    --> Task.fail x

    Task.sequence [ a, Task.fail x, b ]
    --> Task.sequence [ a, Task.fail x ]

    Task.sequence [ task ]
    --> Task.map List.singleton task


### Html.Attributes

    Html.Attributes.classList [ x, y, ( z, False ) ]
    --> Html.Attributes.classList [ x, y ]

    Html.Attributes.classList [ ( onlyOneThing, True ) ]
    --> Html.Attributes.class onlyOneThing

### Parser

    Parser.oneOf [ a ]
    --> a

### Test

    Test.concat [ test ]
    --> test

    Test.concat [ test0, Test.concat [ test1, test2 ], test3 ]
    --> Test.concat [ test0, test1, test2, test3 ]

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project exposing (Exposed)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.Type
import Fn.Array
import Fn.Basics
import Fn.Dict
import Fn.Html.Attributes
import Fn.Json.Decode
import Fn.List
import Fn.Maybe
import Fn.Parser
import Fn.Parser.Advanced
import Fn.Platform.Cmd
import Fn.Platform.Sub
import Fn.Random
import Fn.Result
import Fn.Set
import Fn.String
import Fn.Task
import Fn.Test
import Fn.Tuple
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers exposing (emptyStringAsString, qualifiedToString)
import Simplify.Evaluate as Evaluate
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize
import Simplify.RangeDict as RangeDict exposing (RangeDict)


{-| Rule to simplify Elm code.
-}
rule : Configuration -> Rule
rule (Configuration config) =
    Rule.newProjectRuleSchema "Simplify" initialContext
        |> Rule.withDirectDependenciesProjectVisitor (dependenciesVisitor (Set.fromList config.ignoreConstructors))
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : { config | expectNaN : Bool } -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withCommentsVisitor (\\comments context -> ( [], commentsVisitor comments context ))
        |> Rule.withDeclarationListVisitor (\\decls context -> ( [], declarationListVisitor decls context ))
        |> Rule.withDeclarationEnterVisitor (\\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\\expressionNode context -> expressionVisitor expressionNode config context)
        |> Rule.withExpressionExitVisitor (\\node context -> ( [], expressionExitVisitor node context ))



-- CONFIGURATION


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) and [`expectNaN`](#expectNaN) to alter it.
-}
type Configuration
    = Configuration
        { ignoreConstructors : List String
        , expectNaN : Bool
        }


{-| Default configuration for this rule.

The rule aims tries to improve the code through simplifications that don't impact the behavior. An exception to this are
when the presence of `NaN` values

Use [`expectNaN`](#expectNaN) if you want to opt out of changes that can impact the behaviour of your code if you expect to work with `NaN` values.

Use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) if you want to prevent simplifying case expressions that work on custom types defined in dependencies.

    config =
        [ Simplify.rule Simplify.defaults
        ]

    -- or
    config =
        [ Simplify.defaults
            |> Simplify.expectNaN
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

-}
defaults : Configuration
defaults =
    Configuration
        { ignoreConstructors = []
        , expectNaN = False
        }


{-| Ignore some reports about types from dependencies used in case expressions.

This rule simplifies the following construct:

    module Module.Name exposing (..)

    case value of
        Just _ -> x
        Nothing -> x
    --> x

(Since `v2.0.19`) it will not try to simplify the case expression when some of the patterns references custom types constructors
defined in the project. It will only do so for custom types that are defined in dependencies (including `elm/core`).

If you do happen to want to disable this simplification for a type `Module.Name.Type`, you can configure the rule like this:

    config =
        [ Simplify.defaults
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

I personally don't recommend to use this function too much, because this could be a sign of premature abstraction, and because
I think that often [You Aren't Gonna Need this code](https://jfmengels.net/safe-dead-code-removal/#yagni-you-arent-gonna-need-it).

Please let me know by opening an issue if you do use this function, I am very curious to know;

-}
ignoreCaseOfForTypes : List String -> Configuration -> Configuration
ignoreCaseOfForTypes ignoreConstructors (Configuration config) =
    Configuration { ignoreConstructors = ignoreConstructors ++ config.ignoreConstructors, expectNaN = config.expectNaN }


{-| Usually, `elm-review-simplify` will only suggest simplifications that are safe to apply without risk of changing the original behavior.
However, when encountering [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN)
values, some simplifications can actually impact behavior.

For instance, the following expression will evaluate to `True`:

    x == x
    --> True

However, if `x` is `NaN` or a value containing `NaN` then the expression will evaluate to `False`:

    -- given x = NaN
    x == x
    --> False

    -- given x = { a = ( NaN, 0 ) }
    x == x
    --> False

Given the potential presence of `NaN`, some simplifications become unsafe to apply:

  - `x == x` to `True`
  - `List.member x [ x ]` to `True`
  - `n * 0` to `0`

This special value is hard to recreate in Elm code both intentionally and unintentionally,
and it's therefore unlikely to be found in your application,
which is why the rule applies these simplifications by defaults.

If you somehow expect to create and encounter `NaN` values in your codebase, then you can use this function to disable these simplifications altogether.

    config =
        [ Simplify.defaults
            |> Simplify.expectNaN
            |> Simplify.rule
        ]

-}
expectNaN : Configuration -> Configuration
expectNaN (Configuration config) =
    Configuration { ignoreConstructors = config.ignoreConstructors, expectNaN = True }



-- CONTEXT


type alias ProjectContext =
    { customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , exposedVariants : Dict ModuleName (Set String)
    , exposedRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , exposedCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , exposed : ExposingContext
    , commentRanges : List Range
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , importCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
    , moduleCustomTypes : Dict String { variantNames : Set String }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , branchLocalBindings : RangeDict (Set String)
    , rangesToIgnore : RangeDict ()
    , rightSidesOfPlusPlus : RangeDict ()
    , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , localIgnoredCustomTypes : List Constructor
    , constructorsToIgnore : Set ( ModuleName, String )
    , inferredConstantsDict : RangeDict Infer.Inferred
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , extractSourceCode : Range -> String
    , exposedVariants : Set String
    , importLookup : ImportLookup
    }


type alias ImportLookup =
    Dict
        ModuleName
        { alias : Maybe ModuleName
        , exposed :
            -- includes names of found variants
            Exposed
        }


type alias QualifyResources a =
    { a
        | importLookup : ImportLookup
        , moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }


defaultQualifyResources : QualifyResources {}
defaultQualifyResources =
    { importLookup = implicitImports
    , localBindings = RangeDict.empty
    , moduleBindings = Set.empty
    }


type ExposingContext
    = ExposingAllContext
    | ExposingSomeContext { typesExposingVariants : Set String, potentialTypeAliases : Set String }


type Exposed
    = ExposedAll
    | ExposedSome (Set String)


isExposedFrom : Exposed -> String -> Bool
isExposedFrom exposed name =
    case exposed of
        ExposedAll ->
            True

        ExposedSome some ->
            Set.member name some


type alias ConstructorName =
    String


type alias Constructor =
    { moduleName : ModuleName
    , name : String
    , constructors : List String
    }


initialContext : ProjectContext
initialContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.empty
    , exposedRecordTypeAliases = Dict.empty
    , exposedCustomTypes = Dict.empty
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\\moduleContext ->
            { customTypesToReportInCases = Set.empty
            , exposedVariants =
                Dict.singleton moduleContext.moduleName
                    moduleContext.exposedVariants
            , exposedRecordTypeAliases =
                Dict.singleton moduleContext.moduleName
                    (case moduleContext.exposed of
                        ExposingAllContext ->
                            moduleContext.moduleRecordTypeAliases

                        ExposingSomeContext exposingSomeContext ->
                            Set.foldl
                                (\\exposedPotentialTypeAlias soFar ->
                                    case Dict.get exposedPotentialTypeAlias moduleContext.moduleRecordTypeAliases of
                                        Nothing ->
                                            soFar

                                        Just recordTypeAlias ->
                                            Dict.insert exposedPotentialTypeAlias recordTypeAlias soFar
                                )
                                Dict.empty
                                exposingSomeContext.potentialTypeAliases
                    )
            , exposedCustomTypes =
                Dict.singleton moduleContext.moduleName
                    (case moduleContext.exposed of
                        ExposingAllContext ->
                            moduleContext.moduleCustomTypes

                        ExposingSomeContext exposingSomeContext ->
                            Set.foldl
                                (\\exposedPotentialTypeAlias soFar ->
                                    case Dict.get exposedPotentialTypeAlias moduleContext.moduleCustomTypes of
                                        Nothing ->
                                            soFar

                                        Just recordTypeAlias ->
                                            Dict.insert exposedPotentialTypeAlias recordTypeAlias soFar
                                )
                                Dict.empty
                                exposingSomeContext.typesExposingVariants
                    )
            }
        )


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\\lookupTable metadata extractSourceCode fullAst projectContext ->
            let
                imports : ImportLookup
                imports =
                    List.foldl
                        (\\import_ importLookup ->
                            let
                                importInfo : { moduleName : ModuleName, exposed : Exposed, alias : Maybe ModuleName }
                                importInfo =
                                    importContext import_
                            in
                            insertImport importInfo.moduleName { alias = importInfo.alias, exposed = importInfo.exposed } importLookup
                        )
                        implicitImports
                        fullAst.imports
            in
            { lookupTable = lookupTable
            , moduleName = Rule.moduleNameFromMetadata metadata
            , exposed =
                moduleExposingContext (Elm.Syntax.Module.exposingList (Node.value fullAst.moduleDefinition))
            , importLookup =
                createImportLookup
                    { imports = imports
                    , importExposedVariants = projectContext.exposedVariants
                    }
            , commentRanges = []
            , importRecordTypeAliases = projectContext.exposedRecordTypeAliases
            , moduleRecordTypeAliases = Dict.empty
            , importCustomTypes = projectContext.exposedCustomTypes
            , moduleCustomTypes = Dict.empty
            , moduleBindings = Set.empty
            , localBindings = RangeDict.empty
            , branchLocalBindings = RangeDict.empty
            , rangesToIgnore = RangeDict.empty
            , rightSidesOfPlusPlus = RangeDict.empty
            , localIgnoredCustomTypes = []
            , customTypesToReportInCases = projectContext.customTypesToReportInCases
            , constructorsToIgnore = Set.empty
            , inferredConstantsDict = RangeDict.empty
            , inferredConstants = ( Infer.empty, [] )
            , extractSourceCode = extractSourceCode
            , exposedVariants = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata
        |> Rule.withSourceCodeExtractor
        |> Rule.withFullAst


importContext : Node Import -> { moduleName : ModuleName, exposed : Exposed, alias : Maybe ModuleName }
importContext importNode =
    let
        import_ : Import
        import_ =
            Node.value importNode
    in
    { moduleName = import_.moduleName |> Node.value
    , alias =
        import_.moduleAlias |> Maybe.map Node.value
    , exposed =
        case import_.exposingList of
            Nothing ->
                ExposedSome Set.empty

            Just (Node _ existingExposing) ->
                case existingExposing of
                    Exposing.All _ ->
                        ExposedAll

                    Exposing.Explicit exposes ->
                        ExposedSome
                            (Set.fromList
                                (List.map
                                    (\\(Node _ expose) -> AstHelpers.nameOfExpose expose)
                                    exposes
                                )
                            )
    }


createImportLookup :
    { imports : Dict ModuleName { alias : Maybe ModuleName, exposed : Exposed }
    , importExposedVariants : Dict ModuleName (Set String)
    }
    -> ImportLookup
createImportLookup context =
    context.imports
        |> Dict.map
            (\\moduleName import_ ->
                case import_.exposed of
                    ExposedAll ->
                        import_

                    ExposedSome some ->
                        case Dict.get moduleName context.importExposedVariants of
                            Nothing ->
                                import_

                            Just importExposedVariants ->
                                { import_
                                    | exposed =
                                        ExposedSome
                                            (Set.union some importExposedVariants)
                                }
            )


moduleExposingContext : Exposing.Exposing -> ExposingContext
moduleExposingContext exposingSyntax =
    case exposingSyntax of
        Exposing.All _ ->
            ExposingAllContext

        Exposing.Explicit some ->
            ExposingSomeContext
                (List.foldl
                    (\\(Node _ expose) soFar ->
                        case expose of
                            Exposing.InfixExpose _ ->
                                soFar

                            Exposing.FunctionExpose _ ->
                                soFar

                            Exposing.TypeOrAliasExpose name ->
                                { soFar | potentialTypeAliases = Set.insert name soFar.potentialTypeAliases }

                            Exposing.TypeExpose variantType ->
                                case variantType.open of
                                    Nothing ->
                                        soFar

                                    Just _ ->
                                        { soFar | typesExposingVariants = Set.insert variantType.name soFar.typesExposingVariants }
                    )
                    { typesExposingVariants = Set.empty
                    , potentialTypeAliases = Set.empty
                    }
                    some
                )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.union newContext.exposedVariants previousContext.exposedVariants
    , exposedRecordTypeAliases = Dict.union newContext.exposedRecordTypeAliases previousContext.exposedRecordTypeAliases
    , exposedCustomTypes = Dict.union newContext.exposedCustomTypes previousContext.exposedCustomTypes
    }



-- DEPENDENCIES VISITOR


dependenciesVisitor : Set String -> Dict String Dependency -> ProjectContext -> ( List (Error scope), ProjectContext )
dependenciesVisitor typeNamesAsStrings dict context =
    let
        modules : List Elm.Docs.Module
        modules =
            dict
                |> Dict.values
                |> List.concatMap Dependency.modules

        unions : Set String
        unions =
            List.concatMap (\\module_ -> List.map (\\union -> module_.name ++ "." ++ union.name) module_.unions) modules
                |> Set.fromList

        unknownTypesToIgnore : List String
        unknownTypesToIgnore =
            Set.diff typeNamesAsStrings unions
                |> Set.toList

        customTypesToReportInCases : Set ( ModuleName, String )
        customTypesToReportInCases =
            modules
                |> List.concatMap
                    (\\mod ->
                        let
                            moduleName : ModuleName
                            moduleName =
                                AstHelpers.moduleNameFromString mod.name
                        in
                        mod.unions
                            |> List.filter (\\union -> not (Set.member (mod.name ++ "." ++ union.name) typeNamesAsStrings))
                            |> List.concatMap (\\union -> union.tags)
                            |> List.map (\\( tagName, _ ) -> ( moduleName, tagName ))
                    )
                |> Set.fromList

        dependencyExposedVariants : Dict ModuleName (Set String)
        dependencyExposedVariants =
            List.foldl
                (\\moduleDoc acc ->
                    Dict.insert
                        (AstHelpers.moduleNameFromString moduleDoc.name)
                        (moduleDoc.unions
                            |> List.concatMap
                                (\\union ->
                                    union.tags
                                        |> List.map (\\( variantName, _ ) -> variantName)
                                )
                            |> Set.fromList
                        )
                        acc
                )
                context.exposedVariants
                modules

        recordTypeAliases : Dict ModuleName (Dict String (List String))
        recordTypeAliases =
            modules
                |> List.foldl
                    (\\moduleDocs soFar ->
                        Dict.insert (AstHelpers.moduleNameFromString moduleDocs.name)
                            (moduleDocs.aliases
                                |> List.filterMap
                                    (\\typeAliasDocs ->
                                        case typeAliasDocs.tipe of
                                            Elm.Type.Record fields Nothing ->
                                                Just ( typeAliasDocs.name, List.map (\\( name, _ ) -> name) fields )

                                            _ ->
                                                Nothing
                                    )
                                |> Dict.fromList
                            )
                            soFar
                    )
                    Dict.empty

        exposedCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
        exposedCustomTypes =
            Dict.union
                (dict
                    |> Dict.values
                    |> List.concatMap
                        (\\dependency ->
                            dependency
                                |> Dependency.modules
                                |> List.map
                                    (\\moduleDocs ->
                                        ( moduleDocs.name |> AstHelpers.moduleNameFromString
                                        , moduleDocs.unions
                                            |> List.map
                                                (\\choiceTypeDocs ->
                                                    ( choiceTypeDocs.name
                                                    , { variantNames =
                                                            choiceTypeDocs.tags |> List.map (\\( name, _ ) -> name) |> Set.fromList
                                                      }
                                                    )
                                                )
                                            |> Dict.fromList
                                        )
                                    )
                        )
                    |> Dict.fromList
                )
                context.exposedCustomTypes
    in
    ( if List.isEmpty unknownTypesToIgnore then
        []

      else
        [ errorForUnknownIgnoredConstructor unknownTypesToIgnore ]
    , { customTypesToReportInCases = customTypesToReportInCases
      , exposedVariants = dependencyExposedVariants
      , exposedRecordTypeAliases = recordTypeAliases
      , exposedCustomTypes = exposedCustomTypes
      }
    )


errorForUnknownIgnoredConstructor : List String -> Error scope
errorForUnknownIgnoredConstructor list =
    Rule.globalError
        { message = "Could not find type names: " ++ (String.join ", " <| List.map wrapInBackticks list)
        , details =
            [ "I expected to find these custom types in the dependencies, but I could not find them."
            , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
            , "If you find that these types have been moved or renamed, please update your configuration."
            , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
            , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
            ]
        }



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> ModuleContext -> ModuleContext
commentsVisitor comments context =
    { context | commentRanges = List.map Node.range comments }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor declarationList context =
    { context
        | moduleBindings = AstHelpers.declarationListBindings declarationList
        , moduleRecordTypeAliases =
            List.foldl
                (\\(Node _ declaration) soFar ->
                    case declaration of
                        Declaration.AliasDeclaration typeAliasDeclaration ->
                            case typeAliasDeclaration.typeAnnotation of
                                Node _ (TypeAnnotation.Record fields) ->
                                    Dict.insert (Node.value typeAliasDeclaration.name)
                                        (List.map (\\(Node _ ( Node _ field, _ )) -> field) fields)
                                        soFar

                                _ ->
                                    soFar

                        _ ->
                            soFar
                )
                Dict.empty
                declarationList
        , moduleCustomTypes =
            List.foldl
                (\\(Node _ declaration) soFar ->
                    case declaration of
                        Declaration.CustomTypeDeclaration variantType ->
                            Dict.insert (Node.value variantType.name)
                                { variantNames =
                                    variantType.constructors
                                        |> List.map (\\(Node _ variant) -> Node.value variant.name)
                                        |> Set.fromList
                                }
                                soFar

                        _ ->
                            soFar
                )
                Dict.empty
                declarationList
    }



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor declarationNode context =
    case Node.value declarationNode of
        Declaration.CustomTypeDeclaration variantType ->
            let
                variantsAreExposed : Bool
                variantsAreExposed =
                    case context.exposed of
                        ExposingAllContext ->
                            True

                        ExposingSomeContext exposingSome ->
                            Set.member (Node.value variantType.name) exposingSome.typesExposingVariants
            in
            if variantsAreExposed then
                let
                    exposedVariants : Set String
                    exposedVariants =
                        List.foldl
                            (\\(Node _ variant) acc -> Set.insert (Node.value variant.name) acc)
                            context.exposedVariants
                            variantType.constructors
                in
                { context | exposedVariants = exposedVariants }

            else
                context

        Declaration.FunctionDeclaration functionDeclaration ->
            { context
                | rangesToIgnore = RangeDict.empty
                , rightSidesOfPlusPlus = RangeDict.empty
                , inferredConstantsDict = RangeDict.empty
                , localBindings =
                    RangeDict.singleton
                        (Node.range functionDeclaration.declaration)
                        (AstHelpers.patternListBindings (Node.value functionDeclaration.declaration).arguments)
            }

        _ ->
            context



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node config context =
    let
        expressionRange : Range
        expressionRange =
            Node.range node

        contextWithInferredConstants : ModuleContext
        contextWithInferredConstants =
            case RangeDict.get expressionRange context.inferredConstantsDict of
                Nothing ->
                    context

                Just inferredConstants ->
                    let
                        ( previous, previousStack ) =
                            context.inferredConstants
                    in
                    { context
                        | inferredConstants = ( inferredConstants, previous :: previousStack )
                    }
    in
    if RangeDict.member expressionRange context.rangesToIgnore then
        ( [], contextWithInferredConstants )

    else
        let
            expression : Expression
            expression =
                Node.value node

            withExpressionSurfaceBindings : RangeDict (Set String)
            withExpressionSurfaceBindings =
                RangeDict.insert expressionRange (expressionSurfaceBindings expression) context.localBindings

            withNewBranchLocalBindings : RangeDict (Set String)
            withNewBranchLocalBindings =
                RangeDict.union (expressionBranchLocalBindings expression)
                    context.branchLocalBindings

            contextWithInferredConstantsAndLocalBindings : ModuleContext
            contextWithInferredConstantsAndLocalBindings =
                case RangeDict.get expressionRange context.branchLocalBindings of
                    Nothing ->
                        { contextWithInferredConstants
                            | localBindings = withExpressionSurfaceBindings
                            , branchLocalBindings =
                                withNewBranchLocalBindings
                        }

                    Just currentBranchLocalBindings ->
                        { contextWithInferredConstants
                            | localBindings =
                                RangeDict.insert expressionRange currentBranchLocalBindings withExpressionSurfaceBindings
                            , branchLocalBindings =
                                RangeDict.remove expressionRange withNewBranchLocalBindings
                        }

            expressionChecked : { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
            expressionChecked =
                expressionVisitorHelp node config contextWithInferredConstantsAndLocalBindings
        in
        ( expressionChecked.error |> maybeToList
        , { contextWithInferredConstantsAndLocalBindings
            | rangesToIgnore = RangeDict.union expressionChecked.rangesToIgnore context.rangesToIgnore
            , rightSidesOfPlusPlus = RangeDict.union expressionChecked.rightSidesOfPlusPlus context.rightSidesOfPlusPlus
            , inferredConstantsDict =
                List.foldl (\\( range, constants ) acc -> RangeDict.insert range constants acc)
                    contextWithInferredConstants.inferredConstantsDict
                    expressionChecked.inferredConstants
          }
        )


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports : ImportLookup
implicitImports =
    [ ( [ "Basics" ], { alias = Nothing, exposed = ExposedAll } )
    , ( [ "List" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "List", "(::)" ]) } )
    , ( [ "Maybe" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Maybe", "Just", "Nothing" ]) } )
    , ( [ "Result" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Result", "Ok", "Err" ]) } )
    , ( [ "String" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "String") } )
    , ( [ "Char" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "Char") } )
    , ( [ "Tuple" ], { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "Program") } )
    , ( [ "Platform", "Cmd" ], { alias = Just [ "Cmd" ], exposed = ExposedSome (Set.singleton "Cmd") } )
    , ( [ "Platform", "Sub" ], { alias = Just [ "Sub" ], exposed = ExposedSome (Set.singleton "Sub") } )
    ]
        |> Dict.fromList


{-| Merge a given new import with an existing import lookup.
This is strongly preferred over Dict.insert since the implicit default imports can be overridden
-}
insertImport : ModuleName -> { alias : Maybe ModuleName, exposed : Exposed } -> ImportLookup -> ImportLookup
insertImport moduleName importInfoToAdd importLookup =
    Dict.update moduleName
        (\\existingImport ->
            let
                newImportInfo : { alias : Maybe ModuleName, exposed : Exposed }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            importInfoToAdd

                        Just import_ ->
                            { alias = findMap .alias [ import_, importInfoToAdd ]
                            , exposed = exposedMerge ( import_.exposed, importInfoToAdd.exposed )
                            }
            in
            Just newImportInfo
        )
        importLookup


exposedMerge : ( Exposed, Exposed ) -> Exposed
exposedMerge exposedTuple =
    case exposedTuple of
        ( ExposedAll, _ ) ->
            ExposedAll

        ( ExposedSome _, ExposedAll ) ->
            ExposedAll

        ( ExposedSome aSet, ExposedSome bSet ) ->
            ExposedSome (Set.union aSet bSet)


qualify : ( ModuleName, String ) -> QualifyResources a -> ( ModuleName, String )
qualify ( moduleName, name ) qualifyResources =
    let
        qualification : ModuleName
        qualification =
            case qualifyResources.importLookup |> Dict.get moduleName of
                Nothing ->
                    moduleName

                Just import_ ->
                    let
                        moduleImportedName : ModuleName
                        moduleImportedName =
                            import_.alias |> Maybe.withDefault moduleName
                    in
                    if not (isExposedFrom import_.exposed name) then
                        moduleImportedName

                    else
                        let
                            isShadowed : Bool
                            isShadowed =
                                isBindingInScope qualifyResources name
                        in
                        if isShadowed then
                            moduleImportedName

                        else
                            []
    in
    ( qualification, name )


isBindingInScope :
    { a
        | moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }
    -> String
    -> Bool
isBindingInScope resources name =
    Set.member name resources.moduleBindings
        || RangeDict.any (\\bindings -> Set.member name bindings) resources.localBindings


{-| Whenever you add ranges on expression enter, the same ranges should be removed on expression exit.
Having one function finding unique ranges and a function for extracting bindings there ensures said consistency.

An alternative approach would be to use some kind of tree structure
with parent and sub ranges and bindings as leaves (maybe a "trie", tho I've not seen one as an elm package).

Removing all bindings for an expression's range on leave would then be trivial

-}
expressionSurfaceBindings : Expression -> Set String
expressionSurfaceBindings expression =
    case expression of
        Expression.LambdaExpression lambda ->
            AstHelpers.patternListBindings lambda.args

        Expression.LetExpression letBlock ->
            AstHelpers.letDeclarationListBindings letBlock.declarations

        _ ->
            Set.empty


expressionBranchLocalBindings : Expression -> RangeDict (Set String)
expressionBranchLocalBindings expression =
    case expression of
        Expression.CaseExpression caseBlock ->
            RangeDict.mapFromList
                (\\( Node _ pattern, Node resultRange _ ) ->
                    ( resultRange
                    , AstHelpers.patternBindings pattern
                    )
                )
                caseBlock.cases

        Expression.LetExpression letBlock ->
            List.foldl
                (\\(Node _ letDeclaration) acc ->
                    case letDeclaration of
                        Expression.LetFunction letFunctionOrValueDeclaration ->
                            RangeDict.insert
                                (Node.range (Node.value letFunctionOrValueDeclaration.declaration).expression)
                                (AstHelpers.patternListBindings
                                    (Node.value letFunctionOrValueDeclaration.declaration).arguments
                                )
                                acc

                        _ ->
                            acc
                )
                RangeDict.empty
                letBlock.declarations

        _ ->
            RangeDict.empty


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor (Node expressionRange _) context =
    let
        contextWithUpdatedLocalBindings : ModuleContext
        contextWithUpdatedLocalBindings =
            if RangeDict.member expressionRange context.rangesToIgnore then
                context

            else
                { context
                    | localBindings =
                        RangeDict.remove expressionRange context.localBindings
                }
    in
    if RangeDict.member expressionRange context.inferredConstantsDict then
        case Tuple.second context.inferredConstants of
            topOfStack :: restOfStack ->
                { contextWithUpdatedLocalBindings | inferredConstants = ( topOfStack, restOfStack ) }

            [] ->
                -- should never be empty
                contextWithUpdatedLocalBindings

    else
        contextWithUpdatedLocalBindings


maybeErrorAndRangesToIgnore : Maybe (Error {}) -> RangeDict () -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
maybeErrorAndRangesToIgnore maybeError rangesToIgnore =
    { error = maybeError
    , rangesToIgnore = rangesToIgnore
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


onlyMaybeError : Maybe (Error {}) -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
onlyMaybeError maybeError =
    { error = maybeError
    , rangesToIgnore = RangeDict.empty
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


expressionVisitorHelp : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
expressionVisitorHelp (Node expressionRange expression) config context =
    let
        toCheckInfo :
            { fnRange : Range
            , fn : ( ModuleName, String )
            , argCount : Int
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
            -> CallCheckInfo
        toCheckInfo checkInfo =
            let
                ( parentRange, callStyle ) =
                    case List.drop (checkInfo.argCount - 1) (checkInfo.firstArg :: checkInfo.argsAfterFirst) of
                        lastExpectedArg :: _ :: _ ->
                            -- Too many arguments!
                            -- We'll update the range to drop the extra ones and force the call style to application
                            ( case checkInfo.callStyle of
                                Application ->
                                    { start = checkInfo.fnRange.start, end = (Node.range lastExpectedArg).end }

                                Pipe LeftToRight ->
                                    { start = checkInfo.fnRange.start, end = (Node.range lastExpectedArg).end }

                                Pipe RightToLeft ->
                                    { start = (Node.range checkInfo.firstArg).start, end = (Node.range checkInfo.firstArg).end }
                            , Application
                            )

                        -- [] | _ :: [] ->
                        _ ->
                            ( expressionRange, checkInfo.callStyle )

                argsAfterFirst : List (Node Expression)
                argsAfterFirst =
                    -- Drop the extra arguments
                    List.take (checkInfo.argCount - 1) checkInfo.argsAfterFirst
            in
            { lookupTable = context.lookupTable
            , expectNaN = config.expectNaN
            , extractSourceCode = context.extractSourceCode
            , importLookup = context.importLookup
            , commentRanges = context.commentRanges
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , inferredConstants = context.inferredConstants
            , parentRange = parentRange
            , fnRange = checkInfo.fnRange
            , fn = checkInfo.fn
            , argCount = checkInfo.argCount
            , firstArg = checkInfo.firstArg
            , argsAfterFirst = argsAfterFirst
            , secondArg = List.head argsAfterFirst
            , thirdArg = List.head (List.drop 1 argsAfterFirst)
            , callStyle = callStyle
            }

        toCompositionCheckInfo :
            { earlier : Node Expression
            , later : Node Expression
            }
            -> CompositionCheckInfo
        toCompositionCheckInfo compositionSpecific =
            let
                innerComposition :
                    { earlier :
                        { node : Node Expression, removeRange : Range }
                    , later :
                        { node : Node Expression, removeRange : Range }
                    , isEmbeddedInComposition : Bool
                    }
                innerComposition =
                    getInnerComposition compositionSpecific
            in
            { lookupTable = context.lookupTable
            , importLookup = context.importLookup
            , importRecordTypeAliases = context.importRecordTypeAliases
            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
            , inferredConstants = context.inferredConstants
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , extractSourceCode = context.extractSourceCode
            , earlier = innerComposition.earlier
            , later = innerComposition.later
            , isEmbeddedInComposition = innerComposition.isEmbeddedInComposition
            }
    in
    case expression of
        -----------------
        -- APPLICATION --
        -----------------
        Expression.Application (applied :: firstArg :: argsAfterFirst) ->
            onlyMaybeError
                (case applied of
                    Node fnRange (Expression.FunctionOrValue _ fnName) ->
                        case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                case Dict.get ( moduleName, fnName ) functionCallChecks of
                                    Just ( argCount, checkFn ) ->
                                        checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsAfterFirst
                                                , callStyle = Application
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    Node _ (Expression.ParenthesizedExpression (Node lambdaRange (Expression.LambdaExpression lambda))) ->
                        appliedLambdaError
                            { nodeRange = expressionRange
                            , lambdaRange = lambdaRange
                            , lambda = lambda
                            }

                    Node operatorRange (Expression.PrefixOperator operator) ->
                        case argsAfterFirst of
                            right :: [] ->
                                Just
                                    (fullyAppliedPrefixOperatorError
                                        { operator = operator
                                        , operatorRange = operatorRange
                                        , left = firstArg
                                        , right = right
                                        }
                                    )

                            _ ->
                                Nothing

                    otherApplied ->
                        case AstHelpers.getRecordAccessFunction otherApplied of
                            Just fieldName ->
                                accessingRecordChecks
                                    { parentRange = Range.combine [ Node.range applied, Node.range firstArg ]
                                    , record = firstArg
                                    , fieldRange = Node.range otherApplied
                                    , fieldName = fieldName
                                    , importRecordTypeAliases = context.importRecordTypeAliases
                                    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                                    , lookupTable = context.lookupTable
                                    }
                                    |> Maybe.map (\\e -> Rule.errorWithFix e.info (Node.range otherApplied) e.fix)

                            Nothing ->
                                Nothing
                )

        ----------
        -- (<|) --
        ----------
        Expression.OperatorApplication "<|" _ pipedInto lastArg ->
            case pipedInto of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    onlyMaybeError
                        (case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                case Dict.get ( moduleName, fnName ) functionCallChecks of
                                    Just ( argCount, checkFn ) ->
                                        checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , callStyle = Pipe RightToLeft
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )

                Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast)) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            case Dict.get ( moduleName, fnName ) functionCallChecks of
                                Just ( argCount, checkFn ) ->
                                    maybeErrorAndRangesToIgnore
                                        (checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , argCount = argCount
                                                , fn = ( moduleName, fnName )
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , callStyle = Pipe RightToLeft
                                                }
                                            )
                                        )
                                        (RangeDict.singleton applicationRange ())

                                Nothing ->
                                    onlyMaybeError Nothing

                        Nothing ->
                            onlyMaybeError Nothing

                pipedIntoOther ->
                    onlyMaybeError
                        (pipelineChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = RightToLeft
                            , nodeRange = expressionRange
                            , pipedInto = pipedIntoOther
                            , arg = lastArg
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , lookupTable = context.lookupTable
                            }
                        )

        ----------
        -- (|>) --
        ----------
        Expression.OperatorApplication "|>" _ lastArg pipedInto ->
            case pipedInto of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    onlyMaybeError
                        (case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                case Dict.get ( moduleName, fnName ) functionCallChecks of
                                    Just ( argCount, checks ) ->
                                        checks
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , callStyle = Pipe LeftToRight
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )

                Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast)) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            case Dict.get ( moduleName, fnName ) functionCallChecks of
                                Just ( argCount, checks ) ->
                                    maybeErrorAndRangesToIgnore
                                        (checks
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , callStyle = Pipe LeftToRight
                                                }
                                            )
                                        )
                                        (RangeDict.singleton applicationRange ())

                                Nothing ->
                                    onlyMaybeError Nothing

                        Nothing ->
                            onlyMaybeError Nothing

                pipedIntoOther ->
                    onlyMaybeError
                        (pipelineChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = LeftToRight
                            , nodeRange = expressionRange
                            , pipedInto = pipedIntoOther
                            , arg = lastArg
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , lookupTable = context.lookupTable
                            }
                        )

        ----------
        -- (>>) --
        ----------
        Expression.OperatorApplication ">>" _ earlier composedLater ->
            onlyMaybeError
                (firstThatConstructsJust compositionChecks
                    (toCompositionCheckInfo { earlier = earlier, later = composedLater })
                )

        ----------
        -- (<<) --
        ----------
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            onlyMaybeError
                (firstThatConstructsJust compositionChecks
                    (toCompositionCheckInfo { earlier = earlier, later = composedLater })
                )

        ---------------------
        -- OTHER OPERATION --
        ---------------------
        Expression.OperatorApplication operator _ left right ->
            case Dict.get operator operatorApplicationChecks of
                Just checkFn ->
                    { error =
                        let
                            leftRange : Range
                            leftRange =
                                Node.range left

                            rightRange : Range
                            rightRange =
                                Node.range right
                        in
                        checkFn
                            { lookupTable = context.lookupTable
                            , extractSourceCode = context.extractSourceCode
                            , expectNaN = config.expectNaN
                            , importLookup = context.importLookup
                            , moduleBindings = context.moduleBindings
                            , localBindings = context.localBindings
                            , inferredConstants = context.inferredConstants
                            , parentRange = expressionRange
                            , operator = operator
                            , operatorRange =
                                findOperatorRange
                                    { operator = operator
                                    , commentRanges = context.commentRanges
                                    , extractSourceCode = context.extractSourceCode
                                    , leftRange = leftRange
                                    , rightRange = rightRange
                                    }
                            , left = left
                            , leftRange = leftRange
                            , right = right
                            , rightRange = rightRange
                            , isOnTheRightSideOfPlusPlus = RangeDict.member expressionRange context.rightSidesOfPlusPlus
                            }
                    , rangesToIgnore = RangeDict.empty
                    , rightSidesOfPlusPlus =
                        case operator of
                            "++" ->
                                RangeDict.singleton (Node.range (AstHelpers.removeParens right)) ()

                            _ ->
                                RangeDict.empty
                    , inferredConstants = []
                    }

                Nothing ->
                    onlyMaybeError Nothing

        --------------
        -- NEGATION --
        --------------
        Expression.Negation negatedExpression ->
            onlyMaybeError
                (negationChecks { parentRange = expressionRange, negatedExpression = negatedExpression })

        -------------------
        -- RECORD ACCESS --
        -------------------
        Expression.RecordAccess record (Node fieldRange fieldName) ->
            onlyMaybeError
                (accessingRecordChecks
                    { parentRange = expressionRange
                    , record = record
                    , fieldRange = fieldRange
                    , fieldName = fieldName
                    , importRecordTypeAliases = context.importRecordTypeAliases
                    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                    , lookupTable = context.lookupTable
                    }
                    |> Maybe.map (\\e -> Rule.errorWithFix e.info { start = (Node.range record).end, end = fieldRange.end } e.fix)
                )

        --------
        -- IF --
        --------
        Expression.IfBlock condition trueBranch falseBranch ->
            let
                ifCheckInfo : IfCheckInfo
                ifCheckInfo =
                    { nodeRange = expressionRange
                    , condition = condition
                    , trueBranch = trueBranch
                    , falseBranch = falseBranch
                    , lookupTable = context.lookupTable
                    , inferredConstants = context.inferredConstants
                    , importLookup = context.importLookup
                    , moduleBindings = context.moduleBindings
                    , localBindings = context.localBindings
                    }
            in
            case ifChecks ifCheckInfo of
                Just ifErrors ->
                    maybeErrorAndRangesToIgnore (Just ifErrors.errors) ifErrors.rangesToIgnore

                Nothing ->
                    { error = Nothing
                    , rangesToIgnore = RangeDict.empty
                    , rightSidesOfPlusPlus = RangeDict.empty
                    , inferredConstants =
                        Infer.inferForIfCondition
                            (Node.value (Normalize.normalize context condition))
                            { trueBranchRange = Node.range trueBranch
                            , falseBranchRange = Node.range falseBranch
                            }
                            (Tuple.first context.inferredConstants)
                    }

        -------------
        -- CASE OF --
        -------------
        Expression.CaseExpression caseBlock ->
            onlyMaybeError
                (firstThatConstructsJust caseOfChecks
                    { lookupTable = context.lookupTable
                    , moduleCustomTypes = context.moduleCustomTypes
                    , importCustomTypes = context.importCustomTypes
                    , extractSourceCode = context.extractSourceCode
                    , customTypesToReportInCases = context.customTypesToReportInCases
                    , inferredConstants = context.inferredConstants
                    , parentRange = expressionRange
                    , caseOf = caseBlock
                    }
                )

        ------------
        -- LET IN --
        ------------
        Expression.LetExpression caseBlock ->
            onlyMaybeError (letInChecks caseBlock)

        -------------------
        -- RECORD UPDATE --
        -------------------
        Expression.RecordUpdateExpression variable fields ->
            onlyMaybeError (recordUpdateChecks expressionRange variable fields)

        --------------------
        -- NOT SIMPLIFIED --
        --------------------
        Expression.UnitExpr ->
            onlyMaybeError Nothing

        Expression.CharLiteral _ ->
            onlyMaybeError Nothing

        Expression.Integer _ ->
            onlyMaybeError Nothing

        Expression.Hex _ ->
            onlyMaybeError Nothing

        Expression.Floatable _ ->
            onlyMaybeError Nothing

        Expression.Literal _ ->
            onlyMaybeError Nothing

        Expression.GLSLExpression _ ->
            onlyMaybeError Nothing

        Expression.PrefixOperator _ ->
            onlyMaybeError Nothing

        Expression.RecordAccessFunction _ ->
            onlyMaybeError Nothing

        Expression.FunctionOrValue _ _ ->
            onlyMaybeError Nothing

        Expression.ParenthesizedExpression _ ->
            onlyMaybeError Nothing

        Expression.TupledExpression _ ->
            onlyMaybeError Nothing

        Expression.ListExpr _ ->
            onlyMaybeError Nothing

        Expression.RecordExpr _ ->
            onlyMaybeError Nothing

        Expression.LambdaExpression _ ->
            onlyMaybeError Nothing

        ----------------------
        -- IMPOSSIBLE CASES --
        ----------------------
        Expression.Operator _ ->
            onlyMaybeError Nothing

        Expression.Application [] ->
            onlyMaybeError Nothing

        Expression.Application (_ :: []) ->
            onlyMaybeError Nothing


type alias OperatorApplicationCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , expectNaN : Bool
    , importLookup : ImportLookup
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , operator : String
    , operatorRange : Range
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    , isOnTheRightSideOfPlusPlus : Bool
    }


operatorApplicationChecks : Dict String (OperatorApplicationCheckInfo -> Maybe (Error {}))
operatorApplicationChecks =
    Dict.fromList
        [ ( "+", plusChecks )
        , ( "-", minusChecks )
        , ( "*", multiplyChecks )
        , ( "/", divisionChecks )
        , ( "//", intDivideChecks )
        , ( "++", plusplusChecks )
        , ( "::", consChecks )
        , ( "||", orChecks )
        , ( "&&", andChecks )
        , ( "==", equalityChecks True )
        , ( "/=", equalityChecks False )
        , ( "<", numberComparisonChecks (<) )
        , ( ">", numberComparisonChecks (>) )
        , ( "<=", numberComparisonChecks (<=) )
        , ( ">=", numberComparisonChecks (>=) )
        ]


type alias CallCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , expectNaN : Bool
    , importLookup : ImportLookup
    , extractSourceCode : Range -> String
    , commentRanges : List Range
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , fnRange : Range
    , fn : ( ModuleName, String )
    , argCount : Int
    , callStyle : FunctionCallStyle
    , firstArg : Node Expression
    , argsAfterFirst : List (Node Expression)
    , -- stored for quick access since usage is very common
      -- prefer using secondArg and thirdArg functions
      -- because the optimization could change in the future
      secondArg : Maybe (Node Expression)
    , thirdArg : Maybe (Node Expression)
    }


{-| How an argument is given as input to a function:

  - `Pipe RightToLeft`: `function <| argument`
  - `Pipe LeftToRight`: `argument |> function`
  - `Application`: `function argument`

-}
type FunctionCallStyle
    = Application
    | Pipe LeftOrRightDirection


type LeftOrRightDirection
    = RightToLeft
    | LeftToRight


secondArg : CallCheckInfo -> Maybe (Node Expression)
secondArg checkInfo =
    checkInfo.secondArg


thirdArg : CallCheckInfo -> Maybe (Node Expression)
thirdArg checkInfo =
    checkInfo.thirdArg


type alias CompositionIntoCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , extractSourceCode : Range -> String
    , later :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        , -- how many arguments a fully applied call would have
          argCount : Int
        , removeRange : Range
        }
    , earlier :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        , removeRange : Range
        }
    , isEmbeddedInComposition : Bool
    }
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
                        |> ElmSyntaxPrint.module_
                        |> ElmSyntaxPrint.toString
            in
            if printed == expected then
                Expect.pass

            else
                Expect.fail
                    ("actual printed source is\n\n"
                        ++ printed
                        ++ "\n\nbut I expected\n\n"
                        ++ expected
                        ++ "\n\nThey differ in lines\n"
                        ++ (List.map2
                                (\actualLine expectedLine -> { actual = actualLine, expected = expectedLine })
                                (printed |> String.lines)
                                (expected |> String.lines)
                                |> List.indexedMap
                                    (\i lines ->
                                        if lines.actual == lines.expected then
                                            Nothing

                                        else
                                            Just ((i |> String.fromInt) ++ ": " ++ lines.actual)
                                    )
                                |> List.filterMap identity
                                |> List.take 10
                                |> String.join "\n"
                           )
                    )


expectPrintedAsSame : String -> Expect.Expectation
expectPrintedAsSame alreadyFormattedSource =
    alreadyFormattedSource |> expectPrintedAs alreadyFormattedSource
