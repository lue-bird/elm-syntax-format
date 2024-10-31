module Print exposing
    ( Print, toString
    , exactly, empty, space, linebreak
    , followedBy, sequence
    , withIndentAtNextMultipleOf4, withIndentIncreasedBy, linebreakIndented, spaceOrLinebreakIndented, emptyOrLinebreakIndented
    , LineSpread(..), lineSpreadMerge, lineSpreadsCombine, lineSpread
    )

{-| simple pretty printing

@docs Print, toString


### primitive

@docs exactly, empty, space, linebreak


### combine

@docs followedBy, sequence


### indent

@docs withIndentAtNextMultipleOf4, withIndentIncreasedBy, linebreakIndented, spaceOrLinebreakIndented, emptyOrLinebreakIndented
@docs LineSpread, lineSpreadMerge, lineSpreadsCombine, lineSpread

-}


{-| Like a string that knows which lines need to be indented
if the whole thing is indented.

For example, elm-format inserts full-on linebreaks inside multi-line strings, between the cases of a case-of
or let declarations of a let-in.
These should never have spaces on them no matter how many levels of indentation in
they are.

-}
type alias Print =
    { indent : Int } -> String


{-| Convert to a String with no extra indentation
and no restrictions on line width
-}
toString : Print -> String
toString print =
    print { indent = 0 }


{-| [How many lines](#LineSpread) the given [`Print`](#Print)
take up if turned into a string?
-}
lineSpread : Print -> LineSpread
lineSpread print =
    if toString print |> String.contains "\n" then
        MultipleLines

    else
        SingleLine


{-| A given string. Mostly used for keywords, symbols and literal text.

Do not include linebreaks here and instead use [`linebreak`](#linebreak)

    Print.exactly (4 |> String.fromInt)
        |> Print.toString
    --> "4"

-}
exactly : String -> Print
exactly exactNextString =
    \_ -> exactNextString


{-| `exactly ""`.
Useful when you want to conditionally append something

    printSign : Sign -> Print
    printSign =
        case sign of
            Sign.Negative ->
                Print.exactly "-"

            Sign.Positive ->
                Print.empty

-}
empty : Print
empty =
    exactly ""


{-| `exactly " "`.
Often used after opening brackets
and followed by [`Print.withIndentIncreasedBy 2`](#withIndentIncreasedBy)
-}
space : Print
space =
    exactly " "


{-| Empty line (\\n).
Usually followed by [`Print.linebreakIndented`](#linebreakIndented)
-}
linebreak : Print
linebreak =
    exactly "\n"


{-| Prepend a given [`Print`](#Print)

    Print.exactly "a"
        |> Print.followedBy (Print.exactly "b")
        |> Print.toString
    --> "ab"

To append more than 2, use [`Print.sequence`](#sequence)

-}
followedBy : Print -> (Print -> Print)
followedBy nextPrint soFarPrint =
    \indent -> soFarPrint indent ++ nextPrint indent


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> List.map Print.exactly
        |> Print.sequence
        |> Print.toString
    --> "ab"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
sequence : List Print -> Print
sequence printSequence =
    case printSequence of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl (\next soFar -> soFar |> followedBy next)
                    head


{-| Set the indentation used by [`Print.linebreakIndented`](#linebreakIndented),
[`Print.spaceOrLinebreakIndented`](#spaceOrLinebreakIndented)
and [`Print.emptyOrLinebreakIndented`](#emptyOrLinebreakIndented)
to the current indent + a given number.
-}
withIndentIncreasedBy : Int -> (Print -> Print)
withIndentIncreasedBy indentationIncrease print =
    \soFarState -> print { indent = soFarState.indent + indentationIncrease }


{-| Set the indentation used by [`Print.linebreakIndented`](#linebreakIndented),
[`Print.spaceOrLinebreakIndented`](#spaceOrLinebreakIndented)
and [`Print.emptyOrLinebreakIndented`](#emptyOrLinebreakIndented)
to the current indent minus its remainder by 4 + 4.
-}
withIndentAtNextMultipleOf4 : Print -> Print
withIndentAtNextMultipleOf4 print =
    \soFarState -> print { indent = soFarState.indent // 4 * 4 + 4 }


{-| All on the same line or split across multiple?
-}
type LineSpread
    = SingleLine
    | MultipleLines


{-| If either spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If both are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To merge more than 2, use [`Print.lineSpreadsCombine`](#lineSpreadsCombine)

-}
lineSpreadMerge : LineSpread -> LineSpread -> LineSpread
lineSpreadMerge aLineSpread bLineSpread =
    case aLineSpread of
        MultipleLines ->
            MultipleLines

        SingleLine ->
            bLineSpread


{-| If any spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If all are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To only combine 2, use [`Print.lineSpreadsCombine`](#lineSpreadsCombine)

-}
lineSpreadsCombine : List LineSpread -> LineSpread
lineSpreadsCombine lineSpreads =
    case lineSpreads of
        [] ->
            SingleLine

        head :: tail ->
            case head of
                MultipleLines ->
                    MultipleLines

                SingleLine ->
                    lineSpreadsCombine tail


{-| [`Print.space`](#space) when [`SingleLine`](#LineSpread),
[`Print.linebreakIndented`](#linebreakIndented) when [`MultipleLines`](#LineSpread),

Specify the indentation with
[`Print.withIndentIncreasedBy`](#withIndentIncreasedBy)
and [`withIndentAtNextMultipleOf4`](#withIndentAtNextMultipleOf4)

-}
spaceOrLinebreakIndented : LineSpread -> Print
spaceOrLinebreakIndented lineSpreadToUse =
    case lineSpreadToUse of
        SingleLine ->
            space

        MultipleLines ->
            linebreakIndented


{-| [`Print.empty`](#empty) when [`SingleLine`](#LineSpread),
[`Print.linebreakIndented`](#linebreakIndented) when [`MultipleLines`](#LineSpread),

Specify the indentation with
[`Print.withIndentIncreasedBy`](#withIndentIncreasedBy)
and [`withIndentAtNextMultipleOf4`](#withIndentAtNextMultipleOf4)

-}
emptyOrLinebreakIndented : LineSpread -> Print
emptyOrLinebreakIndented lineSpreadToUse =
    case lineSpreadToUse of
        SingleLine ->
            empty

        MultipleLines ->
            linebreakIndented


{-| Linebreak followed by spaces to the current indentation level.

Specify the indentation with
[`Print.withIndentIncreasedBy`](#withIndentIncreasedBy)
and [`withIndentAtNextMultipleOf4`](#withIndentAtNextMultipleOf4)

-}
linebreakIndented : Print
linebreakIndented =
    \state -> "\n" ++ String.repeat state.indent " "
