module PrintDefunctionalizedMicroOptimized exposing
    ( Print, toString
    , exactly, empty, linebreak
    , followedBy, listFlatten, listMapAndFlatten, listIntersperseAndFlatten, listMapAndIntersperseAndFlatten, listReverseAndIntersperseAndFlatten, listReverseAndMapAndFlatten
    , withIndentAtNextMultipleOf4, withIndentIncreasedBy, linebreakIndented, spaceOrLinebreakIndented, emptyOrLinebreakIndented
    , LineSpread(..), lineSpreadMergeWith, lineSpreadMergeWithStrict, lineSpreadListMapAndCombine, lineSpread
    , exactFollowedBy, followedByExact, listIntersperseExactAndFlatten, listMapToExactAndIntersperseAndFlatten
    )

{-| simple pretty printing

@docs Print, toString


### primitive

@docs exactly, empty, linebreak


### combine

@docs followedBy, listFlatten, listMapAndFlatten, listIntersperseAndFlatten, listMapAndIntersperseAndFlatten, listReverseAndIntersperseAndFlatten, listReverseAndMapAndFlatten


### indent

@docs withIndentAtNextMultipleOf4, withIndentIncreasedBy, linebreakIndented, spaceOrLinebreakIndented, emptyOrLinebreakIndented
@docs LineSpread, lineSpreadMergeWith, lineSpreadMergeWithStrict, lineSpreadListMapAndCombine, lineSpread

-}


{-| Like a string that knows which lines need to be indented
if the whole thing is indented.

For example, elm-format inserts full-on linebreaks inside multi-line strings, between the cases of a case-of
or let declarations of a let-in.
These should never have spaces on them no matter how many levels of indentation in
they are.

-}
type Print
    = Exact String ()
    | FollowedBy Print Print
    | Linebreak () ()
    | LinebreakIndented () ()
    | WithIndentIncreasedBy Int Print
    | WithIndentAtNextMultipleOf4 Print ()


{-| Convert to a String with no extra indentation
and no restrictions on line width
-}
toString : Print -> String
toString print =
    toStringWithIndent 0 print


toStringWithIndent : Int -> Print -> String
toStringWithIndent indent print =
    case print of
        Exact string () ->
            string

        FollowedBy b a ->
            toStringWithIndent indent a ++ toStringWithIndent indent b ++ ""

        Linebreak () () ->
            "\n"

        LinebreakIndented () () ->
            "\n" ++ String.repeat indent " "

        WithIndentIncreasedBy increase innerPrint ->
            toStringWithIndent (indent + increase)
                innerPrint

        WithIndentAtNextMultipleOf4 innerPrint () ->
            toStringWithIndent (indent // 4 * 4 + 4)
                innerPrint


{-| [How many lines](#LineSpread) the given [`Print`](#Print)
take up if turned into a string?
-}
lineSpread : Print -> LineSpread
lineSpread print =
    case print of
        Exact _ () ->
            SingleLine

        FollowedBy b a ->
            lineSpread a |> lineSpreadMergeWith (\() -> lineSpread b)

        Linebreak () () ->
            MultipleLines

        LinebreakIndented () () ->
            MultipleLines

        WithIndentIncreasedBy increase innerPrint ->
            lineSpread innerPrint

        WithIndentAtNextMultipleOf4 innerPrint () ->
            lineSpread innerPrint


{-| A given string. Mostly used for keywords, symbols and literal text.

Do not include linebreaks here and instead use [`linebreak`](#linebreak)

    Print.exactly (4 |> String.fromInt)
        |> Print.toString
    --> "4"

-}
exactly : String -> Print
exactly exactNextString =
    Exact exactNextString ()


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
    Linebreak () ()


{-| Prepend a given [`Print`](#Print)

    Print.exactly "a"
        |> Print.followedBy (Print.exactly "b")
        |> Print.toString
    --> "ab"

To append more than 2, use [`Print.listFlatten`](#listFlatten)

-}
followedBy : Print -> (Print -> Print)
followedBy =
    FollowedBy


exactFollowedBy : Print -> (String -> Print)
exactFollowedBy b aString =
    Exact aString () |> FollowedBy b


followedByExact : String -> (Print -> Print)
followedByExact bString a =
    a |> FollowedBy (Exact bString ())


{-| Concatenate a given list of [`Print`](#Print)s
one after the other after mapping each element

    [ "a", "b" ]
        |> Print.listMapAndFlatten Print.exactly
        |> Print.toString
    --> "ab"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listMapAndFlatten : (a -> Print) -> List a -> Print
listMapAndFlatten elementToPrint elements =
    elements
        |> List.foldl
            (\next soFar -> soFar |> followedBy (next |> elementToPrint))
            empty


{-| Concatenate a given list of [`Print`](#Print)s
one after the other after mapping each element

    [ "a", "b" ]
        |> Print.listReverseAndMapAndFlatten Print.exactly
        |> Print.toString
    --> "ba"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listReverseAndMapAndFlatten : (a -> Print) -> List a -> Print
listReverseAndMapAndFlatten elementToPrint elements =
    elements
        |> List.foldr
            (\next soFar -> soFar |> followedBy (elementToPrint next))
            empty


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> List.map Print.exactly
        |> Print.listIntersperseAndFlatten (Print.exactly ",")
        |> Print.toString
    --> "a,b"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listIntersperseAndFlatten : Print -> List Print -> Print
listIntersperseAndFlatten inBetweenPrint elements =
    case elements of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        soFar
                            |> followedBy inBetweenPrint
                            |> followedBy next
                    )
                    head


listIntersperseExactAndFlatten : String -> List Print -> Print
listIntersperseExactAndFlatten inBetweenString elements =
    case elements of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        soFar
                            |> followedByExact inBetweenString
                            |> followedBy next
                    )
                    head


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> Print.listMapAndIntersperseAndFlatten
            Print.exactly
            (Print.exactly ",")
        |> Print.toString
    --> "a,b"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listMapAndIntersperseAndFlatten : (a -> Print) -> Print -> List a -> Print
listMapAndIntersperseAndFlatten elementToPrint inBetweenPrint prints =
    case prints of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        soFar
                            |> followedBy inBetweenPrint
                            |> followedBy (elementToPrint next)
                    )
                    (elementToPrint head)


listMapToExactAndIntersperseAndFlatten : (a -> String) -> Print -> List a -> Print
listMapToExactAndIntersperseAndFlatten elementToPrint inBetweenPrint prints =
    case prints of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        soFar
                            |> followedBy inBetweenPrint
                            |> followedByExact (elementToPrint next)
                    )
                    (exactly (elementToPrint head))


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> List.map Print.exactly
        |> Print.listReverseAndIntersperseAndFlatten (Print.exactly ",")
        |> Print.toString
    --> "b,a"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listReverseAndIntersperseAndFlatten : Print -> List Print -> Print
listReverseAndIntersperseAndFlatten inBetweenPrint prints =
    case prints of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        next
                            |> followedBy inBetweenPrint
                            |> followedBy soFar
                    )
                    head


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> List.map Print.exactly
        |> Print.listFlatten
        |> Print.toString
    --> "ab"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listFlatten : List Print -> Print
listFlatten prints =
    case prints of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl followedBy
                    head


{-| Set the indentation used by [`Print.linebreakIndented`](#linebreakIndented),
[`Print.spaceOrLinebreakIndented`](#spaceOrLinebreakIndented)
and [`Print.emptyOrLinebreakIndented`](#emptyOrLinebreakIndented)
to the current indent + a given number.
-}
withIndentIncreasedBy : Int -> (Print -> Print)
withIndentIncreasedBy indentationIncrease print =
    WithIndentIncreasedBy indentationIncrease print


{-| Set the indentation used by [`Print.linebreakIndented`](#linebreakIndented),
[`Print.spaceOrLinebreakIndented`](#spaceOrLinebreakIndented)
and [`Print.emptyOrLinebreakIndented`](#emptyOrLinebreakIndented)
to the current indent minus its remainder by 4 + 4.
-}
withIndentAtNextMultipleOf4 : Print -> Print
withIndentAtNextMultipleOf4 print =
    WithIndentAtNextMultipleOf4 print ()


{-| All on the same line or split across multiple?
-}
type LineSpread
    = SingleLine
    | MultipleLines


{-| If either spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If both are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To merge 2 already known [`LineSpread`](#LineSpread)s, use [`Print.lineSpreadMergeWithStrict`](#lineSpreadMergeWithStrict)
To merge a list, use [`Print.lineSpreadListMapAndCombine`](#lineSpreadListMapAndCombine)

-}
lineSpreadMergeWith : (() -> LineSpread) -> LineSpread -> LineSpread
lineSpreadMergeWith bLineSpreadLazy aLineSpread =
    case aLineSpread of
        MultipleLines ->
            MultipleLines

        SingleLine ->
            bLineSpreadLazy ()


{-| If either spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If both are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To merge more a list, use [`Print.lineSpreadListMapAndCombine`](#lineSpreadListMapAndCombine)

-}
lineSpreadMergeWithStrict : LineSpread -> LineSpread -> LineSpread
lineSpreadMergeWithStrict bLineSpreadLazy aLineSpread =
    case aLineSpread of
        MultipleLines ->
            MultipleLines

        SingleLine ->
            bLineSpreadLazy


{-| If any element spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If all are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To only combine 2, use [`Print.lineSpreadMergeWith`](#lineSpreadMergeWith)

-}
lineSpreadListMapAndCombine : (a -> LineSpread) -> (List a -> LineSpread)
lineSpreadListMapAndCombine elementLineSpread lineSpreads =
    case lineSpreads of
        [] ->
            SingleLine

        head :: tail ->
            case elementLineSpread head of
                MultipleLines ->
                    MultipleLines

                SingleLine ->
                    lineSpreadListMapAndCombine elementLineSpread tail


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
    LinebreakIndented () ()