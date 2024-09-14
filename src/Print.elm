module Print exposing (LineOffset(..), Print, bumpIndentBy1, bumpIndentBy2, bumpIndentBy4, emptiableLayoutPositiveIndent, empty, followedBy, inSequence, layoutPositiveIndent, layoutTopIndent, lineOffset, lineOffsetMerge, linebreak, listCombineLineOffset, space, symbol, toString)

import List as L
    exposing
        ( filter
        , map
        )


type alias Print =
    { indent : Int
    }
    -> String


toString : Print -> String
toString print =
    print { indent = 0 }


lineOffset : Print -> LineOffset
lineOffset print =
    -- TODO can performance be improved? defunctionalization etc
    if toString print |> String.contains "\n" then
        NextLine

    else
        SameLine


symbol : String -> Print
symbol exactNextString =
    \_ -> exactNextString


empty : Print
empty =
    symbol ""


space : Print
space =
    symbol " "


linebreak : Print
linebreak =
    symbol "\n"


followedBy : Print -> (Print -> Print)
followedBy nextPrint soFarPrint =
    \indent -> soFarPrint indent ++ nextPrint indent


inSequence : List Print -> Print
inSequence printSequence =
    case printSequence of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl (\next soFar -> soFar |> followedBy next)
                    head


bumpIndentBy4 : Print -> Print
bumpIndentBy4 print =
    \soFarState -> print { indent = soFarState.indent + 4 }


bumpIndentBy2 : Print -> Print
bumpIndentBy2 print =
    \soFarState -> print { indent = soFarState.indent + 2 }


bumpIndentBy1 : Print -> Print
bumpIndentBy1 print =
    \soFarState -> print { indent = soFarState.indent + 1 }


type LineOffset
    = SameLine
    | NextLine


lineOffsetMerge : LineOffset -> LineOffset -> LineOffset
lineOffsetMerge aLineOffset bLineOffset =
    case aLineOffset of
        NextLine ->
            NextLine

        SameLine ->
            bLineOffset


listCombineLineOffset : List LineOffset -> LineOffset
listCombineLineOffset lineOffsets =
    lineOffsets |> List.foldl lineOffsetMerge SameLine


layoutTopIndent : LineOffset -> Print
layoutTopIndent lineOffsetToUse =
    case lineOffsetToUse of
        SameLine ->
            space

        NextLine ->
            \state -> "\n" ++ String.repeat state.indent " "


layoutPositiveIndent : LineOffset -> Print
layoutPositiveIndent lineOffsetToUse =
    case lineOffsetToUse of
        SameLine ->
            space

        NextLine ->
            \state -> "\n" ++ String.repeat (nextMultipleOf4 state.indent) " "


emptiableLayoutPositiveIndent : LineOffset -> Print
emptiableLayoutPositiveIndent lineOffsetToUse =
    case lineOffsetToUse of
        SameLine ->
            empty

        NextLine ->
            \state -> "\n" ++ String.repeat (nextMultipleOf4 state.indent) " "


nextMultipleOf4 : Int -> Int
nextMultipleOf4 n =
    (n // 4) * 4 + 4
