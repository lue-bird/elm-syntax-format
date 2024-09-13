module Print exposing (LineOffset(..), Print, bumpIndentBy1, bumpIndentBy4, char, emptiableLayoutPositiveIndent, empty, followedBy, inSequence, layoutModuleLevelIndent, layoutPositiveIndent, layoutTopIndent, lineOffsetMerge, linebreak, listCombineLineOffset, space, symbol, toString)

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


symbol : String -> Print
symbol exactNextString =
    \_ -> exactNextString


empty : Print
empty =
    symbol ""


char : Char -> Print
char c =
    symbol (String.fromChar c)


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


layoutModuleLevelIndent : LineOffset -> Print
layoutModuleLevelIndent lineOffset =
    case lineOffset of
        SameLine ->
            space

        NextLine ->
            linebreak


layoutTopIndent : LineOffset -> Print
layoutTopIndent lineOffset =
    case lineOffset of
        SameLine ->
            space

        NextLine ->
            \state -> "\n" ++ String.repeat state.indent " "


layoutPositiveIndent : LineOffset -> Print
layoutPositiveIndent lineOffset =
    case lineOffset of
        SameLine ->
            space

        NextLine ->
            \state -> "\n" ++ String.repeat (nextMultipleOf4 state.indent) " "


emptiableLayoutPositiveIndent : LineOffset -> Print
emptiableLayoutPositiveIndent lineOffset =
    case lineOffset of
        SameLine ->
            empty

        NextLine ->
            \state -> "\n" ++ String.repeat (nextMultipleOf4 state.indent) " "


nextMultipleOf4 : Int -> Int
nextMultipleOf4 n =
    (n // 4) * 4 + 4
