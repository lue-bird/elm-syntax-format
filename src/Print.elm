module Print exposing (LineOffset(..), Print, emptiableLayout, empty, followedBy, inSequence, indented, layout, lineOffset, lineOffsetMerge, linebreak, listCombineLineOffset, space, symbol, toString)


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


indented : Int -> Print -> Print
indented indentationIncrease print =
    \soFarState -> print { indent = soFarState.indent + indentationIncrease }


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


layout : LineOffset -> Print
layout lineOffsetToUse =
    case lineOffsetToUse of
        SameLine ->
            space

        NextLine ->
            \state -> "\n" ++ String.repeat state.indent " "


emptiableLayout : LineOffset -> Print
emptiableLayout lineOffsetToUse =
    case lineOffsetToUse of
        SameLine ->
            empty

        NextLine ->
            \state -> "\n" ++ String.repeat state.indent " "
