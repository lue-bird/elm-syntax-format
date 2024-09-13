module ElmSyntaxFormat exposing
    ( module_, moduleName, exposing_, expose, imports, import_
    , patternNotParenthesized, patternParenthesizedIfSpaceSeparated, typeNotParenthesized
    )

{-| Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

@docs module_, moduleName, exposing_, expose, imports, import_
@docs expression, patternNotParenthesized, patternParenthesizedIfSpaceSeparated, typeNotParenthesized

-}

import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Print exposing (LineOffset, Print)


module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    -- TODO module documentation
    -- TODO declarations
    -- TODO comments
    syntaxModule.moduleDefinition
        |> Elm.Syntax.Node.value
        |> moduleHeader
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (syntaxModule.imports
                |> List.map (\(Elm.Syntax.Node.Node _ syntaxImport) -> syntaxImport)
                |> imports
            )


moduleHeader : Elm.Syntax.Module.Module -> Print
moduleHeader syntaxModuleHeader =
    case syntaxModuleHeader of
        Elm.Syntax.Module.NormalModule defaultModuleData ->
            let
                lineOffset : LineOffset
                lineOffset =
                    defaultModuleData.exposingList |> exposingLineOffset
            in
            Print.symbol "module"
                |> Print.followedBy Print.space
                |> Print.followedBy (moduleName (defaultModuleData.moduleName |> Elm.Syntax.Node.value))
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "exposing")
                |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                |> Print.followedBy (defaultModuleData.exposingList |> exposing_ lineOffset)

        Elm.Syntax.Module.PortModule defaultModuleData ->
            let
                lineOffset : LineOffset
                lineOffset =
                    defaultModuleData.exposingList |> exposingLineOffset
            in
            Print.symbol "port"
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "module")
                |> Print.followedBy Print.space
                |> Print.followedBy (moduleName (defaultModuleData.moduleName |> Elm.Syntax.Node.value))
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "exposing")
                |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                |> Print.followedBy (defaultModuleData.exposingList |> exposing_ lineOffset)

        Elm.Syntax.Module.EffectModule effectModuleData ->
            let
                lineOffset : LineOffset
                lineOffset =
                    effectModuleData.exposingList |> exposingLineOffset
            in
            Print.symbol "effect"
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "module")
                |> Print.followedBy Print.space
                |> Print.followedBy (moduleName (effectModuleData.moduleName |> Elm.Syntax.Node.value))
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "where")
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "{")
                |> Print.followedBy Print.space
                |> Print.followedBy
                    (Print.inSequence
                        ([ case effectModuleData.command of
                            Nothing ->
                                Nothing

                            Just (Elm.Syntax.Node.Node _ name) ->
                                Just
                                    (Print.symbol "command"
                                        |> Print.followedBy Print.space
                                        |> Print.followedBy (Print.symbol "=")
                                        |> Print.followedBy Print.space
                                        |> Print.followedBy (Print.symbol name)
                                    )
                         , case effectModuleData.subscription of
                            Nothing ->
                                Nothing

                            Just (Elm.Syntax.Node.Node _ name) ->
                                Just
                                    (Print.symbol "subscription"
                                        |> Print.followedBy Print.space
                                        |> Print.followedBy (Print.symbol "=")
                                        |> Print.followedBy Print.space
                                        |> Print.followedBy (Print.symbol name)
                                    )
                         ]
                            |> List.filterMap identity
                            |> List.intersperse
                                (Print.symbol ","
                                    |> Print.followedBy Print.space
                                )
                        )
                    )
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "}")
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "exposing")
                |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                |> Print.followedBy (effectModuleData.exposingList |> exposing_ lineOffset)


imports : List Elm.Syntax.Import.Import -> Print
imports syntaxImports =
    -- TODO group according to doc tags
    -- TODO check how overlaps with default imports are handled in elm-format
    Print.inSequence
        (syntaxImports
            |> List.sortWith
                (\a b ->
                    compare (a.moduleName |> Elm.Syntax.Node.value) (b.moduleName |> Elm.Syntax.Node.value)
                )
            |> importsCombine
            |> List.map (\syntaxImport -> import_ syntaxImport)
            |> List.intersperse Print.linebreak
        )


exposingLineOffset : Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing -> LineOffset
exposingLineOffset syntaxExposing =
    case syntaxExposing of
        Elm.Syntax.Node.Node _ (Elm.Syntax.Exposing.All _) ->
            Print.SameLine

        Elm.Syntax.Node.Node exposingRange (Elm.Syntax.Exposing.Explicit exposingSet) ->
            case exposingSet of
                [] ->
                    Print.SameLine

                [ _ ] ->
                    Print.SameLine

                _ :: _ :: _ ->
                    lineOffsetInRange exposingRange


import_ : Elm.Syntax.Import.Import -> Print
import_ syntaxImport =
    let
        lineOffset : Print.LineOffset
        lineOffset =
            case syntaxImport.exposingList of
                Nothing ->
                    Print.SameLine

                Just syntaxExposing ->
                    syntaxExposing |> exposingLineOffset
    in
    Print.symbol "import"
        |> Print.followedBy Print.space
        |> Print.followedBy (moduleName (Elm.Syntax.Node.value syntaxImport.moduleName))
        |> Print.followedBy
            (case syntaxImport.moduleAlias of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ moduleAlias) ->
                    Print.space
                        |> Print.followedBy (Print.symbol "as")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (moduleName moduleAlias)
            )
        |> Print.followedBy
            (case syntaxImport.exposingList of
                Nothing ->
                    Print.empty

                Just syntaxExposing ->
                    Print.layoutPositiveIndent lineOffset
                        |> Print.followedBy (Print.symbol "exposing")
                        |> Print.followedBy
                            (Print.bumpIndentBy4
                                (Print.layoutPositiveIndent lineOffset
                                    |> Print.followedBy (exposing_ lineOffset syntaxExposing)
                                )
                            )
            )


importsCombine :
    List Elm.Syntax.Import.Import
    -> List Elm.Syntax.Import.Import
importsCombine syntaxImports =
    case syntaxImports of
        [] ->
            []

        [ onlyImport ] ->
            [ onlyImport |> importToNormal ]

        import0 :: import1 :: import2Up ->
            if (import0.moduleName |> Elm.Syntax.Node.value) == (import1.moduleName |> Elm.Syntax.Node.value) then
                importsCombine (importsMerge import0 import1 :: import2Up)

            else
                (import0 |> importToNormal) :: importsCombine (import1 :: import2Up)


importToNormal : Elm.Syntax.Import.Import -> Elm.Syntax.Import.Import
importToNormal syntaxImport =
    { moduleName = syntaxImport.moduleName
    , moduleAlias = syntaxImport.moduleAlias
    , exposingList =
        syntaxImport.exposingList |> exposingToNormal
    }


exposeListToNormal :
    List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
exposeListToNormal syntaxExposeList =
    syntaxExposeList
        |> List.map Elm.Syntax.Node.value
        |> List.sortWith exposeCompare
        |> exposesCombine
        |> List.map Elm.Syntax.Node.empty


exposingToNormal :
    Maybe (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing)
    -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing)
exposingToNormal syntaxExposing =
    case syntaxExposing of
        Just (Elm.Syntax.Node.Node exposingAllRange (Elm.Syntax.Exposing.All allRange)) ->
            Just (Elm.Syntax.Node.Node exposingAllRange (Elm.Syntax.Exposing.All allRange))

        Just (Elm.Syntax.Node.Node exposingExplicitRange (Elm.Syntax.Exposing.Explicit exposeSet)) ->
            Just
                (Elm.Syntax.Node.Node
                    exposingExplicitRange
                    (Elm.Syntax.Exposing.Explicit (exposeSet |> exposeListToNormal))
                )

        Nothing ->
            Nothing


importsMerge : Elm.Syntax.Import.Import -> Elm.Syntax.Import.Import -> Elm.Syntax.Import.Import
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , moduleAlias =
        case earlier.moduleAlias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.moduleAlias
    , exposingList =
        exposingCombine earlier.exposingList later.exposingList
    }


exposingCombine :
    Maybe (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing)
    -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing)
    -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing)
exposingCombine a b =
    case a of
        Just (Elm.Syntax.Node.Node exposingAllRange (Elm.Syntax.Exposing.All allRange)) ->
            Just (Elm.Syntax.Node.Node exposingAllRange (Elm.Syntax.Exposing.All allRange))

        Just (Elm.Syntax.Node.Node earlierExposingExplicitRange (Elm.Syntax.Exposing.Explicit earlierExposeSet)) ->
            Just
                (case b of
                    Just (Elm.Syntax.Node.Node exposingAllRange (Elm.Syntax.Exposing.All allRange)) ->
                        Elm.Syntax.Node.Node exposingAllRange (Elm.Syntax.Exposing.All allRange)

                    Just (Elm.Syntax.Node.Node laterExposingExplicitRange (Elm.Syntax.Exposing.Explicit laterExposeSet)) ->
                        Elm.Syntax.Node.Node
                            (case lineOffsetInRange earlierExposingExplicitRange of
                                Print.NextLine ->
                                    earlierExposingExplicitRange

                                Print.SameLine ->
                                    laterExposingExplicitRange
                            )
                            (Elm.Syntax.Exposing.Explicit
                                (earlierExposeSet ++ laterExposeSet |> exposeListToNormal)
                            )

                    Nothing ->
                        Elm.Syntax.Node.Node earlierExposingExplicitRange (Elm.Syntax.Exposing.Explicit earlierExposeSet)
                )

        Nothing ->
            b


lineOffsetBetweenNodes : Elm.Syntax.Node.Node a -> Elm.Syntax.Node.Node b -> Print.LineOffset
lineOffsetBetweenNodes (Elm.Syntax.Node.Node earlierRange _) (Elm.Syntax.Node.Node laterRange _) =
    if earlierRange.end.row == laterRange.start.row then
        Print.SameLine

    else
        Print.NextLine


lineOffsetInNode : Elm.Syntax.Node.Node a -> Print.LineOffset
lineOffsetInNode (Elm.Syntax.Node.Node range _) =
    lineOffsetInRange range


lineOffsetInRange : Elm.Syntax.Range.Range -> Print.LineOffset
lineOffsetInRange range =
    if range.start.row == range.end.row then
        Print.SameLine

    else
        Print.NextLine


lineOffsetBetweenNodeList : Elm.Syntax.Node.Node a -> Elm.Syntax.Node.Node b -> Print.LineOffset
lineOffsetBetweenNodeList (Elm.Syntax.Node.Node earlierRange _) (Elm.Syntax.Node.Node laterRange _) =
    if earlierRange.end.row == laterRange.start.row then
        Print.SameLine

    else
        Print.NextLine


exposing_ : LineOffset -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing -> Print
exposing_ lineOffset (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
    Print.symbol "("
        |> Print.followedBy
            (case syntaxExposing of
                Elm.Syntax.Exposing.All _ ->
                    Print.symbol ".."

                Elm.Syntax.Exposing.Explicit exposingSet ->
                    case exposingSet of
                        [] ->
                            Print.empty

                        [ Elm.Syntax.Node.Node _ onlySyntaxExpose ] ->
                            expose onlySyntaxExpose

                        _ :: _ :: _ ->
                            (case lineOffset of
                                Print.SameLine ->
                                    Print.empty

                                Print.NextLine ->
                                    Print.space
                            )
                                |> Print.followedBy
                                    (commaSeparated
                                        lineOffset
                                        (exposingSet
                                            |> List.sortWith
                                                (\(Elm.Syntax.Node.Node _ a) (Elm.Syntax.Node.Node _ b) -> exposeCompare a b)
                                            |> List.map
                                                (\(Elm.Syntax.Node.Node _ syntaxExpose) -> expose syntaxExpose)
                                        )
                                    )
            )
        |> Print.followedBy (Print.symbol ")")


commaSeparated : Print.LineOffset -> List Print -> Print
commaSeparated lineOffset elements =
    Print.inSequence
        (elements
            |> List.map
                (\elementPrint ->
                    elementPrint
                        |> Print.followedBy
                            (Print.emptiableLayoutPositiveIndent lineOffset)
                )
            |> List.intersperse
                (Print.symbol ","
                    |> Print.followedBy Print.space
                )
        )


moduleName : Elm.Syntax.ModuleName.ModuleName -> Print
moduleName syntaxModuleName =
    Print.symbol (syntaxModuleName |> String.join ".")


exposeCompare : Elm.Syntax.Exposing.TopLevelExpose -> Elm.Syntax.Exposing.TopLevelExpose -> Basics.Order
exposeCompare a b =
    case a of
        Elm.Syntax.Exposing.InfixExpose aOperatorSymbol ->
            case b of
                Elm.Syntax.Exposing.InfixExpose bOperatorSymbol ->
                    compare aOperatorSymbol bOperatorSymbol

                Elm.Syntax.Exposing.FunctionExpose _ ->
                    LT

                Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
                    LT

                Elm.Syntax.Exposing.TypeExpose _ ->
                    LT

        Elm.Syntax.Exposing.FunctionExpose aName ->
            case b of
                Elm.Syntax.Exposing.InfixExpose _ ->
                    GT

                Elm.Syntax.Exposing.FunctionExpose bName ->
                    compare aName bName

                Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
                    GT

                Elm.Syntax.Exposing.TypeExpose _ ->
                    GT

        Elm.Syntax.Exposing.TypeOrAliasExpose aName ->
            case b of
                Elm.Syntax.Exposing.InfixExpose _ ->
                    GT

                Elm.Syntax.Exposing.FunctionExpose _ ->
                    LT

                Elm.Syntax.Exposing.TypeOrAliasExpose bName ->
                    compare aName bName

                Elm.Syntax.Exposing.TypeExpose bTypeExpose ->
                    compare aName bTypeExpose.name

        Elm.Syntax.Exposing.TypeExpose aTypeExpose ->
            case b of
                Elm.Syntax.Exposing.InfixExpose _ ->
                    GT

                Elm.Syntax.Exposing.FunctionExpose _ ->
                    LT

                Elm.Syntax.Exposing.TypeOrAliasExpose bName ->
                    compare aTypeExpose.name bName

                Elm.Syntax.Exposing.TypeExpose bTypeExpose ->
                    compare aTypeExpose.name bTypeExpose.name


exposesCombine : List Elm.Syntax.Exposing.TopLevelExpose -> List Elm.Syntax.Exposing.TopLevelExpose
exposesCombine syntaxExposes =
    case syntaxExposes of
        [] ->
            []

        [ onlyExpose ] ->
            [ onlyExpose ]

        expose0 :: expose1 :: expose2Up ->
            case exposeCompare expose0 expose1 of
                EQ ->
                    exposesCombine
                        (exposeMerge expose0 expose1 :: expose2Up)

                -- LT | GT
                _ ->
                    expose0 :: exposesCombine (expose1 :: expose2Up)


exposeMerge : Elm.Syntax.Exposing.TopLevelExpose -> Elm.Syntax.Exposing.TopLevelExpose -> Elm.Syntax.Exposing.TopLevelExpose
exposeMerge a b =
    case a of
        Elm.Syntax.Exposing.TypeExpose aTypeExpose ->
            case b of
                Elm.Syntax.Exposing.TypeExpose bTypeExpose ->
                    Elm.Syntax.Exposing.TypeExpose
                        { name = aTypeExpose.name
                        , open =
                            case aTypeExpose.open of
                                Just openRange ->
                                    Just openRange

                                Nothing ->
                                    bTypeExpose.open
                        }

                _ ->
                    a

        _ ->
            b


expose : Elm.Syntax.Exposing.TopLevelExpose -> Print
expose syntaxExpose =
    case syntaxExpose of
        Elm.Syntax.Exposing.InfixExpose operatorSymbol ->
            Print.symbol "("
                |> Print.followedBy (Print.symbol operatorSymbol)
                |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Exposing.FunctionExpose name ->
            Print.symbol name

        Elm.Syntax.Exposing.TypeOrAliasExpose name ->
            Print.symbol name

        Elm.Syntax.Exposing.TypeExpose syntaxExposeType ->
            case syntaxExposeType.open of
                Nothing ->
                    Print.symbol syntaxExposeType.name

                Just _ ->
                    Print.symbol syntaxExposeType.name
                        |> Print.followedBy (Print.symbol "(..)")


intToHexString : Int -> String
intToHexString int =
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "a"

        11 ->
            "b"

        12 ->
            "c"

        13 ->
            "d"

        14 ->
            "e"

        -- 15
        _ ->
            "f"


patternIsSpaceSeparated : Elm.Syntax.Pattern.Pattern -> Bool
patternIsSpaceSeparated syntaxPattern =
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            False

        Elm.Syntax.Pattern.UnitPattern ->
            False

        Elm.Syntax.Pattern.VarPattern _ ->
            False

        Elm.Syntax.Pattern.CharPattern _ ->
            False

        Elm.Syntax.Pattern.StringPattern _ ->
            False

        Elm.Syntax.Pattern.IntPattern _ ->
            False

        Elm.Syntax.Pattern.HexPattern _ ->
            False

        Elm.Syntax.Pattern.FloatPattern _ ->
            -- invalid syntax
            False

        Elm.Syntax.Pattern.ParenthesizedPattern (Elm.Syntax.Node.Node _ inParens) ->
            patternIsSpaceSeparated inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                [] ->
                    -- should be covered by UnitPattern
                    False

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    patternIsSpaceSeparated inParens

                part0 :: part1 :: part2 :: part3Up ->
                    -- invalid syntax
                    False

        Elm.Syntax.Pattern.RecordPattern fields ->
            False

        Elm.Syntax.Pattern.UnConsPattern _ _ ->
            True

        Elm.Syntax.Pattern.ListPattern _ ->
            False

        Elm.Syntax.Pattern.NamedPattern _ argumentPatterns ->
            case argumentPatterns of
                [] ->
                    False

                _ :: _ ->
                    True

        Elm.Syntax.Pattern.AsPattern _ _ ->
            True


patternParenthesizedIfSpaceSeparated : Elm.Syntax.Pattern.Pattern -> Print
patternParenthesizedIfSpaceSeparated syntaxPattern =
    if patternIsSpaceSeparated syntaxPattern then
        Print.symbol "("
            |> Print.followedBy (patternNotParenthesized syntaxPattern)
            |> Print.followedBy (Print.symbol ")")

    else
        patternNotParenthesized syntaxPattern


patternNotParenthesized : Elm.Syntax.Pattern.Pattern -> Print
patternNotParenthesized syntaxPattern =
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Print.symbol "_"

        Elm.Syntax.Pattern.UnitPattern ->
            Print.symbol "()"

        Elm.Syntax.Pattern.VarPattern name ->
            Print.symbol name

        Elm.Syntax.Pattern.CharPattern char ->
            -- TODO escape, linebreak, tab, '
            Print.symbol "'"
                |> Print.followedBy (Print.char char)
                |> Print.followedBy (Print.symbol "'")

        Elm.Syntax.Pattern.StringPattern string ->
            -- TODO escape, linebreak, tab, "
            Print.symbol "\""
                |> Print.followedBy (Print.symbol string)
                |> Print.followedBy (Print.symbol "\"")

        Elm.Syntax.Pattern.IntPattern int ->
            -- TODO cap out
            Print.symbol (intToHexString int)

        Elm.Syntax.Pattern.HexPattern int ->
            -- TODO cap out
            Print.symbol (String.fromInt int)

        Elm.Syntax.Pattern.FloatPattern float ->
            -- invalid syntax
            Print.symbol (String.fromFloat float)

        Elm.Syntax.Pattern.ParenthesizedPattern (Elm.Syntax.Node.Node _ inParens) ->
            patternNotParenthesized inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ Elm.Syntax.Node.Node _ part0, Elm.Syntax.Node.Node _ part1 ] ->
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (patternNotParenthesized part0)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (patternNotParenthesized part1)
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.symbol ")")

                [ Elm.Syntax.Node.Node _ part0, Elm.Syntax.Node.Node _ part1, Elm.Syntax.Node.Node _ part2 ] ->
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (patternNotParenthesized part0)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (patternNotParenthesized part1)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (patternNotParenthesized part2)
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.symbol ")")

                [] ->
                    -- should be covered by UnitPattern
                    Print.symbol "()"

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    patternNotParenthesized inParens

                part0 :: part1 :: part2 :: part3Up ->
                    -- invalid syntax
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated Print.SameLine
                                ((part0 :: part1 :: part2 :: part3Up)
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ partPattern) ->
                                            patternNotParenthesized partPattern
                                        )
                                )
                            )
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Pattern.RecordPattern fields ->
            Print.symbol "{"
                |> Print.followedBy Print.space
                |> Print.followedBy
                    (commaSeparated Print.SameLine
                        (fields |> List.map (\(Elm.Syntax.Node.Node _ fieldName) -> Print.symbol fieldName))
                    )
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node _ headPattern) (Elm.Syntax.Node.Node _ tailPattern) ->
            patternParenthesizedIfSpaceSeparated headPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "::")
                |> Print.followedBy Print.space
                |> Print.followedBy (patternParenthesizedIfSpaceSeparated tailPattern)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Print.symbol "["
                |> Print.followedBy Print.space
                |> Print.followedBy
                    (commaSeparated Print.SameLine
                        (elementPatterns
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ elementPattern) ->
                                    patternNotParenthesized elementPattern
                                )
                        )
                    )
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "]")

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            qualifiedNameRef syntaxQualifiedNameRef
                |> Print.followedBy
                    (Print.inSequence
                        (argumentPatterns
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ argumentPattern) ->
                                    Print.space
                                        |> Print.followedBy
                                            (patternParenthesizedIfSpaceSeparated argumentPattern)
                                )
                        )
                    )

        Elm.Syntax.Pattern.AsPattern (Elm.Syntax.Node.Node _ aliasedPattern) (Elm.Syntax.Node.Node _ aliasName) ->
            patternParenthesizedIfSpaceSeparated aliasedPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "as")
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol aliasName)


qualifiedNameRef : Elm.Syntax.Pattern.QualifiedNameRef -> Print
qualifiedNameRef syntaxQualifiedNameRef =
    case syntaxQualifiedNameRef.moduleName of
        [] ->
            Print.symbol syntaxQualifiedNameRef.name

        modulePartHead :: modulePartTail ->
            Print.symbol modulePartHead
                |> Print.followedBy (Print.symbol ".")
                |> Print.followedBy
                    (Print.inSequence
                        (modulePartTail
                            |> List.map Print.symbol
                            |> List.intersperse (Print.symbol ".")
                        )
                    )


qualifiedTuple : ( Elm.Syntax.ModuleName.ModuleName, String ) -> Print
qualifiedTuple ( qualification, unqualified ) =
    qualifiedNameRef { moduleName = qualification, name = unqualified }


typeIsSpaceSeparated : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Bool
typeIsSpaceSeparated syntaxType =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.GenericType _ ->
            False

        Elm.Syntax.TypeAnnotation.Typed _ arguments ->
            case arguments of
                [] ->
                    False

                _ :: _ ->
                    True

        Elm.Syntax.TypeAnnotation.Unit ->
            False

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    False

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    typeIsSpaceSeparated inParens

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    False

        Elm.Syntax.TypeAnnotation.Record _ ->
            False

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            False

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ _ ->
            True


typeParenthesizedIfSpaceSeparated : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Print
typeParenthesizedIfSpaceSeparated typeNode =
    if typeIsSpaceSeparated (typeNode |> Elm.Syntax.Node.value) then
        let
            lineOffset : LineOffset
            lineOffset =
                lineOffsetInRange (typeNode |> Elm.Syntax.Node.range)
        in
        Print.symbol "("
            |> Print.followedBy (Print.bumpIndentBy1 (typeNotParenthesized typeNode))
            |> Print.followedBy (Print.emptiableLayoutPositiveIndent lineOffset)
            |> Print.followedBy (Print.symbol ")")

    else
        typeNotParenthesized typeNode


typeParenthesizedIfFunction : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Print
typeParenthesizedIfFunction typeNode =
    case typeNode |> Elm.Syntax.Node.value of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Print.symbol "("
                |> Print.followedBy
                    (Print.bumpIndentBy1
                        (typeFunctionNotParenthesized inType outType)
                    )
                |> Print.followedBy
                    (Print.emptiableLayoutPositiveIndent
                        (lineOffsetBetweenNodes inType outType)
                    )
                |> Print.followedBy (Print.symbol ")")

        _ ->
            typeNotParenthesized typeNode


typeFunctionNotParenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeFunctionNotParenthesized inType outType =
    typeParenthesizedIfFunction inType
        |> Print.followedBy
            (Print.layoutPositiveIndent
                (lineOffsetBetweenNodes inType outType)
            )
        |> Print.followedBy (Print.symbol "->")
        |> Print.followedBy (Print.layoutPositiveIndent (lineOffsetInNode outType))
        |> Print.followedBy
            (Print.bumpIndentBy4 (typeNotParenthesized outType))


typeIsFunction : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Bool
typeIsFunction syntaxType =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Tupled [ Elm.Syntax.Node.Node _ inParens ] ->
            typeIsFunction inParens

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ _ ->
            True

        _ ->
            False


typeParenthesizedIfParenthesizedFunction : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Print
typeParenthesizedIfParenthesizedFunction syntaxType =
    case syntaxType of
        Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ inParens ]) ->
            if typeIsFunction (inParens |> Elm.Syntax.Node.value) then
                Print.symbol "("
                    |> Print.followedBy
                        (Print.bumpIndentBy1
                            (typeNotParenthesized inParens)
                        )
                    |> Print.followedBy
                        (Print.emptiableLayoutPositiveIndent
                            (lineOffsetInNode inParens)
                        )
                    |> Print.followedBy (Print.symbol ")")

            else
                typeNotParenthesized inParens

        Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType) ->
            typeFunctionNotParenthesized inType outType

        otherType ->
            typeNotParenthesized otherType


typeNotParenthesized : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Print
typeNotParenthesized (Elm.Syntax.Node.Node typeRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Print.symbol "()"

        Elm.Syntax.TypeAnnotation.GenericType name ->
            Print.symbol name

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ syntaxQualifiedTuple) arguments ->
            qualifiedTuple syntaxQualifiedTuple
                |> Print.followedBy
                    (Print.inSequence
                        (arguments
                            |> List.map
                                (\argument -> typeParenthesizedIfSpaceSeparated argument)
                        )
                    )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Print.symbol "()"

                [ inParens ] ->
                    typeNotParenthesized inParens

                [ part0, part1 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange typeRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (typeNotParenthesized part0)
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (typeNotParenthesized part1)
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                [ part0, part1, part2 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange typeRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (typeNotParenthesized part0)
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (typeNotParenthesized part1)
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (typeNotParenthesized part2)
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange typeRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated lineOffset
                                ((part0 :: part1 :: part2 :: part3 :: part4Up)
                                    |> List.map (\part -> typeNotParenthesized part)
                                )
                            )
                        |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.TypeAnnotation.Record fields ->
            case fields of
                [] ->
                    Print.symbol "{}"

                field0 :: field1Up ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange typeRange
                    in
                    Print.symbol "{"
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated lineOffset
                                (fields
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                            Print.symbol fieldName
                                                |> Print.followedBy Print.space
                                                |> Print.followedBy (Print.symbol ":")
                                                |> Print.followedBy (Print.layoutPositiveIndent (lineOffsetInNode fieldValue))
                                                |> Print.followedBy
                                                    (Print.bumpIndentBy4 (typeNotParenthesized fieldValue))
                                        )
                                )
                            )
                        |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ recordVariable) (Elm.Syntax.Node.Node _ fields) ->
            let
                lineOffset : Print.LineOffset
                lineOffset =
                    lineOffsetInRange typeRange
            in
            Print.symbol "{"
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol recordVariable)
                |> Print.followedBy (Print.layoutPositiveIndent (lineOffsetInRange typeRange))
                |> Print.followedBy
                    (Print.bumpIndentBy4
                        (Print.symbol "|"
                            |> Print.followedBy Print.space
                            |> Print.followedBy
                                (commaSeparated lineOffset
                                    (fields
                                        |> List.map
                                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                                Print.symbol fieldName
                                                    |> Print.followedBy Print.space
                                                    |> Print.followedBy (Print.symbol ":")
                                                    |> Print.followedBy (Print.layoutPositiveIndent (lineOffsetInNode fieldValue))
                                                    |> Print.followedBy (typeNotParenthesized fieldValue)
                                            )
                                    )
                                )
                        )
                    )
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            typeFunctionNotParenthesized inType outType
