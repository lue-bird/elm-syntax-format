module ElmSyntaxFormat exposing (exposing_, expose, import_, moduleName, module_)

{-|

@docs exposing_, expose, expression, import_, moduleName, module_, pattern, type_

-}

import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Print exposing (LineOffset, Print)


module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    -- TODO header
    -- TODO declarations
    -- TODO comments
    Print.inSequence
        (syntaxModule.imports
            |> List.map (\(Elm.Syntax.Node.Node _ syntaxImport) -> syntaxImport)
            |> List.sortWith
                (\a b ->
                    compare (a.moduleName |> Elm.Syntax.Node.value) (b.moduleName |> Elm.Syntax.Node.value)
                )
            |> importsCombine
            |> List.map (\syntaxImport -> import_ syntaxImport)
            |> List.intersperse Print.linebreak
        )


import_ : Elm.Syntax.Import.Import -> Print
import_ syntaxImport =
    let
        lineOffset : Print.LineOffset
        lineOffset =
            case syntaxImport.exposingList of
                Nothing ->
                    Print.SameLine

                Just (Elm.Syntax.Node.Node _ (Elm.Syntax.Exposing.All _)) ->
                    Print.SameLine

                Just (Elm.Syntax.Node.Node exposingRange (Elm.Syntax.Exposing.Explicit exposingSet)) ->
                    case exposingSet of
                        [] ->
                            Print.SameLine

                        [ _ ] ->
                            Print.SameLine

                        _ :: _ :: _ ->
                            lineOffsetInRange exposingRange
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
                            (Print.bumpIndent
                                (Print.layoutPositiveIndent lineOffset
                                    |> Print.followedBy (exposing_ lineOffset syntaxExposing)
                                )
                            )
            )


importsCombine :
    List Elm.Syntax.Import.Import
    -> List Elm.Syntax.Import.Import
importsCombine imports =
    case imports of
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
                            commaSeparated
                                lineOffset
                                (exposingSet
                                    |> List.sortWith
                                        (\(Elm.Syntax.Node.Node _ a) (Elm.Syntax.Node.Node _ b) -> exposeCompare a b)
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ syntaxExpose) -> expose syntaxExpose)
                                )
            )
        |> Print.followedBy (Print.symbol ")")


commaSeparated : Print.LineOffset -> List Print -> Print
commaSeparated lineOffset elements =
    (case lineOffset of
        Print.SameLine ->
            Print.empty

        Print.NextLine ->
            Print.space
    )
        |> Print.followedBy
            (Print.inSequence
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
            Print.symbol operatorSymbol

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
