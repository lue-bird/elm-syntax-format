module ElmSyntaxFormat exposing
    ( module_, moduleName, exposing_, expose, imports, import_, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , case_, patternNotParenthesized, patternParenthesizedIfSpaceSeparated, typeNotParenthesized
    )

{-| Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

@docs module_, moduleName, exposing_, expose, imports, import_, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expression, case_, patternNotParenthesized, patternParenthesizedIfSpaceSeparated, typeNotParenthesized

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import Print exposing (Print)
import Unicode


module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    let
        maybeModuleDocumentation : Maybe (Elm.Syntax.Node.Node String)
        maybeModuleDocumentation =
            moduleDocumentation syntaxModule

        commentsAndPortDocumentationComments : List (Elm.Syntax.Node.Node String)
        commentsAndPortDocumentationComments =
            case maybeModuleDocumentation of
                Nothing ->
                    syntaxModule.comments

                Just syntaxModuleDocumentation ->
                    syntaxModule.comments
                        |> List.filter (\c -> c /= syntaxModuleDocumentation)

        atDocsLines : List (List String)
        atDocsLines =
            case maybeModuleDocumentation of
                Nothing ->
                    []

                Just (Elm.Syntax.Node.Node _ syntaxModuleDocumentation) ->
                    syntaxModuleDocumentation
                        |> moduleDocumentationParse
                        |> .whileAtDocsLines
                        |> List.map .atDocsLine

        maybeModuleDocumentationPrint : Print
        maybeModuleDocumentationPrint =
            case maybeModuleDocumentation of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ moduleDocumentationAsString) ->
                    Print.linebreak
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy (Print.symbol moduleDocumentationAsString)

        importsPrint : Print
        importsPrint =
            case syntaxModule.imports of
                [] ->
                    Print.empty

                import0 :: import1Up ->
                    Print.linebreak
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            ((import0 :: import1Up)
                                |> List.map Elm.Syntax.Node.value
                                |> imports
                            )
                        |> Print.followedBy Print.linebreak
    in
    syntaxModule.moduleDefinition
        |> Elm.Syntax.Node.value
        |> moduleHeader atDocsLines
        |> Print.followedBy maybeModuleDocumentationPrint
        |> Print.followedBy importsPrint
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (syntaxModule.declarations
                |> declarations commentsAndPortDocumentationComments
            )
        |> Print.followedBy Print.linebreak


moduleDocumentation : Elm.Syntax.File.File -> Maybe (Elm.Syntax.Node.Node String)
moduleDocumentation ast =
    let
        cutOffLine : Int
        cutOffLine =
            case ast.imports of
                (Elm.Syntax.Node.Node firstImportRange _) :: _ ->
                    firstImportRange.start.row

                [] ->
                    case ast.declarations of
                        (Elm.Syntax.Node.Node firstDeclarationRange _) :: _ ->
                            firstDeclarationRange.start.row

                        [] ->
                            -- Should not happen, as every module should have at least one declaration
                            0
    in
    moduleDocumentationBeforeCutOffLine cutOffLine ast.comments


moduleDocumentationBeforeCutOffLine : Int -> List (Elm.Syntax.Node.Node String) -> Maybe (Elm.Syntax.Node.Node String)
moduleDocumentationBeforeCutOffLine cutOffLine allComments =
    case allComments of
        [] ->
            Nothing

        headComment :: restOfComments ->
            let
                (Elm.Syntax.Node.Node range content) =
                    headComment
            in
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just headComment

            else
                moduleDocumentationBeforeCutOffLine cutOffLine restOfComments


atDocsLineToExposesAndRemaining :
    List String
    -> List Elm.Syntax.Exposing.TopLevelExpose
    ->
        { remainingExposes : List Elm.Syntax.Exposing.TopLevelExpose
        , exposes : List Elm.Syntax.Exposing.TopLevelExpose
        }
atDocsLineToExposesAndRemaining atDocsLine remainingExposes =
    atDocsLine
        |> List.foldr
            (\exposeAsAtDocsString soFar ->
                let
                    toExposeReferencedByAtDocsString : Elm.Syntax.Exposing.TopLevelExpose -> Maybe Elm.Syntax.Exposing.TopLevelExpose
                    toExposeReferencedByAtDocsString ex =
                        if (ex |> exposeToAtDocsString) == exposeAsAtDocsString then
                            Just ex

                        else
                            Nothing
                in
                case soFar.remainingExposes |> listFirstJustMap toExposeReferencedByAtDocsString of
                    Nothing ->
                        soFar

                    Just exposeReferencedByAtDocsString ->
                        { remainingExposes =
                            soFar.remainingExposes
                                |> List.filter (\ex -> ex /= exposeReferencedByAtDocsString)
                        , exposes = exposeReferencedByAtDocsString :: soFar.exposes
                        }
            )
            { remainingExposes = remainingExposes
            , exposes = []
            }


exposingMaybeGroupedByAtDocsLines :
    List (List String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
exposingMaybeGroupedByAtDocsLines atDocsLines (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Elm.Syntax.Exposing.All _ ->
            Print.symbol "("
                |> Print.followedBy (Print.symbol "..")
                |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet |> exposeListToNormal of
                [] ->
                    Print.symbol "()"

                [ Elm.Syntax.Node.Node _ onlySyntaxExpose ] ->
                    Print.symbol "("
                        |> Print.followedBy (expose onlySyntaxExpose)
                        |> Print.followedBy (Print.symbol ")")

                expose0 :: expose1 :: expose2Up ->
                    case atDocsLines of
                        atDocsLine0 :: atDocsLine1Up ->
                            let
                                atDocsExposeLines :
                                    { remainingExposes : List Elm.Syntax.Exposing.TopLevelExpose
                                    , atDocsExposeLines : List (List Elm.Syntax.Exposing.TopLevelExpose)
                                    }
                                atDocsExposeLines =
                                    (atDocsLine0 :: atDocsLine1Up)
                                        |> List.foldr
                                            (\atDocsLine soFar ->
                                                let
                                                    atDocsExposeLine :
                                                        { remainingExposes : List Elm.Syntax.Exposing.TopLevelExpose
                                                        , exposes : List Elm.Syntax.Exposing.TopLevelExpose
                                                        }
                                                    atDocsExposeLine =
                                                        atDocsLineToExposesAndRemaining atDocsLine soFar.remainingExposes
                                                in
                                                { atDocsExposeLines =
                                                    atDocsExposeLine.exposes :: soFar.atDocsExposeLines
                                                , remainingExposes = atDocsExposeLine.remainingExposes
                                                }
                                            )
                                            { remainingExposes =
                                                (expose0 :: expose1 :: expose2Up)
                                                    |> exposeListToNormal
                                                    |> List.map Elm.Syntax.Node.value
                                            , atDocsExposeLines = []
                                            }
                            in
                            case atDocsExposeLines.atDocsExposeLines |> List.filter (\line -> line /= []) of
                                [] ->
                                    exposingMulti (lineOffsetInRange exposingRange) expose0 expose1 expose2Up

                                atDocsExposeLine0 :: atDocsExposeLine1Up ->
                                    Print.symbol "("
                                        |> Print.followedBy Print.space
                                        |> Print.followedBy
                                            (Print.inSequence
                                                ((atDocsExposeLine0 :: atDocsExposeLine1Up)
                                                    |> List.map
                                                        (\atDocsLine ->
                                                            Print.inSequence
                                                                (atDocsLine
                                                                    |> List.map expose
                                                                    |> List.intersperse
                                                                        (Print.symbol ","
                                                                            |> Print.followedBy Print.space
                                                                        )
                                                                )
                                                        )
                                                    |> List.intersperse
                                                        (Print.layout Print.NextLine
                                                            |> Print.followedBy (Print.symbol ",")
                                                            |> Print.followedBy Print.space
                                                        )
                                                )
                                            )
                                        |> Print.followedBy (Print.layout Print.NextLine)
                                        |> Print.followedBy
                                            (case atDocsExposeLines.remainingExposes of
                                                [] ->
                                                    Print.empty

                                                remainingExpose0 :: remainingExpose1Up ->
                                                    Print.symbol ","
                                                        |> Print.followedBy Print.space
                                                        |> Print.followedBy
                                                            (Print.inSequence
                                                                ((remainingExpose0 :: remainingExpose1Up)
                                                                    |> List.map expose
                                                                    |> List.intersperse
                                                                        (Print.symbol ","
                                                                            |> Print.followedBy Print.space
                                                                        )
                                                                )
                                                            )
                                                        |> Print.followedBy (Print.layout Print.NextLine)
                                            )
                                        |> Print.followedBy (Print.symbol ")")

                        [] ->
                            exposingMulti (lineOffsetInRange exposingRange) expose0 expose1 expose2Up


moduleHeader : List (List String) -> Elm.Syntax.Module.Module -> Print
moduleHeader atDocsLines syntaxModuleHeader =
    case syntaxModuleHeader of
        Elm.Syntax.Module.NormalModule defaultModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    defaultModuleData.exposingList
                        |> exposingMaybeGroupedByAtDocsLines atDocsLines

                lineOffset : Print.LineOffset
                lineOffset =
                    exposingPrint |> Print.lineOffset
            in
            Print.symbol "module"
                |> Print.followedBy Print.space
                |> Print.followedBy (moduleName (defaultModuleData.moduleName |> Elm.Syntax.Node.value))
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "exposing")
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
                        (Print.layout lineOffset
                            |> Print.followedBy exposingPrint
                        )
                    )

        Elm.Syntax.Module.PortModule defaultModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    defaultModuleData.exposingList
                        |> exposingMaybeGroupedByAtDocsLines atDocsLines

                lineOffset : Print.LineOffset
                lineOffset =
                    exposingPrint |> Print.lineOffset
            in
            Print.symbol "port"
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "module")
                |> Print.followedBy Print.space
                |> Print.followedBy (moduleName (defaultModuleData.moduleName |> Elm.Syntax.Node.value))
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "exposing")
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
                        (Print.layout lineOffset
                            |> Print.followedBy exposingPrint
                        )
                    )

        Elm.Syntax.Module.EffectModule effectModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    effectModuleData.exposingList
                        |> exposingMaybeGroupedByAtDocsLines atDocsLines

                lineOffset : Print.LineOffset
                lineOffset =
                    exposingPrint |> Print.lineOffset
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
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
                        (Print.layout lineOffset
                            |> Print.followedBy exposingPrint
                        )
                    )


exposeToAtDocsString : Elm.Syntax.Exposing.TopLevelExpose -> String
exposeToAtDocsString syntaxExpose =
    case syntaxExpose of
        Elm.Syntax.Exposing.InfixExpose operatorSymbol ->
            "(" ++ operatorSymbol ++ ")"

        Elm.Syntax.Exposing.FunctionExpose name ->
            name

        Elm.Syntax.Exposing.TypeOrAliasExpose name ->
            name

        Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
            choiceTypeExpose.name


imports : List Elm.Syntax.Import.Import -> Print
imports syntaxImports =
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


exposingLineOffset : Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing -> Print.LineOffset
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
                    let
                        exposingPrint : Print
                        exposingPrint =
                            exposing_ syntaxExposing

                        lineOffset : Print.LineOffset
                        lineOffset =
                            Print.lineOffset exposingPrint
                    in
                    Print.indentedByNextMultipleOf4
                        (Print.layout lineOffset
                            |> Print.followedBy (Print.symbol "exposing")
                            |> Print.followedBy
                                (Print.indentedByNextMultipleOf4
                                    (Print.layout lineOffset
                                        |> Print.followedBy exposingPrint
                                    )
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
        case syntaxImport.exposingList of
            Nothing ->
                Nothing

            Just (Elm.Syntax.Node.Node exposingRange syntaxExposing) ->
                Just
                    (Elm.Syntax.Node.Node exposingRange
                        (syntaxExposing |> exposingToNormal)
                    )
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


exposingToNormal : Elm.Syntax.Exposing.Exposing -> Elm.Syntax.Exposing.Exposing
exposingToNormal syntaxExposing =
    case syntaxExposing of
        Elm.Syntax.Exposing.All allRange ->
            Elm.Syntax.Exposing.All allRange

        Elm.Syntax.Exposing.Explicit exposeSet ->
            Elm.Syntax.Exposing.Explicit (exposeSet |> exposeListToNormal)


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
    if earlierRange.start.row == laterRange.end.row then
        Print.SameLine

    else
        Print.NextLine


lineOffsetBetweenNodeList : List (Elm.Syntax.Node.Node b) -> Print.LineOffset
lineOffsetBetweenNodeList nodes =
    case nodes of
        [] ->
            Print.SameLine

        (Elm.Syntax.Node.Node earlierRange _) :: nodesAfterEarlier ->
            if nodesAfterEarlier |> List.all (\(Elm.Syntax.Node.Node laterRange _) -> earlierRange.end.row == laterRange.start.row) then
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


exposing_ : Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing -> Print
exposing_ (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Elm.Syntax.Exposing.All _ ->
            Print.symbol "("
                |> Print.followedBy (Print.symbol "..")
                |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet of
                [] ->
                    Print.symbol "()"

                [ Elm.Syntax.Node.Node _ onlySyntaxExpose ] ->
                    Print.symbol "("
                        |> Print.followedBy (expose onlySyntaxExpose)
                        |> Print.followedBy (Print.symbol ")")

                expose0 :: expose1 :: expose2Up ->
                    exposingMulti (lineOffsetInRange exposingRange) expose0 expose1 expose2Up


exposingMulti :
    Print.LineOffset
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
    -> Print
exposingMulti lineOffset expose0 expose1 expose2Up =
    Print.symbol "("
        |> Print.followedBy
            (case lineOffset of
                Print.SameLine ->
                    Print.empty

                Print.NextLine ->
                    Print.space
            )
        |> Print.followedBy
            (commaSeparated
                lineOffset
                ((expose0 :: expose1 :: expose2Up)
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ syntaxExpose) -> expose syntaxExpose)
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ")")


{-| `--` or `{- -}` comments placed _within a declaration_.
For top-level comments: [`moduleLevelComments`](#moduleLevelComments)
-}
comments : List String -> Print
comments syntaxComments =
    Print.inSequence
        (syntaxComments
            |> List.map
                (\syntaxComment ->
                    comment syntaxComment
                        |> Print.followedBy (Print.layout Print.NextLine)
                )
        )


moduleLevelComments : List String -> Print
moduleLevelComments syntaxComments =
    case syntaxComments of
        [] ->
            Print.empty

        comment0 :: comment1Up ->
            comment comment0
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (Print.inSequence
                        (comment1Up
                            |> List.map
                                (\syntaxComment ->
                                    if syntaxComment == "{--}" then
                                        Print.linebreak
                                            |> Print.followedBy Print.linebreak
                                            |> Print.followedBy (comment syntaxComment)
                                            |> Print.followedBy Print.linebreak

                                    else
                                        comment syntaxComment
                                            |> Print.followedBy Print.linebreak
                                )
                        )
                    )


comment : String -> Print
comment syntaxComment =
    if syntaxComment == "{--}" then
        Print.symbol "{--}"

    else if syntaxComment |> String.startsWith "--" then
        Print.symbol (syntaxComment |> String.trimRight)

    else
        -- comment starts with {-
        let
            commentContent : List String
            commentContent =
                syntaxComment
                    |> -- {-
                       String.dropLeft 2
                    |> -- -}
                       String.dropRight 2
                    |> String.lines
                    |> List.map String.trim
                    |> listDropLastIfIs (\line -> line == "")

            -- TODO drop last if == ""
        in
        Print.symbol "{-"
            |> Print.followedBy
                -- if original commentContent contains >= 2 lines, keep but
                (case commentContent of
                    -- only spaces
                    [] ->
                        Print.symbol "  "

                    [ singleLine ] ->
                        Print.space
                            |> Print.followedBy (Print.symbol singleLine)
                            |> Print.followedBy Print.space

                    firstLine :: secondLine :: thirdLineUp ->
                        (case firstLine of
                            "" ->
                                Print.layout Print.NextLine

                            lineNotEmpty ->
                                Print.space
                                    |> Print.followedBy (Print.symbol lineNotEmpty)
                                    |> Print.followedBy (Print.layout Print.NextLine)
                        )
                            |> Print.followedBy
                                (Print.inSequence
                                    ((secondLine :: thirdLineUp)
                                        |> List.map
                                            (\line ->
                                                case line of
                                                    "" ->
                                                        Print.layout Print.NextLine

                                                    lineNotEmpty ->
                                                        Print.symbol "   "
                                                            |> Print.followedBy (Print.symbol lineNotEmpty)
                                                            |> Print.followedBy (Print.layout Print.NextLine)
                                            )
                                    )
                                )
                )
            |> Print.followedBy (Print.symbol "-}")


listDropLastIfIs : (a -> Bool) -> List a -> List a
listDropLastIfIs lastElementShouldBeRemoved list =
    case list of
        [] ->
            []

        [ onlyElement ] ->
            if onlyElement |> lastElementShouldBeRemoved then
                []

            else
                [ onlyElement ]

        element0 :: element1 :: element2Up ->
            element0 :: listDropLastIfIs lastElementShouldBeRemoved (element1 :: element2Up)


commentsInRange : Elm.Syntax.Range.Range -> List (Elm.Syntax.Node.Node String) -> List String
commentsInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        (Elm.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case Elm.Syntax.Range.compareLocations headCommentRange.start range.start of
                LT ->
                    commentsInRange range tailComments

                EQ ->
                    headComment :: commentsInRange range tailComments

                GT ->
                    case Elm.Syntax.Range.compareLocations headCommentRange.end range.end of
                        GT ->
                            []

                        LT ->
                            headComment :: commentsInRange range tailComments

                        EQ ->
                            headComment :: commentsInRange range tailComments


atDocsStringLength : Int
atDocsStringLength =
    "@docs" |> String.length


moduleDocumentationParse :
    String
    ->
        { whileAtDocsLines : List { rawBefore : String, atDocsLine : List String }
        , rawAfterAtDocsLines : String
        }
moduleDocumentationParse moduleDocumentationContent =
    let
        parsed :
            { rawSinceAtDocs : String
            , finishedBlocks : List { rawBefore : String, atDocsLine : List String }
            }
        parsed =
            moduleDocumentationContent
                |> String.lines
                |> List.foldl
                    (\line soFar ->
                        if String.startsWith "@docs " line then
                            { rawSinceAtDocs = ""
                            , finishedBlocks =
                                { rawBefore = soFar.rawSinceAtDocs
                                , atDocsLine =
                                    String.slice atDocsStringLength (line |> String.length) line
                                        |> String.split ","
                                        |> List.map String.trim
                                }
                                    :: soFar.finishedBlocks
                            }

                        else
                            { rawSinceAtDocs = soFar.rawSinceAtDocs ++ "\n"
                            , finishedBlocks = soFar.finishedBlocks
                            }
                    )
                    { rawSinceAtDocs = "", finishedBlocks = [] }
    in
    { whileAtDocsLines = parsed.finishedBlocks |> List.reverse
    , rawAfterAtDocsLines = parsed.rawSinceAtDocs
    }


commaSeparated : Print.LineOffset -> List Print -> Print
commaSeparated lineOffset elements =
    -- TODO inline (and check indentation!)
    Print.inSequence
        (elements
            |> List.intersperse
                (Print.emptiableLayout lineOffset
                    |> Print.followedBy (Print.symbol ",")
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


quotedString : String -> Print
quotedString stringContent =
    Print.symbol "\""
        |> Print.followedBy
            (Print.symbol
                (stringContent
                    |> String.foldl
                        (\contentChar soFar ->
                            soFar ++ quotedStringCharToEscaped contentChar
                        )
                        ""
                )
            )
        |> Print.followedBy (Print.symbol "\"")


quotedStringCharToEscaped : Char -> String
quotedStringCharToEscaped character =
    case character of
        '"' ->
            "\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\u{000D}"

        otherCharacter ->
            if characterIsPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


quotedChar : Char -> Print
quotedChar charContent =
    Print.symbol "'"
        |> Print.followedBy
            (Print.symbol
                (quotedCharToEscaped charContent)
            )
        |> Print.followedBy (Print.symbol "'")


quotedCharToEscaped : Char -> String
quotedCharToEscaped character =
    case character of
        '\'' ->
            "'"

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\u{000D}"

        otherCharacter ->
            if characterIsPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


characterHex : Char -> String
characterHex character =
    String.toUpper (intToHexString (Char.toCode character))


characterIsPrint : Char -> Bool
characterIsPrint character =
    case Unicode.getCategory character of
        Nothing ->
            False

        Just category ->
            case category of
                Unicode.SeparatorLine ->
                    True

                Unicode.SeparatorParagraph ->
                    True

                Unicode.OtherControl ->
                    True

                Unicode.OtherFormat ->
                    True

                Unicode.OtherSurrogate ->
                    True

                Unicode.OtherPrivateUse ->
                    True

                Unicode.OtherNotAssigned ->
                    True

                _ ->
                    False


cappedInt : Int -> Print
cappedInt int =
    Print.symbol (String.fromInt int)


cappedHex : Int -> Print
cappedHex int =
    let
        maybeSignPrint : Print
        maybeSignPrint =
            if int < 0 then
                Print.symbol "-"

            else
                Print.empty

        intAbs : Int
        intAbs =
            int |> Basics.abs

        digitCountToPrint : Int
        digitCountToPrint =
            if intAbs <= 0xFF then
                2

            else if intAbs <= 0xFFFF then
                4

            else if intAbs <= 0xFFFFFFFF then
                8

            else
                16
    in
    maybeSignPrint
        |> Print.followedBy (Print.symbol "0x")
        |> Print.followedBy
            (Print.symbol (intToHexString int |> stringResizePadLeftWith0s digitCountToPrint))


stringResizePadLeftWith0s : Int -> (String -> String)
stringResizePadLeftWith0s length unpaddedString =
    if length < (unpaddedString |> String.length) then
        String.left length unpaddedString

    else
        String.repeat (length - (unpaddedString |> String.length)) "0"
            ++ unpaddedString


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
            quotedChar char

        Elm.Syntax.Pattern.StringPattern string ->
            quotedString string

        Elm.Syntax.Pattern.IntPattern int ->
            cappedInt int

        Elm.Syntax.Pattern.HexPattern int ->
            cappedHex int

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
            case fields of
                [] ->
                    Print.symbol "{}"

                field0 :: field1Up ->
                    Print.symbol "{"
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated Print.SameLine
                                ((field0 :: field1Up)
                                    |> List.map (\(Elm.Syntax.Node.Node _ fieldName) -> Print.symbol fieldName)
                                )
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
    qualifiedTuple ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name )


qualifiedTuple : ( Elm.Syntax.ModuleName.ModuleName, String ) -> Print
qualifiedTuple ( qualification, unqualified ) =
    case qualification of
        [] ->
            Print.symbol unqualified

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
                |> Print.followedBy (Print.symbol unqualified)


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
            typePrint : Print
            typePrint =
                typeNotParenthesized typeNode
        in
        Print.symbol "("
            |> Print.followedBy (Print.indented 1 typePrint)
            |> Print.followedBy
                (Print.emptiableLayout (Print.lineOffset typePrint))
            |> Print.followedBy (Print.symbol ")")

    else
        typeNotParenthesized typeNode


typeParenthesizedIfFunction : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Print
typeParenthesizedIfFunction typeNode =
    case typeNode |> Elm.Syntax.Node.value of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Print.symbol "("
                |> Print.followedBy
                    (Print.indented 1
                        (typeFunctionNotParenthesized inType outType)
                    )
                |> Print.followedBy
                    (Print.emptiableLayout
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
    let
        fullLineOffset : Print.LineOffset
        fullLineOffset =
            lineOffsetBetweenNodes inType outType
    in
    typeParenthesizedIfFunction inType
        |> Print.followedBy
            (Print.inSequence
                (typeFunctionExpand outType
                    |> List.map
                        (\afterArrowTypeNode ->
                            Print.layout fullLineOffset
                                |> Print.followedBy (Print.symbol "->")
                                |> Print.followedBy
                                    (Print.indentedByNextMultipleOf4
                                        (Print.layout (lineOffsetInNode afterArrowTypeNode)
                                            |> Print.followedBy (typeParenthesizedIfFunction afterArrowTypeNode)
                                        )
                                    )
                        )
                )
            )


typeFunctionExpand :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeFunctionExpand typeNode =
    case typeNode of
        Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType) ->
            inType :: typeFunctionExpand outType

        typeNodeNotFunction ->
            [ typeNodeNotFunction ]


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
                        (Print.indented 1
                            (typeNotParenthesized inParens)
                        )
                    |> Print.followedBy
                        (Print.emptiableLayout
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
typeNotParenthesized (Elm.Syntax.Node.Node fullRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Print.symbol "()"

        Elm.Syntax.TypeAnnotation.GenericType name ->
            Print.symbol name

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ syntaxQualifiedTuple) arguments ->
            let
                argumentPrints : List Print
                argumentPrints =
                    arguments
                        |> List.map
                            (\argument -> typeParenthesizedIfSpaceSeparated argument)

                lineOffset : Print.LineOffset
                lineOffset =
                    argumentPrints
                        |> List.map Print.lineOffset
                        |> Print.listCombineLineOffset
            in
            qualifiedTuple syntaxQualifiedTuple
                |> Print.followedBy
                    (Print.inSequence
                        (argumentPrints
                            |> List.map
                                (\argumentPrint ->
                                    Print.layout lineOffset
                                        |> Print.followedBy argumentPrint
                                )
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
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized part0))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized part1))
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                [ part0, part1, part2 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized part0))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized part1))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized part2))
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated lineOffset
                                ((part0 :: part1 :: part2 :: part3 :: part4Up)
                                    |> List.map (\part -> Print.indented 2 (typeNotParenthesized part))
                                )
                            )
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.TypeAnnotation.Record fields ->
            case fields of
                [] ->
                    Print.symbol "{}"

                field0 :: field1Up ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
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
                                                |> Print.followedBy
                                                    (Print.indentedByNextMultipleOf4
                                                        (Print.layout (lineOffsetInNode fieldValue)
                                                            |> Print.followedBy
                                                                (typeNotParenthesized fieldValue)
                                                        )
                                                    )
                                        )
                                )
                            )
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ recordVariable) (Elm.Syntax.Node.Node _ fields) ->
            let
                lineOffset : Print.LineOffset
                lineOffset =
                    lineOffsetInRange fullRange
            in
            Print.symbol "{"
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol recordVariable)
                |> Print.followedBy (Print.layout lineOffset)
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
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
                                                    |> Print.followedBy
                                                        (Print.indentedByNextMultipleOf4
                                                            (Print.layout (lineOffsetInNode fieldValue)
                                                                |> Print.followedBy (typeNotParenthesized fieldValue)
                                                            )
                                                        )
                                            )
                                    )
                                )
                        )
                    )
                |> Print.followedBy (Print.layout lineOffset)
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            typeFunctionNotParenthesized inType outType


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


declarations :
    List (Elm.Syntax.Node.Node String)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)
    -> Print
declarations syntaxComments syntaxDeclarations =
    case syntaxDeclarations of
        [] ->
            -- invalid syntax
            Print.empty

        (Elm.Syntax.Node.Node declaration0Range declaration0) :: declarations1Up ->
            declaration declaration0
                |> Print.followedBy
                    (declarations1Up
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node declarationRange syntaxDeclaration) soFar ->
                                { print =
                                    soFar.print
                                        |> Print.followedBy
                                            (case commentsInRange { start = soFar.previousRange.end, end = declarationRange.start } syntaxComments of
                                                comment0 :: comment1Up ->
                                                    Print.linebreak
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy (moduleLevelComments (comment0 :: comment1Up))
                                                        |> Print.followedBy
                                                            (if listFilledLast ( comment0, comment1Up ) == "{--}" then
                                                                -- don't ask me why elm-format formats it that way
                                                                Print.empty

                                                             else
                                                                Print.linebreak
                                                                    |> Print.followedBy Print.linebreak
                                                            )
                                                        |> Print.followedBy (declaration syntaxDeclaration)

                                                [] ->
                                                    linebreaksFollowedByDeclaration syntaxComments
                                                        syntaxDeclaration
                                            )
                                , previousRange = declarationRange
                                }
                            )
                            { print = Print.empty
                            , previousRange = declaration0Range
                            }
                        |> .print
                    )


listFilledLast : ( a, List a ) -> a
listFilledLast ( head, tail ) =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast ( tailHead, tailTail )


linebreaksFollowedByDeclaration :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Declaration.Declaration
    -> Print
linebreaksFollowedByDeclaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        Elm.Syntax.Declaration.FunctionDeclaration syntaxExpressionDeclaration ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationExpression syntaxExpressionDeclaration)

        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationTypeAlias syntaxTypeAliasDeclaration)

        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationChoiceType syntaxChoiceTypeDeclaration)

        Elm.Syntax.Declaration.PortDeclaration signature ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationPort signature)

        Elm.Syntax.Declaration.InfixDeclaration syntaxInfixDeclaration ->
            Print.linebreak
                |> Print.followedBy (declarationInfix syntaxInfixDeclaration)

        Elm.Syntax.Declaration.Destructuring (Elm.Syntax.Node.Node _ destructuringPattern) destructuringExpression ->
            -- invalid syntax
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (declarationDestructuring destructuringPattern destructuringExpression)


declaration : Elm.Syntax.Declaration.Declaration -> Print
declaration syntaxDeclaration =
    case syntaxDeclaration of
        Elm.Syntax.Declaration.FunctionDeclaration syntaxExpressionDeclaration ->
            declarationExpression syntaxExpressionDeclaration

        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
            declarationTypeAlias syntaxTypeAliasDeclaration

        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
            declarationChoiceType syntaxChoiceTypeDeclaration

        Elm.Syntax.Declaration.PortDeclaration signature ->
            declarationPort signature

        Elm.Syntax.Declaration.InfixDeclaration syntaxInfixDeclaration ->
            declarationInfix syntaxInfixDeclaration

        Elm.Syntax.Declaration.Destructuring (Elm.Syntax.Node.Node _ destructuringPattern) destructuringExpression ->
            -- invalid syntax
            declarationDestructuring destructuringPattern destructuringExpression


declarationDestructuring : Elm.Syntax.Pattern.Pattern -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
declarationDestructuring destructuringPattern destructuringExpression =
    patternParenthesizedIfSpaceSeparated destructuringPattern
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "=")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (expressionNotParenthesized destructuringExpression)
                )
            )


declarationPort : Elm.Syntax.Signature.Signature -> Print
declarationPort signature =
    Print.symbol "port"
        |> Print.followedBy Print.space
        |> Print.followedBy (declarationSignature signature)


declarationTypeAlias : Elm.Syntax.TypeAlias.TypeAlias -> Print
declarationTypeAlias syntaxTypeAliasDeclaration =
    let
        maybeDocumentationPrint : Print
        maybeDocumentationPrint =
            case syntaxTypeAliasDeclaration.documentation of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ documentation) ->
                    Print.symbol documentation
                        |> Print.followedBy Print.linebreak
    in
    maybeDocumentationPrint
        |> Print.followedBy (Print.symbol "type")
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "alias")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.symbol (syntaxTypeAliasDeclaration.name |> Elm.Syntax.Node.value))
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.inSequence
                (syntaxTypeAliasDeclaration.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            Print.symbol parameter
                                |> Print.followedBy Print.space
                        )
                )
            )
        |> Print.followedBy (Print.symbol "=")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (typeNotParenthesized syntaxTypeAliasDeclaration.typeAnnotation)
                )
            )


declarationChoiceType : Elm.Syntax.Type.Type -> Print
declarationChoiceType syntaxChoiceTypeDeclaration =
    let
        maybeDocumentationPrint : Print
        maybeDocumentationPrint =
            case syntaxChoiceTypeDeclaration.documentation of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ documentation) ->
                    Print.symbol documentation
                        |> Print.followedBy Print.linebreak
    in
    maybeDocumentationPrint
        |> Print.followedBy (Print.symbol "type")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.symbol (syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value))
        |> Print.followedBy
            (Print.inSequence
                (syntaxChoiceTypeDeclaration.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            Print.space
                                |> Print.followedBy (Print.symbol parameter)
                        )
                )
            )
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (Print.symbol "=")
                    |> Print.followedBy Print.space
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxChoiceTypeDeclaration.constructors
                                |> List.map
                                    (\(Elm.Syntax.Node.Node _ variant) ->
                                        let
                                            parameterPrints : List Print
                                            parameterPrints =
                                                variant.arguments
                                                    |> List.map typeParenthesizedIfSpaceSeparated

                                            parametersLineOffset : Print.LineOffset
                                            parametersLineOffset =
                                                Print.listCombineLineOffset (parameterPrints |> List.map Print.lineOffset)
                                        in
                                        Print.symbol (variant.name |> Elm.Syntax.Node.value)
                                            |> Print.followedBy
                                                (Print.indentedByNextMultipleOf4
                                                    (Print.inSequence
                                                        (parameterPrints
                                                            |> List.map
                                                                (\parameterPrint ->
                                                                    Print.layout parametersLineOffset
                                                                        |> Print.followedBy parameterPrint
                                                                )
                                                        )
                                                    )
                                                )
                                    )
                                |> List.intersperse
                                    (Print.layout Print.NextLine
                                        |> Print.followedBy (Print.symbol "|")
                                        |> Print.followedBy Print.space
                                    )
                            )
                        )
                )
            )


declarationInfix : Elm.Syntax.Infix.Infix -> Print
declarationInfix syntaxInfixDeclaration =
    Print.symbol "infix"
        |> Print.followedBy Print.space
        |> Print.followedBy
            (infixDirection (syntaxInfixDeclaration.direction |> Elm.Syntax.Node.value))
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.symbol (String.fromInt (syntaxInfixDeclaration.precedence |> Elm.Syntax.Node.value)))
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "(")
        |> Print.followedBy
            (Print.symbol (syntaxInfixDeclaration.operator |> Elm.Syntax.Node.value))
        |> Print.followedBy (Print.symbol ")")
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "=")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.symbol (syntaxInfixDeclaration.function |> Elm.Syntax.Node.value))


infixDirection : Elm.Syntax.Infix.InfixDirection -> Print
infixDirection syntaxInfixDirection =
    case syntaxInfixDirection of
        Elm.Syntax.Infix.Left ->
            Print.symbol "left "

        Elm.Syntax.Infix.Right ->
            Print.symbol "right"

        Elm.Syntax.Infix.Non ->
            Print.symbol "non  "


declarationExpression : Elm.Syntax.Expression.Function -> Print
declarationExpression syntaxExpressionDeclaration =
    let
        maybeDocumentationPrint : Print
        maybeDocumentationPrint =
            case syntaxExpressionDeclaration.documentation of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ documentation) ->
                    Print.symbol documentation
                        |> Print.followedBy Print.linebreak

        maybeSignaturePrint : Print
        maybeSignaturePrint =
            case syntaxExpressionDeclaration.signature of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ signature) ->
                    declarationSignature signature
                        |> Print.followedBy Print.linebreak
    in
    maybeDocumentationPrint
        |> Print.followedBy maybeSignaturePrint
        |> Print.followedBy
            (declarationExpressionImplementation
                (syntaxExpressionDeclaration.declaration |> Elm.Syntax.Node.value)
            )


declarationSignature : Elm.Syntax.Signature.Signature -> Print
declarationSignature signature =
    let
        typePrint : Print
        typePrint =
            typeNotParenthesized signature.typeAnnotation
    in
    Print.symbol (signature.name |> Elm.Syntax.Node.value)
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol ":")
        |> Print.followedBy
            (Print.layout (Print.lineOffset typePrint))
        |> Print.followedBy typePrint


declarationExpressionImplementation : Elm.Syntax.Expression.FunctionImplementation -> Print
declarationExpressionImplementation implementation =
    Print.symbol (implementation.name |> Elm.Syntax.Node.value)
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.inSequence
                (implementation.arguments
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameterPattern) ->
                            patternParenthesizedIfSpaceSeparated parameterPattern
                                |> Print.followedBy Print.space
                        )
                )
            )
        |> Print.followedBy (Print.symbol "=")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (expressionNotParenthesized implementation.expression)
                )
            )


expressionNotParenthesized : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionNotParenthesized (Elm.Syntax.Node.Node fullRange syntaxExpression) =
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Print.symbol "()"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    -- invalid syntax
                    Print.empty

                [ notAppliedAfterAll ] ->
                    -- invalid syntax
                    expressionNotParenthesized notAppliedAfterAll

                applied :: argument0 :: argument1Up ->
                    let
                        argument1UpLineOffset : Print.LineOffset
                        argument1UpLineOffset =
                            lineOffsetInRange fullRange
                    in
                    expressionParenthesizedIfSpaceSeparated applied
                        |> Print.followedBy
                            (Print.indentedByNextMultipleOf4
                                (Print.layout (lineOffsetBetweenNodes applied argument0)
                                    |> Print.followedBy (expressionParenthesizedIfSpaceSeparated argument0)
                                    |> Print.followedBy
                                        (Print.inSequence
                                            (argument1Up
                                                |> List.map
                                                    (\argument ->
                                                        Print.layout argument1UpLineOffset
                                                            |> Print.followedBy (expressionParenthesizedIfSpaceSeparated argument)
                                                    )
                                            )
                                        )
                                )
                            )

        Elm.Syntax.Expression.OperatorApplication operator syntaxInfixDirection left right ->
            let
                lineOffset : Print.LineOffset
                lineOffset =
                    lineOffsetInRange fullRange

                operationExpanded :
                    { leftest : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    , beforeRightestOperatorExpressionChain :
                        List
                            { operator : String
                            , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                            }
                    , rightestOperator : String
                    , rightestExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    }
                operationExpanded =
                    expressionOperationExpand left operator right

                leftestPrint : Print
                leftestPrint =
                    expressionParenthesizedIfSpaceSeparatedExceptApplication operationExpanded.leftest
            in
            leftestPrint
                |> Print.followedBy
                    (List.foldr
                        (\operatorExpression chainRightPrint ->
                            \previousLineOffset ->
                                let
                                    expressionPrint : Print
                                    expressionPrint =
                                        expressionParenthesizedIfSpaceSeparatedExceptApplication operatorExpression.expression
                                in
                                case operatorExpression.operator of
                                    "<|" ->
                                        Print.layout previousLineOffset
                                            |> Print.followedBy (Print.symbol "<|")
                                            |> Print.followedBy
                                                (Print.indentedByNextMultipleOf4
                                                    (Print.layout lineOffset
                                                        |> Print.followedBy expressionPrint
                                                        |> Print.followedBy (chainRightPrint (Print.lineOffset expressionPrint))
                                                    )
                                                )

                                    nonApLOperator ->
                                        Print.indentedByNextMultipleOf4
                                            (Print.layout lineOffset
                                                |> Print.followedBy (Print.symbol nonApLOperator)
                                                |> Print.followedBy
                                                    (Print.indentedByNextMultipleOf4
                                                        (Print.space
                                                            |> Print.followedBy expressionPrint
                                                        )
                                                    )
                                            )
                                            |> Print.followedBy (chainRightPrint (Print.lineOffset expressionPrint))
                        )
                        (\previousLineOffset ->
                            case operationExpanded.rightestOperator of
                                "<|" ->
                                    let
                                        expressionPrint : Print
                                        expressionPrint =
                                            expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda operationExpanded.rightestExpression
                                    in
                                    Print.layout previousLineOffset
                                        |> Print.followedBy (Print.symbol "<|")
                                        |> Print.followedBy
                                            (Print.indentedByNextMultipleOf4
                                                (Print.layout lineOffset
                                                    |> Print.followedBy expressionPrint
                                                )
                                            )

                                nonApLOperator ->
                                    let
                                        expressionPrint : Print
                                        expressionPrint =
                                            expressionParenthesizedIfSpaceSeparatedExceptApplication operationExpanded.rightestExpression
                                    in
                                    Print.indentedByNextMultipleOf4
                                        (Print.layout lineOffset
                                            |> Print.followedBy (Print.symbol nonApLOperator)
                                            |> Print.followedBy
                                                (Print.indentedByNextMultipleOf4
                                                    (Print.space
                                                        |> Print.followedBy expressionPrint
                                                    )
                                                )
                                        )
                        )
                        operationExpanded.beforeRightestOperatorExpressionChain
                        (Print.lineOffset leftestPrint)
                    )

        Elm.Syntax.Expression.FunctionOrValue qualification unqualified ->
            qualifiedTuple ( qualification, unqualified )

        Elm.Syntax.Expression.IfBlock condition onTrue onFalse ->
            expressionIfThenElse condition onTrue onFalse

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Print.symbol "("
                |> Print.followedBy (Print.symbol operatorSymbol)
                |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Expression.Operator operatorSymbol ->
            -- invalid syntax
            Print.symbol operatorSymbol

        Elm.Syntax.Expression.Integer int ->
            cappedInt int

        Elm.Syntax.Expression.Hex int ->
            cappedHex int

        Elm.Syntax.Expression.Floatable float ->
            Print.symbol (String.fromFloat float)

        Elm.Syntax.Expression.Negation negated ->
            Print.symbol "-"
                |> Print.followedBy (expressionParenthesizedIfSpaceSeparated negated)

        Elm.Syntax.Expression.Literal string ->
            quotedString string

        Elm.Syntax.Expression.CharLiteral char ->
            quotedChar char

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Print.symbol "()"

                [ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    expressionNotParenthesized inParens

                [ part0, part1 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (expressionNotParenthesized part0))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (expressionNotParenthesized part1))
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                [ part0, part1, part2 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (expressionNotParenthesized part0))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (expressionNotParenthesized part1))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (expressionNotParenthesized part2))
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated lineOffset
                                ((part0 :: part1 :: part2 :: part3 :: part4Up)
                                    |> List.map (\part -> Print.indented 2 (expressionNotParenthesized part))
                                )
                            )
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            expressionNotParenthesized inParens

        Elm.Syntax.Expression.LetExpression syntaxLetIn ->
            expressionLetIn syntaxLetIn

        Elm.Syntax.Expression.CaseExpression syntaxCaseOf ->
            expressionCaseOf syntaxCaseOf

        Elm.Syntax.Expression.LambdaExpression syntaxLambda ->
            expressionLambda (Elm.Syntax.Node.Node fullRange syntaxLambda)

        Elm.Syntax.Expression.RecordExpr fields ->
            case fields of
                [] ->
                    Print.symbol "{}"

                field0 :: field1Up ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
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
                                                |> Print.followedBy (Print.symbol "=")
                                                |> Print.followedBy
                                                    (Print.indentedByNextMultipleOf4
                                                        (Print.layout (lineOffsetInNode fieldValue)
                                                            |> Print.followedBy
                                                                (expressionNotParenthesized fieldValue)
                                                        )
                                                    )
                                        )
                                )
                            )
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.Expression.ListExpr elements ->
            case elements of
                [] ->
                    Print.symbol "[]"

                field0 :: field1Up ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "["
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (commaSeparated lineOffset
                                (elements
                                    |> List.map
                                        (\element -> Print.indented 2 (expressionNotParenthesized element))
                                )
                            )
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol "]")

        Elm.Syntax.Expression.RecordAccess record (Elm.Syntax.Node.Node _ accessedFieldName) ->
            expressionParenthesizedIfSpaceSeparated record
                |> Print.followedBy (Print.symbol ".")
                |> Print.followedBy (Print.symbol accessedFieldName)

        Elm.Syntax.Expression.RecordAccessFunction dotFieldName ->
            Print.symbol dotFieldName

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ recordVariable) fields ->
            let
                lineOffset : Print.LineOffset
                lineOffset =
                    lineOffsetInRange fullRange
            in
            Print.symbol "{"
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol recordVariable)
                |> Print.followedBy (Print.layout (lineOffsetInRange fullRange))
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
                        (Print.symbol "|"
                            |> Print.followedBy Print.space
                            |> Print.followedBy
                                (commaSeparated lineOffset
                                    (fields
                                        |> List.map
                                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                                Print.symbol fieldName
                                                    |> Print.followedBy Print.space
                                                    |> Print.followedBy (Print.symbol "=")
                                                    |> Print.followedBy
                                                        (Print.indentedByNextMultipleOf4
                                                            (Print.layout (lineOffsetInNode fieldValue)
                                                                |> Print.followedBy (expressionNotParenthesized fieldValue)
                                                            )
                                                        )
                                            )
                                    )
                                )
                        )
                    )
                |> Print.followedBy (Print.layout lineOffset)
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.Expression.GLSLExpression glsl ->
            Print.symbol glsl


expressionOperationExpand :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> String
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        { leftest : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , beforeRightestOperatorExpressionChain :
            List
                { operator : String
                , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                }
        , rightestOperator : String
        , rightestExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
expressionOperationExpand left operator right =
    let
        rightExpanded =
            case right of
                Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.OperatorApplication rightOperator _ rightLeft rightRight) ->
                    let
                        rightOperationExpanded :
                            { leftest : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                            , beforeRightestOperatorExpressionChain :
                                List
                                    { operator : String
                                    , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                                    }
                            , rightestOperator : String
                            , rightestExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                            }
                        rightOperationExpanded =
                            expressionOperationExpand rightLeft rightOperator rightRight
                    in
                    { beforeRightestOperatorExpressionChain =
                        { operator = operator, expression = rightOperationExpanded.leftest }
                            :: rightOperationExpanded.beforeRightestOperatorExpressionChain
                    , rightestOperator = rightOperationExpanded.rightestOperator
                    , rightestExpression = rightOperationExpanded.rightestExpression
                    }

                _ ->
                    { beforeRightestOperatorExpressionChain = []
                    , rightestOperator = operator
                    , rightestExpression = right
                    }
    in
    case left of
        Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.OperatorApplication leftOperator _ leftLeft leftRight) ->
            let
                leftOperationExpanded :
                    { leftest : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    , beforeRightestOperatorExpressionChain :
                        List
                            { operator : String
                            , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                            }
                    , rightestOperator : String
                    , rightestExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    }
                leftOperationExpanded =
                    expressionOperationExpand leftLeft leftOperator leftRight
            in
            { leftest = leftOperationExpanded.leftest
            , beforeRightestOperatorExpressionChain =
                leftOperationExpanded.beforeRightestOperatorExpressionChain
                    ++ ({ operator = leftOperationExpanded.rightestOperator
                        , expression = leftOperationExpanded.rightestExpression
                        }
                            :: rightExpanded.beforeRightestOperatorExpressionChain
                       )
            , rightestOperator = rightExpanded.rightestOperator
            , rightestExpression = rightExpanded.rightestExpression
            }

        _ ->
            { leftest = left
            , beforeRightestOperatorExpressionChain = rightExpanded.beforeRightestOperatorExpressionChain
            , rightestOperator = rightExpanded.rightestOperator
            , rightestExpression = rightExpanded.rightestExpression
            }


expressionLambda : Elm.Syntax.Node.Node Elm.Syntax.Expression.Lambda -> Print
expressionLambda (Elm.Syntax.Node.Node lambdaRange syntaxLambda) =
    Print.symbol "\\"
        |> Print.followedBy
            (Print.inSequence
                (syntaxLambda.args
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameterPattern) ->
                            patternParenthesizedIfSpaceSeparated parameterPattern
                                |> Print.followedBy Print.space
                        )
                )
            )
        |> Print.followedBy (Print.symbol "->")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout (lineOffsetInRange lambdaRange))
            )
        |> Print.followedBy
            (expressionNotParenthesized syntaxLambda.expression)


expressionIfThenElse :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionIfThenElse condition onTrue onFalse =
    let
        conditionPrint : Print
        conditionPrint =
            expressionNotParenthesized condition

        conditionLineOffset : Print.LineOffset
        conditionLineOffset =
            Print.lineOffset conditionPrint

        onTruePrint : Print
        onTruePrint =
            expressionNotParenthesized onTrue

        onFalsePrint : Print
        onFalsePrint =
            expressionNotParenthesized onFalse
    in
    Print.symbol "if"
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout conditionLineOffset
                    |> Print.followedBy conditionPrint
                )
            )
        |> Print.followedBy (Print.layout conditionLineOffset)
        |> Print.followedBy (Print.symbol "then")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy onTruePrint
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy (Print.layout Print.NextLine)
        |> Print.followedBy (Print.symbol "else")
        |> Print.followedBy
            (case expressionToNotParenthesized (onFalse |> Elm.Syntax.Node.value) of
                Elm.Syntax.Expression.IfBlock onFalseCondition onFalseOnTrue onFalseOnFalse ->
                    Print.space
                        |> Print.followedBy
                            (expressionIfThenElse onFalseCondition onFalseOnTrue onFalseOnFalse)

                _ ->
                    Print.indentedByNextMultipleOf4
                        (Print.layout Print.NextLine
                            |> Print.followedBy onFalsePrint
                        )
            )


expressionToNotParenthesized : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
expressionToNotParenthesized syntaxExpression =
    case syntaxExpression of
        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ inParens) ->
            inParens

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    Elm.Syntax.Expression.TupledExpression parts

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    inParens

                [ _, _ ] ->
                    Elm.Syntax.Expression.TupledExpression parts

                [ _, _, _ ] ->
                    Elm.Syntax.Expression.TupledExpression parts

                _ :: _ :: _ :: _ :: _ ->
                    Elm.Syntax.Expression.TupledExpression parts

        syntaxExpressionNotParenthesized ->
            syntaxExpressionNotParenthesized


expressionCaseOf : Elm.Syntax.Expression.CaseBlock -> Print
expressionCaseOf syntaxCaseOf =
    let
        casedExpressionLineOffset : Print.LineOffset
        casedExpressionLineOffset =
            lineOffsetInNode syntaxCaseOf.expression
    in
    Print.symbol "case"
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout casedExpressionLineOffset
                    |> Print.followedBy (expressionNotParenthesized syntaxCaseOf.expression)
                )
            )
        |> Print.followedBy
            (Print.layout casedExpressionLineOffset)
        |> Print.followedBy (Print.symbol "of")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxCaseOf.cases
                                |> List.map case_
                                |> List.intersperse
                                    (Print.linebreak
                                        |> Print.followedBy (Print.layout Print.NextLine)
                                    )
                            )
                        )
                )
            )


case_ : Elm.Syntax.Expression.Case -> Print
case_ ( Elm.Syntax.Node.Node _ casePattern, caseResult ) =
    patternNotParenthesized casePattern
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "->")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (expressionNotParenthesized caseResult)
                )
            )


expressionLetIn : Elm.Syntax.Expression.LetBlock -> Print
expressionLetIn syntaxLetIn =
    Print.symbol "let"
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxLetIn.declarations
                                |> List.map
                                    (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                        expressionLetDeclaration letDeclaration
                                    )
                                |> List.intersperse
                                    (Print.linebreak
                                        |> Print.followedBy (Print.layout Print.NextLine)
                                    )
                            )
                        )
                )
            )
        |> Print.followedBy (Print.layout Print.NextLine)
        |> Print.followedBy (Print.symbol "in")
        |> Print.followedBy (Print.layout Print.NextLine)
        |> Print.followedBy (expressionNotParenthesized syntaxLetIn.expression)


expressionLetDeclaration : Elm.Syntax.Expression.LetDeclaration -> Print
expressionLetDeclaration letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction letDeclarationExpression ->
            let
                maybeSignaturePrint : Print
                maybeSignaturePrint =
                    case letDeclarationExpression.signature of
                        Nothing ->
                            Print.empty

                        Just (Elm.Syntax.Node.Node _ signature) ->
                            declarationSignature signature
                                |> Print.followedBy (Print.layout Print.NextLine)
            in
            maybeSignaturePrint
                |> Print.followedBy
                    (declarationExpressionImplementation
                        (letDeclarationExpression.declaration |> Elm.Syntax.Node.value)
                    )

        Elm.Syntax.Expression.LetDestructuring (Elm.Syntax.Node.Node _ destructuringPattern) destructuredExpression ->
            patternParenthesizedIfSpaceSeparated destructuringPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "=")
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
                        (Print.layout Print.NextLine
                            |> Print.followedBy (expressionNotParenthesized destructuredExpression)
                        )
                    )


expressionParenthesizedIfSpaceSeparated : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionParenthesizedIfSpaceSeparated expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> Elm.Syntax.Node.value) then
        expressionParenthesized expressionNode

    else
        expressionNotParenthesized expressionNode


expressionParenthesizedIfSpaceSeparatedExceptApplication : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplication expressionNode =
    let
        (Elm.Syntax.Node.Node _ syntaxExpression) =
            expressionNode
    in
    if expressionIsSpaceSeparated syntaxExpression then
        case syntaxExpression |> expressionToNotParenthesized of
            Elm.Syntax.Expression.Application _ ->
                expressionNotParenthesized expressionNode

            _ ->
                expressionParenthesized expressionNode

    else
        expressionNotParenthesized expressionNode


expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda expressionNode =
    let
        (Elm.Syntax.Node.Node _ syntaxExpression) =
            expressionNode
    in
    if expressionIsSpaceSeparated syntaxExpression then
        case syntaxExpression |> expressionToNotParenthesized of
            Elm.Syntax.Expression.Application _ ->
                expressionNotParenthesized expressionNode

            Elm.Syntax.Expression.LambdaExpression _ ->
                expressionNotParenthesized expressionNode

            _ ->
                expressionParenthesized expressionNode

    else
        expressionNotParenthesized expressionNode


expressionParenthesized : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionParenthesized expressionNode =
    let
        expressionPrint : Print
        expressionPrint =
            expressionNotParenthesized expressionNode
    in
    Print.symbol "("
        |> Print.followedBy (Print.indented 1 expressionPrint)
        |> Print.followedBy
            (Print.emptiableLayout (Print.lineOffset expressionPrint))
        |> Print.followedBy (Print.symbol ")")


expressionIsParenthesizedOperation : Elm.Syntax.Expression.Expression -> Bool
expressionIsParenthesizedOperation syntaxExpression =
    case syntaxExpression of
        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ inParens) ->
            expressionIsOperation inParens

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    False

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    expressionIsOperation inParens

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    False

        _ ->
            False


expressionIsOperation : Elm.Syntax.Expression.Expression -> Bool
expressionIsOperation syntaxExpression =
    case syntaxExpression of
        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ inParens) ->
            expressionIsOperation inParens

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            True

        _ ->
            False


expressionIsSpaceSeparated : Elm.Syntax.Expression.Expression -> Bool
expressionIsSpaceSeparated syntaxExpression =
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            False

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    -- invalid syntax
                    False

                [ Elm.Syntax.Node.Node _ notActuallyApplied ] ->
                    -- invalid syntax
                    expressionIsSpaceSeparated notActuallyApplied

                _ :: _ :: _ ->
                    True

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            True

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            False

        Elm.Syntax.Expression.IfBlock _ _ _ ->
            True

        Elm.Syntax.Expression.PrefixOperator _ ->
            False

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            False

        Elm.Syntax.Expression.Integer _ ->
            False

        Elm.Syntax.Expression.Hex _ ->
            False

        Elm.Syntax.Expression.Floatable _ ->
            False

        Elm.Syntax.Expression.Negation _ ->
            False

        Elm.Syntax.Expression.Literal _ ->
            False

        Elm.Syntax.Expression.CharLiteral _ ->
            False

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- should be handled by UnitExpr
                    False

                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    expressionIsSpaceSeparated inParens

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    False

        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ inParens) ->
            expressionIsSpaceSeparated inParens

        Elm.Syntax.Expression.LetExpression _ ->
            True

        Elm.Syntax.Expression.CaseExpression _ ->
            True

        Elm.Syntax.Expression.LambdaExpression _ ->
            True

        Elm.Syntax.Expression.RecordExpr _ ->
            False

        Elm.Syntax.Expression.ListExpr _ ->
            False

        Elm.Syntax.Expression.RecordAccess _ _ ->
            False

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            False

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            False

        Elm.Syntax.Expression.GLSLExpression _ ->
            False


listFirstJustMap : (a -> Maybe b) -> (List a -> Maybe b)
listFirstJustMap elementToMaybe list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case elementToMaybe head of
                Nothing ->
                    listFirstJustMap elementToMaybe tail

                Just b ->
                    Just b
