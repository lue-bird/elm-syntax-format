module ElmSyntaxFormat exposing
    ( printToString, Print
    , module_, moduleName, moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
    )

{-| Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

@docs printToString, Print

@docs module_, moduleName, moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized

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
import Print
import Unicode


{-| Pretty printable intermediate representation
-}
type alias Print =
    { indent : Int } -> String


{-| All other helpers in this module produce a [`Print`](#Print)
which you can in the end convert to a String with [`printToString`](#printToString)
-}
printToString : Print -> String
printToString print =
    print |> Print.toString


{-| Print an [`Elm.Syntax.File.File`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-File#File)
-}
module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    let
        maybeModuleDocumentation : Maybe (Elm.Syntax.Node.Node String)
        maybeModuleDocumentation =
            moduleDocumentation syntaxModule

        commentsAndPortDocumentationComments : { portDocumentationComments : List (Elm.Syntax.Node.Node String), comments : List (Elm.Syntax.Node.Node String) }
        commentsAndPortDocumentationComments =
            (case maybeModuleDocumentation of
                Nothing ->
                    syntaxModule.comments

                Just syntaxModuleDocumentation ->
                    syntaxModule.comments
                        |> List.filter (\c -> c /= syntaxModuleDocumentation)
            )
                |> splitOffPortDocumentationComments

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

        lastSyntaxLocationBeforeDeclarations : Elm.Syntax.Range.Location
        lastSyntaxLocationBeforeDeclarations =
            case syntaxModule.imports of
                (Elm.Syntax.Node.Node firstImportRange _) :: _ ->
                    firstImportRange.end

                [] ->
                    syntaxModule.moduleDefinition
                        |> Elm.Syntax.Node.range
                        |> .end
    in
    syntaxModule.moduleDefinition
        |> Elm.Syntax.Node.value
        |> moduleHeader { atDocsLines = atDocsLines, comments = commentsAndPortDocumentationComments.comments }
        |> Print.followedBy
            (case maybeModuleDocumentation of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ moduleDocumentationAsString) ->
                    Print.linebreak
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy (Print.symbol moduleDocumentationAsString)
            )
        |> Print.followedBy
            (case syntaxModule.imports of
                [] ->
                    Print.empty

                (Elm.Syntax.Node.Node import0Range import0) :: import1Up ->
                    (case
                        commentsInRange
                            { start =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.range
                                    |> .end
                            , end = import0Range.start
                            }
                            commentsAndPortDocumentationComments.comments
                     of
                        [] ->
                            Print.linebreak

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy Print.linebreak
                                |> Print.followedBy
                                    (moduleLevelComments (comment0 :: comment1Up))
                    )
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            ((Elm.Syntax.Node.Node import0Range import0 :: import1Up)
                                |> imports commentsAndPortDocumentationComments.comments
                            )
                        |> Print.followedBy Print.linebreak
            )
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (case syntaxModule.declarations of
                [] ->
                    -- invalid syntax
                    Print.empty

                (Elm.Syntax.Node.Node declaration0Range _) :: _ ->
                    case
                        commentsInRange
                            { start = lastSyntaxLocationBeforeDeclarations
                            , end = declaration0Range.start
                            }
                            commentsAndPortDocumentationComments.comments
                    of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            moduleLevelCommentsBeforeDeclaration
                                { comment0 = comment0, comment1Up = comment1Up }
            )
        |> Print.followedBy
            (syntaxModule.declarations
                |> declarations
                    { comments = commentsAndPortDocumentationComments.comments
                    , portDocumentationComments = commentsAndPortDocumentationComments.portDocumentationComments
                    , previousEnd = lastSyntaxLocationBeforeDeclarations
                    }
            )
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (case syntaxModule.declarations of
                [] ->
                    -- invalid syntax
                    Print.empty

                declaration0 :: declaration1Up ->
                    case
                        commentsAfter
                            (listFilledLast ( declaration0, declaration1Up )
                                |> Elm.Syntax.Node.range
                                |> .end
                            )
                            commentsAndPortDocumentationComments.comments
                    of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy Print.linebreak
                                |> Print.followedBy Print.linebreak
                                |> Print.followedBy
                                    (moduleLevelComments (comment0 :: comment1Up))
            )


{-| Resulting lists are sorted by range
-}
splitOffPortDocumentationComments :
    List (Elm.Syntax.Node.Node String)
    ->
        { portDocumentationComments : List (Elm.Syntax.Node.Node String)
        , comments : List (Elm.Syntax.Node.Node String)
        }
splitOffPortDocumentationComments commentsAndPortDocumentationComments =
    commentsAndPortDocumentationComments
        |> List.foldr
            (\commentOrPortDocumentationComments soFar ->
                if commentOrPortDocumentationComments |> Elm.Syntax.Node.value |> String.startsWith "{-|" then
                    { comments = soFar.comments
                    , portDocumentationComments = commentOrPortDocumentationComments :: soFar.portDocumentationComments
                    }

                else
                    { comments = commentOrPortDocumentationComments :: soFar.comments
                    , portDocumentationComments = soFar.portDocumentationComments
                    }
            )
            { comments = [], portDocumentationComments = [] }


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
                                    String.slice 6 (line |> String.length) line
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


commentsAfter : Elm.Syntax.Range.Location -> List (Elm.Syntax.Node.Node String) -> List String
commentsAfter end sortedComments =
    case sortedComments of
        [] ->
            []

        (Elm.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case Elm.Syntax.Range.compareLocations headCommentRange.start end of
                LT ->
                    commentsAfter end tailComments

                GT ->
                    headComment :: (tailComments |> List.map Elm.Syntax.Node.value)

                EQ ->
                    headComment :: (tailComments |> List.map Elm.Syntax.Node.value)


moduleLevelCommentsBeforeDeclaration : { comment0 : String, comment1Up : List String } -> Print
moduleLevelCommentsBeforeDeclaration syntaxComments =
    Print.linebreak
        |> Print.followedBy
            (moduleLevelComments (syntaxComments.comment0 :: syntaxComments.comment1Up))
        |> Print.followedBy
            (case listFilledLast ( syntaxComments.comment0, syntaxComments.comment1Up ) of
                "{--}" ->
                    -- don't ask me why elm-format formats it that way
                    Print.empty

                _ ->
                    Print.linebreak
                        |> Print.followedBy Print.linebreak
            )


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


lineOffsetInRange : Elm.Syntax.Range.Range -> Print.LineOffset
lineOffsetInRange range =
    if range.start.row == range.end.row then
        Print.SameLine

    else
        Print.NextLine


exposingMulti :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , expose0 : Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose
        , expose1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
        }
    -> Print
exposingMulti syntaxComments syntaxExposing =
    let
        containedComments : List String
        containedComments =
            commentsInRange syntaxExposing.fullRange syntaxComments

        lineOffset : Print.LineOffset
        lineOffset =
            case containedComments of
                _ :: _ ->
                    Print.NextLine

                [] ->
                    lineOffsetInRange syntaxExposing.fullRange
    in
    Print.symbol "("
        |> Print.followedBy
            (case lineOffset of
                Print.SameLine ->
                    Print.empty

                Print.NextLine ->
                    Print.space
            )
        |> Print.followedBy
            (Print.inSequence
                ((syntaxExposing.expose0 :: syntaxExposing.expose1Up)
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ syntaxExpose) -> expose syntaxExpose)
                    |> List.intersperse
                        (Print.emptiableLayout lineOffset
                            |> Print.followedBy (Print.symbol ",")
                            |> Print.followedBy Print.space
                        )
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy
            (case containedComments of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
            )
        |> Print.followedBy (Print.symbol ")")


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


exposeListToNormal :
    List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
exposeListToNormal syntaxExposeList =
    syntaxExposeList
        |> List.map Elm.Syntax.Node.value
        |> List.sortWith exposeCompare
        |> exposesCombine
        |> List.map Elm.Syntax.Node.empty


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

                LT ->
                    expose0 :: exposesCombine (expose1 :: expose2Up)

                GT ->
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

                Elm.Syntax.Exposing.InfixExpose _ ->
                    Elm.Syntax.Exposing.TypeExpose aTypeExpose

                Elm.Syntax.Exposing.FunctionExpose _ ->
                    Elm.Syntax.Exposing.TypeExpose aTypeExpose

                Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
                    Elm.Syntax.Exposing.TypeExpose aTypeExpose

        Elm.Syntax.Exposing.InfixExpose _ ->
            b

        Elm.Syntax.Exposing.FunctionExpose _ ->
            b

        Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
            b


{-| The stuff after `exposing` in a module header.
For import exposing: [`importExposing`](#importExposing)
-}
moduleExposing :
    { atDocsLines : List (List String), comments : List (Elm.Syntax.Node.Node String) }
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
moduleExposing context (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
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
                    let
                        containedComments : List String
                        containedComments =
                            commentsInRange exposingRange context.comments

                        lineOffset : Print.LineOffset
                        lineOffset =
                            case containedComments of
                                _ :: _ ->
                                    Print.NextLine

                                [] ->
                                    Print.SameLine
                    in
                    Print.symbol "("
                        |> Print.followedBy
                            (case lineOffset of
                                Print.SameLine ->
                                    Print.empty

                                Print.NextLine ->
                                    Print.space
                            )
                        |> Print.followedBy (expose onlySyntaxExpose)
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy
                            (case containedComments of
                                [] ->
                                    Print.empty

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                            )
                        |> Print.followedBy (Print.symbol ")")

                expose0 :: expose1 :: expose2Up ->
                    case context.atDocsLines of
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
                            case
                                atDocsExposeLines.atDocsExposeLines
                                    |> List.filter
                                        (\line ->
                                            case line of
                                                [] ->
                                                    False

                                                _ :: _ ->
                                                    True
                                        )
                            of
                                [] ->
                                    exposingMulti context.comments
                                        { fullRange = exposingRange
                                        , expose0 = expose0
                                        , expose1Up = expose1 :: expose2Up
                                        }

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
                            exposingMulti context.comments
                                { fullRange = exposingRange
                                , expose0 = expose0
                                , expose1Up = expose1 :: expose2Up
                                }


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


{-| Print an [`Elm.Syntax.Module.Module`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader :
    { atDocsLines : List (List String), comments : List (Elm.Syntax.Node.Node String) }
    -> Elm.Syntax.Module.Module
    -> Print
moduleHeader context syntaxModuleHeader =
    case syntaxModuleHeader of
        Elm.Syntax.Module.NormalModule defaultModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    defaultModuleData.exposingList
                        |> moduleExposing context

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
                        |> moduleExposing context

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
                        |> moduleExposing context

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


{-| Print a set of [`Elm.Syntax.Import.Import`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import#Import)s
-}
imports :
    List (Elm.Syntax.Node.Node String)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> Print
imports syntaxComments syntaxImports =
    case syntaxImports of
        [] ->
            Print.empty

        (Elm.Syntax.Node.Node import0Range import0) :: imports1Up ->
            let
                commentsBetweenImports : List String
                commentsBetweenImports =
                    (Elm.Syntax.Node.Node import0Range import0 :: imports1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node importRange _) soFar ->
                                { previousImportRange = importRange
                                , comments =
                                    soFar.comments
                                        ++ commentsInRange { start = soFar.previousImportRange.end, end = importRange.start }
                                            syntaxComments
                                }
                            )
                            { previousImportRange = import0Range
                            , comments = []
                            }
                        |> .comments
            in
            (case commentsBetweenImports of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    moduleLevelComments (comment0 :: comment1Up)
                        |> Print.followedBy Print.linebreak
            )
                |> Print.followedBy
                    (Print.inSequence
                        ((Elm.Syntax.Node.Node import0Range import0 :: imports1Up)
                            |> List.sortWith
                                (\(Elm.Syntax.Node.Node _ a) (Elm.Syntax.Node.Node _ b) ->
                                    compare (a.moduleName |> Elm.Syntax.Node.value) (b.moduleName |> Elm.Syntax.Node.value)
                                )
                            |> importsCombine
                            |> List.map (\syntaxImport -> import_ syntaxComments syntaxImport)
                            |> List.intersperse Print.linebreak
                        )
                    )


importsCombine :
    List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
importsCombine syntaxImports =
    case syntaxImports of
        [] ->
            []

        [ onlyImport ] ->
            [ onlyImport |> Elm.Syntax.Node.map importToNormal ]

        (Elm.Syntax.Node.Node import0Range import0) :: (Elm.Syntax.Node.Node import1Range import1) :: import2Up ->
            if (import0.moduleName |> Elm.Syntax.Node.value) == (import1.moduleName |> Elm.Syntax.Node.value) then
                importsCombine
                    (Elm.Syntax.Node.Node import1Range
                        (importsMerge import0 import1)
                        :: import2Up
                    )

            else
                Elm.Syntax.Node.Node import0Range (import0 |> importToNormal)
                    :: importsCombine (Elm.Syntax.Node.Node import1Range import1 :: import2Up)


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


exposingToNormal : Elm.Syntax.Exposing.Exposing -> Elm.Syntax.Exposing.Exposing
exposingToNormal syntaxExposing =
    case syntaxExposing of
        Elm.Syntax.Exposing.All allRange ->
            Elm.Syntax.Exposing.All allRange

        Elm.Syntax.Exposing.Explicit exposeSet ->
            Elm.Syntax.Exposing.Explicit (exposeSet |> exposeListToNormal)


importsMerge : Elm.Syntax.Import.Import -> Elm.Syntax.Import.Import -> Elm.Syntax.Import.Import
importsMerge earlier later =
    { moduleName = later.moduleName
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


{-| Print a single [`Elm.Syntax.Import.Import`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import#Import)
-}
import_ :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Import.Import
    -> Print
import_ syntaxComments (Elm.Syntax.Node.Node incorrectImportRange syntaxImport) =
    let
        importRange : Elm.Syntax.Range.Range
        importRange =
            case syntaxImport.exposingList of
                Nothing ->
                    incorrectImportRange

                Just (Elm.Syntax.Node.Node syntaxExposingRange _) ->
                    { start = incorrectImportRange.start, end = syntaxExposingRange.end }

        (Elm.Syntax.Node.Node moduleNameRange syntaxModuleName) =
            syntaxImport.moduleName
    in
    Print.symbol "import"
        |> Print.followedBy
            (case syntaxImport.moduleAlias of
                Nothing ->
                    case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                        [] ->
                            Print.space
                                |> Print.followedBy (moduleName syntaxModuleName)

                        comment0 :: comment1Up ->
                            Print.indentedByNextMultipleOf4
                                (Print.layout Print.NextLine
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy (Print.layout Print.NextLine)
                                    |> Print.followedBy (moduleName syntaxModuleName)
                                )

                Just (Elm.Syntax.Node.Node moduleAliasRange moduleAlias) ->
                    case commentsInRange { start = moduleNameRange.end, end = moduleAliasRange.start } syntaxComments of
                        [] ->
                            (case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                                [] ->
                                    Print.space
                                        |> Print.followedBy (moduleName syntaxModuleName)

                                moduleNameComment0 :: moduleNameComment1Up ->
                                    Print.indentedByNextMultipleOf4
                                        (Print.layout Print.NextLine
                                            |> Print.followedBy (comments (moduleNameComment0 :: moduleNameComment1Up))
                                            |> Print.followedBy (Print.layout Print.NextLine)
                                            |> Print.followedBy (moduleName syntaxModuleName)
                                        )
                            )
                                |> Print.followedBy Print.space
                                |> Print.followedBy (Print.symbol "as")
                                |> Print.followedBy Print.space
                                |> Print.followedBy (moduleName moduleAlias)

                        aliasComment0 :: aliasComment1Up ->
                            Print.indentedByNextMultipleOf4
                                (Print.layout Print.NextLine
                                    |> Print.followedBy
                                        (case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                                            [] ->
                                                moduleName syntaxModuleName

                                            moduleNameComment0 :: moduleNameComment1Up ->
                                                comments (moduleNameComment0 :: moduleNameComment1Up)
                                                    |> Print.followedBy (Print.layout Print.NextLine)
                                                    |> Print.followedBy (moduleName syntaxModuleName)
                                        )
                                    |> Print.followedBy
                                        (Print.indentedByNextMultipleOf4
                                            (Print.layout Print.NextLine
                                                |> Print.followedBy (Print.symbol "as")
                                                |> Print.followedBy
                                                    (Print.indentedByNextMultipleOf4
                                                        (Print.layout Print.NextLine
                                                            |> Print.followedBy (comments (aliasComment0 :: aliasComment1Up))
                                                            |> Print.followedBy (Print.layout Print.NextLine)
                                                            |> Print.followedBy (moduleName moduleAlias)
                                                        )
                                                    )
                                            )
                                        )
                                )
            )
        |> Print.followedBy
            (case syntaxImport.exposingList of
                Nothing ->
                    Print.empty

                Just syntaxExposing ->
                    let
                        exposingPrint : Print
                        exposingPrint =
                            importExposing syntaxComments syntaxExposing

                        exposingPartStart : Elm.Syntax.Range.Location
                        exposingPartStart =
                            case syntaxImport.moduleAlias of
                                Nothing ->
                                    moduleNameRange.end

                                Just (Elm.Syntax.Node.Node moduleAliasRange _) ->
                                    moduleAliasRange.end
                    in
                    case commentsInRange { start = exposingPartStart, end = importRange.end } syntaxComments of
                        [] ->
                            let
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

                        exposingComment0 :: exposingComment1Up ->
                            Print.indentedByNextMultipleOf4
                                (Print.layout Print.NextLine
                                    |> Print.followedBy (Print.symbol "exposing")
                                    |> Print.followedBy
                                        (Print.indentedByNextMultipleOf4
                                            (Print.layout Print.NextLine
                                                |> Print.followedBy (comments (exposingComment0 :: exposingComment1Up))
                                                |> Print.followedBy (Print.layout Print.NextLine)
                                                |> Print.followedBy exposingPrint
                                            )
                                        )
                                )
            )


{-| The stuff after `exposing` in an import.
For module header exposing: [`moduleExposing`](#moduleExposing)
-}
importExposing :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
importExposing syntaxComments (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Elm.Syntax.Exposing.All _ ->
            Print.symbol "("
                |> Print.followedBy (Print.symbol "..")
                |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet of
                [] ->
                    Print.symbol "()"

                expose0 :: expose1Up ->
                    exposingMulti syntaxComments
                        { fullRange = exposingRange, expose0 = expose0, expose1Up = expose1Up }


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
                )
            |> List.intersperse (Print.layout Print.NextLine)
        )


{-| `--` or `{- -}` comments placed outside of declarations at the top level.
For comments within a declaration: [`comments`](#comments)
-}
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
                                    case syntaxComment of
                                        "{--}" ->
                                            Print.linebreak
                                                |> Print.followedBy Print.linebreak
                                                |> Print.followedBy (comment "{--}")
                                                |> Print.followedBy Print.linebreak

                                        notEmptyMultiLineComment ->
                                            comment notEmptyMultiLineComment
                                                |> Print.followedBy Print.linebreak
                                )
                        )
                    )


{-| Print a single `--` or `{- -}` comment.
-}
comment : String -> Print
comment syntaxComment =
    case syntaxComment of
        "{--}" ->
            Print.symbol "{--}"

        nonDirectlyClosingMultiLineComment ->
            if nonDirectlyClosingMultiLineComment |> String.startsWith "--" then
                Print.symbol (nonDirectlyClosingMultiLineComment |> String.trimRight)

            else
                -- comment starts with {-
                let
                    commentContent : List String
                    commentContent =
                        nonDirectlyClosingMultiLineComment
                            |> -- {-
                               String.dropLeft 2
                            |> -- -}
                               String.dropRight 2
                            |> String.lines
                            |> List.map String.trim
                            |> listDropLastIfIs
                                (\line ->
                                    case line of
                                        "" ->
                                            True

                                        _ ->
                                            False
                                )
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


{-| Print an [`Elm.Syntax.ModuleName.ModuleName`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
-}
moduleName : Elm.Syntax.ModuleName.ModuleName -> Print
moduleName syntaxModuleName =
    Print.symbol (syntaxModuleName |> String.join ".")


{-| Print a single [`Elm.Syntax.Exposing.TopLevelExpose`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#TopLevelExpose)
-}
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


patternParenthesizedIfSpaceSeparated :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Print
patternParenthesizedIfSpaceSeparated syntaxComments syntaxPattern =
    if patternIsSpaceSeparated (syntaxPattern |> Elm.Syntax.Node.value) then
        let
            (Elm.Syntax.Node.Node innerRange _) =
                syntaxPattern |> patternToNotParenthesized

            innerPrint : Print
            innerPrint =
                patternNotParenthesized syntaxComments syntaxPattern

            commentsBeforeInner : List String
            commentsBeforeInner =
                commentsInRange
                    { start = (syntaxPattern |> Elm.Syntax.Node.range).start
                    , end = innerRange.start
                    }
                    syntaxComments

            commentsAfterInner : List String
            commentsAfterInner =
                commentsInRange
                    { start = innerRange.end
                    , end = (syntaxPattern |> Elm.Syntax.Node.range).end
                    }
                    syntaxComments
        in
        Print.symbol "("
            |> Print.followedBy
                (case commentsBeforeInner of
                    [] ->
                        case commentsAfterInner of
                            [] ->
                                Print.indented 1 innerPrint
                                    |> Print.followedBy
                                        (Print.layout (innerPrint |> Print.lineOffset))

                            comment0AfterInner :: comment1UpAfterInner ->
                                Print.indented 1
                                    (innerPrint
                                        |> Print.followedBy (Print.layout Print.NextLine)
                                        |> Print.followedBy (comments (comment0AfterInner :: comment1UpAfterInner))
                                    )
                                    |> Print.followedBy (Print.layout Print.NextLine)

                    comment0BeforeInner :: comment1UpBeforeInner ->
                        Print.indented 1
                            (comments (comment0BeforeInner :: comment1UpBeforeInner)
                                |> Print.followedBy (Print.layout Print.NextLine)
                                |> Print.followedBy innerPrint
                                |> Print.followedBy
                                    (case commentsAfterInner of
                                        [] ->
                                            Print.empty

                                        comment0AfterInner :: comment1UpAfterInner ->
                                            Print.layout Print.NextLine
                                                |> Print.followedBy (comments (comment0AfterInner :: comment1UpAfterInner))
                                    )
                            )
                            |> Print.followedBy (Print.layout Print.NextLine)
                )
            |> Print.followedBy (Print.symbol ")")

    else
        patternNotParenthesized syntaxComments syntaxPattern


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
                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    patternIsSpaceSeparated inParens

                [] ->
                    -- should be covered by UnitPattern
                    False

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    False

        Elm.Syntax.Pattern.RecordPattern _ ->
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

                Unicode.LetterUppercase ->
                    False

                Unicode.LetterLowercase ->
                    False

                Unicode.LetterTitlecase ->
                    False

                Unicode.MarkNonSpacing ->
                    False

                Unicode.MarkSpacingCombining ->
                    False

                Unicode.MarkEnclosing ->
                    False

                Unicode.NumberDecimalDigit ->
                    False

                Unicode.NumberLetter ->
                    False

                Unicode.NumberOther ->
                    False

                Unicode.SeparatorSpace ->
                    False

                Unicode.LetterModifier ->
                    False

                Unicode.LetterOther ->
                    False

                Unicode.PunctuationConnector ->
                    False

                Unicode.PunctuationDash ->
                    False

                Unicode.PunctuationOpen ->
                    False

                Unicode.PunctuationClose ->
                    False

                Unicode.PunctuationInitialQuote ->
                    False

                Unicode.PunctuationFinalQuote ->
                    False

                Unicode.PunctuationOther ->
                    False

                Unicode.SymbolMath ->
                    False

                Unicode.SymbolCurrency ->
                    False

                Unicode.SymbolModifier ->
                    False

                Unicode.SymbolOther ->
                    False


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


patternToNotParenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
patternToNotParenthesized (Elm.Syntax.Node.Node patternRange syntaxPattern) =
    case syntaxPattern of
        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    inParens

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1, part2 ])

                [] ->
                    -- should be covered by UnitPattern
                    Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.TuplePattern [])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up))

        Elm.Syntax.Pattern.AllPattern ->
            Elm.Syntax.Node.Node patternRange Elm.Syntax.Pattern.AllPattern

        Elm.Syntax.Pattern.UnitPattern ->
            Elm.Syntax.Node.Node patternRange Elm.Syntax.Pattern.UnitPattern

        Elm.Syntax.Pattern.VarPattern name ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.VarPattern name)

        Elm.Syntax.Pattern.CharPattern char ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.CharPattern char)

        Elm.Syntax.Pattern.StringPattern string ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.StringPattern string)

        Elm.Syntax.Pattern.IntPattern int ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.IntPattern int)

        Elm.Syntax.Pattern.HexPattern int ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.HexPattern int)

        Elm.Syntax.Pattern.FloatPattern float ->
            -- invalid syntax
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.FloatPattern float)

        Elm.Syntax.Pattern.RecordPattern fields ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.RecordPattern fields)

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.ListPattern elementPatterns)

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns)

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode ->
            Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode)


{-| Print an [`Elm.Syntax.Pattern.Pattern`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Print
patternNotParenthesized syntaxComments (Elm.Syntax.Node.Node patternRange syntaxPattern) =
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

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            patternNotParenthesized syntaxComments inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    { part0 = part0, part1 = part1, fullRange = patternRange }
                        |> patternTuple syntaxComments

                [ part0, part1, part2 ] ->
                    { part0 = part0, part1 = part1, part2 = part2, fullRange = patternRange }
                        |> patternTriple syntaxComments

                [] ->
                    -- should be covered by UnitPattern
                    Print.symbol "()"

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    patternNotParenthesized syntaxComments inParens

                part0 :: part1 :: part2 :: part3Up ->
                    -- invalid syntax
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy
                            (Print.inSequence
                                ((part0 :: part1 :: part2 :: part3Up)
                                    |> List.map
                                        (\partPattern ->
                                            patternNotParenthesized syntaxComments partPattern
                                        )
                                    |> List.intersperse
                                        (Print.emptiableLayout Print.SameLine
                                            |> Print.followedBy (Print.symbol ",")
                                            |> Print.followedBy Print.space
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
                            (Print.inSequence
                                ((field0 :: field1Up)
                                    |> List.map (\(Elm.Syntax.Node.Node _ fieldName) -> Print.symbol fieldName)
                                    |> List.intersperse
                                        (Print.emptiableLayout Print.SameLine
                                            |> Print.followedBy (Print.symbol ",")
                                            |> Print.followedBy Print.space
                                        )
                                )
                            )
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            patternParenthesizedIfSpaceSeparated syntaxComments headPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "::")
                |> Print.followedBy Print.space
                |> Print.followedBy (patternParenthesizedIfSpaceSeparated syntaxComments tailPattern)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Print.symbol "["
                |> Print.followedBy Print.space
                |> Print.followedBy
                    (Print.inSequence
                        (elementPatterns
                            |> List.map
                                (\elementPattern ->
                                    patternNotParenthesized syntaxComments elementPattern
                                )
                            |> List.intersperse
                                (Print.emptiableLayout Print.SameLine
                                    |> Print.followedBy (Print.symbol ",")
                                    |> Print.followedBy Print.space
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
                                (\argumentPattern ->
                                    Print.space
                                        |> Print.followedBy
                                            (patternParenthesizedIfSpaceSeparated syntaxComments argumentPattern)
                                )
                        )
                    )

        Elm.Syntax.Pattern.AsPattern aliasedPattern (Elm.Syntax.Node.Node _ aliasName) ->
            patternParenthesizedIfSpaceSeparated syntaxComments aliasedPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "as")
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol aliasName)


patternTuple :
    List (Elm.Syntax.Node.Node String)
    ->
        { part0 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , part1 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
patternTuple syntaxComments tuple =
    let
        part0NormalParensRange : Elm.Syntax.Range.Range
        part0NormalParensRange =
            tuple.part0 |> patternToNotParenthesized |> Elm.Syntax.Node.range

        part1NormalParensRange : Elm.Syntax.Range.Range
        part1NormalParensRange =
            tuple.part1 |> patternToNotParenthesized |> Elm.Syntax.Node.range

        beforePart0Comments : List String
        beforePart0Comments =
            commentsInRange
                { start = tuple.fullRange.start
                , end = part0NormalParensRange.start
                }
                syntaxComments

        beforePart1Comments : List String
        beforePart1Comments =
            commentsInRange
                { start = part0NormalParensRange.end
                , end = part1NormalParensRange.start
                }
                syntaxComments

        afterPart1Comments : List String
        afterPart1Comments =
            commentsInRange
                { start = part1NormalParensRange.end
                , end = tuple.fullRange.end
                }
                syntaxComments

        lineOffset : Print.LineOffset
        lineOffset =
            case ( beforePart0Comments, ( beforePart1Comments, afterPart1Comments ) ) of
                ( _ :: _, _ ) ->
                    Print.NextLine

                ( _, ( _ :: _, _ ) ) ->
                    Print.NextLine

                ( _, ( _, _ :: _ ) ) ->
                    Print.NextLine

                ( [], ( [], [] ) ) ->
                    Print.SameLine
    in
    Print.symbol "("
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                ((case beforePart0Comments of
                    [] ->
                        Print.empty

                    comment0 :: comment1Up ->
                        comments (comment0 :: comment1Up)
                            |> Print.followedBy (Print.layout Print.NextLine)
                 )
                    |> Print.followedBy (patternNotParenthesized syntaxComments tuple.part0)
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ",")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                ((case beforePart1Comments of
                    [] ->
                        Print.empty

                    comment0 :: comment1Up ->
                        comments (comment0 :: comment1Up)
                            |> Print.followedBy (Print.layout Print.NextLine)
                 )
                    |> Print.followedBy (patternNotParenthesized syntaxComments tuple.part1)
                    |> Print.followedBy
                        (case afterPart1Comments of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.layout Print.NextLine
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.layout lineOffset)
        |> Print.followedBy (Print.symbol ")")


patternTriple :
    List (Elm.Syntax.Node.Node String)
    ->
        { part0 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , part1 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , part2 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
patternTriple syntaxComments triple =
    let
        part0NormalParensRange : Elm.Syntax.Range.Range
        part0NormalParensRange =
            triple.part0 |> patternToNotParenthesized |> Elm.Syntax.Node.range

        part1NormalParensRange : Elm.Syntax.Range.Range
        part1NormalParensRange =
            triple.part1 |> patternToNotParenthesized |> Elm.Syntax.Node.range

        part2NormalParensRange : Elm.Syntax.Range.Range
        part2NormalParensRange =
            triple.part2 |> patternToNotParenthesized |> Elm.Syntax.Node.range

        beforePart0Comments : List String
        beforePart0Comments =
            commentsInRange
                { start = triple.fullRange.start
                , end = part0NormalParensRange.start
                }
                syntaxComments

        beforePart1Comments : List String
        beforePart1Comments =
            commentsInRange
                { start = part0NormalParensRange.end
                , end = part1NormalParensRange.start
                }
                syntaxComments

        beforePart2Comments : List String
        beforePart2Comments =
            commentsInRange
                { start = part1NormalParensRange.end
                , end = part2NormalParensRange.start
                }
                syntaxComments

        afterPart2Comments : List String
        afterPart2Comments =
            commentsInRange
                { start = part2NormalParensRange.end
                , end = triple.fullRange.end
                }
                syntaxComments

        lineOffset : Print.LineOffset
        lineOffset =
            case ( beforePart0Comments, ( beforePart1Comments, ( beforePart2Comments, afterPart2Comments ) ) ) of
                ( _ :: _, _ ) ->
                    Print.NextLine

                ( _, ( _ :: _, _ ) ) ->
                    Print.NextLine

                ( _, ( _, ( _ :: _, _ ) ) ) ->
                    Print.NextLine

                ( _, ( _, ( _, _ :: _ ) ) ) ->
                    Print.NextLine

                ( [], ( [], ( [], [] ) ) ) ->
                    Print.SameLine
    in
    Print.symbol "("
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                ((case beforePart0Comments of
                    [] ->
                        Print.empty

                    comment0 :: comment1Up ->
                        comments (comment0 :: comment1Up)
                            |> Print.followedBy (Print.layout Print.NextLine)
                 )
                    |> Print.followedBy (patternNotParenthesized syntaxComments triple.part0)
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ",")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                ((case beforePart1Comments of
                    [] ->
                        Print.empty

                    comment0 :: comment1Up ->
                        comments (comment0 :: comment1Up)
                            |> Print.followedBy (Print.layout Print.NextLine)
                 )
                    |> Print.followedBy (patternNotParenthesized syntaxComments triple.part1)
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ",")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                ((case beforePart2Comments of
                    [] ->
                        Print.empty

                    comment0 :: comment1Up ->
                        comments (comment0 :: comment1Up)
                            |> Print.followedBy (Print.layout Print.NextLine)
                 )
                    |> Print.followedBy (patternNotParenthesized syntaxComments triple.part2)
                    |> Print.followedBy
                        (case afterPart2Comments of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.layout Print.NextLine
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.layout lineOffset)
        |> Print.followedBy (Print.symbol ")")


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


lineOffsetBetweenNodes : Elm.Syntax.Node.Node a_ -> Elm.Syntax.Node.Node b_ -> Print.LineOffset
lineOffsetBetweenNodes (Elm.Syntax.Node.Node earlierRange _) (Elm.Syntax.Node.Node laterRange _) =
    if earlierRange.start.row == laterRange.end.row then
        Print.SameLine

    else
        Print.NextLine


lineOffsetInNode : Elm.Syntax.Node.Node a_ -> Print.LineOffset
lineOffsetInNode (Elm.Syntax.Node.Node range _) =
    lineOffsetInRange range


typeFunctionNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    ->
        { inType : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        , outType : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
typeFunctionNotParenthesized syntaxComments function =
    let
        fullLineOffset : Print.LineOffset
        fullLineOffset =
            lineOffsetBetweenNodes function.inType function.outType
    in
    typeParenthesizedIfFunction syntaxComments function.inType
        |> Print.followedBy
            (Print.inSequence
                (typeFunctionExpand function.outType
                    |> List.map
                        (\afterArrowTypeNode ->
                            Print.layout fullLineOffset
                                |> Print.followedBy (Print.symbol "->")
                                |> Print.followedBy
                                    (Print.indentedByNextMultipleOf4
                                        (Print.layout (lineOffsetInNode afterArrowTypeNode)
                                            |> Print.followedBy
                                                (typeParenthesizedIfParenthesizedFunction syntaxComments
                                                    afterArrowTypeNode
                                                )
                                        )
                                    )
                        )
                )
            )


typeParenthesizedIfParenthesizedFunction :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfParenthesizedFunction syntaxComments typeNode =
    case typeNode |> Elm.Syntax.Node.value of
        Elm.Syntax.TypeAnnotation.Tupled [ inParens ] ->
            inParens |> typeParenthesizedIfFunction syntaxComments

        _ ->
            typeNotParenthesized syntaxComments typeNode


typeParenthesizedIfFunction :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfFunction syntaxComments typeNode =
    case typeNode |> typeToFunction of
        Just function ->
            Print.symbol "("
                |> Print.followedBy
                    (Print.indented 1
                        (typeFunctionNotParenthesized syntaxComments function)
                    )
                |> Print.followedBy
                    (Print.emptiableLayout
                        (lineOffsetBetweenNodes function.inType function.outType)
                    )
                |> Print.followedBy (Print.symbol ")")

        Nothing ->
            typeNotParenthesized syntaxComments typeNode


typeToFunction :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        Maybe
            { inType : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
            , outType : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
            }
typeToFunction typeNode =
    case typeNode |> typeToNotParenthesized |> Elm.Syntax.Node.value of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Just { inType = inType, outType = outType }

        _ ->
            Nothing


{-| Remove as many parens as possible
-}
typeToNotParenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
typeToNotParenthesized (Elm.Syntax.Node.Node typeRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.GenericType name ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.GenericType name)

        Elm.Syntax.TypeAnnotation.Typed reference arguments ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Typed reference arguments)

        Elm.Syntax.TypeAnnotation.Unit ->
            Elm.Syntax.Node.Node typeRange Elm.Syntax.TypeAnnotation.Unit

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [ inParens ] ->
                    typeToNotParenthesized inParens

                [] ->
                    -- should be handled by Unit
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [])

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ part0, part1, part2 ])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled (part0 :: part1 :: part2 :: part3 :: part4Up))

        Elm.Syntax.TypeAnnotation.Record fields ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Record fields)

        Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode)

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType)


typeFunctionExpand :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeFunctionExpand typeNode =
    case typeNode of
        Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType) ->
            inType :: typeFunctionExpand outType

        typeNodeNotFunction ->
            [ typeNodeNotFunction ]


typeParenthesizedIfSpaceSeparated :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfSpaceSeparated syntaxComments typeNode =
    if typeIsSpaceSeparated (typeNode |> Elm.Syntax.Node.value) then
        let
            typePrint : Print
            typePrint =
                typeNotParenthesized syntaxComments typeNode
        in
        Print.symbol "("
            |> Print.followedBy (Print.indented 1 typePrint)
            |> Print.followedBy
                (Print.emptiableLayout (Print.lineOffset typePrint))
            |> Print.followedBy (Print.symbol ")")

    else
        typeNotParenthesized syntaxComments typeNode


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


{-| Remove as many parens as possible
-}
typeNormalizeParens :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
typeNormalizeParens (Elm.Syntax.Node.Node typeRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.GenericType name ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.GenericType name)

        Elm.Syntax.TypeAnnotation.Typed reference arguments ->
            Elm.Syntax.Node.Node typeRange
                (Elm.Syntax.TypeAnnotation.Typed reference
                    (arguments
                        |> List.map
                            (\argument ->
                                if argument |> Elm.Syntax.Node.value |> typeIsSpaceSeparated then
                                    typeNormalizeParensButKeepOneOuter argument

                                else
                                    typeNormalizeParens argument
                            )
                    )
                )

        Elm.Syntax.TypeAnnotation.Unit ->
            Elm.Syntax.Node.Node typeRange Elm.Syntax.TypeAnnotation.Unit

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [ inParens ] ->
                    typeNormalizeParens inParens

                [] ->
                    -- should be handled by Unit
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [])

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ typeNormalizeParens part0, typeNormalizeParens part1 ])

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ typeNormalizeParens part0, typeNormalizeParens part1, typeNormalizeParens part2 ])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    Elm.Syntax.Node.Node typeRange
                        (Elm.Syntax.TypeAnnotation.Tupled
                            ((part0 :: part1 :: part2 :: part3 :: part4Up)
                                |> List.map typeNormalizeParens
                            )
                        )

        Elm.Syntax.TypeAnnotation.Record fields ->
            Elm.Syntax.Node.Node typeRange
                (Elm.Syntax.TypeAnnotation.Record
                    (fields
                        |> List.map
                            (\fieldNode ->
                                fieldNode |> Elm.Syntax.Node.map typeFieldNormalizeParens
                            )
                    )
                )

        Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode ->
            Elm.Syntax.Node.Node typeRange
                (Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName
                    (additionalFieldsNode
                        |> Elm.Syntax.Node.map
                            (\additionalFields ->
                                additionalFields
                                    |> List.map
                                        (\fieldNode ->
                                            fieldNode |> Elm.Syntax.Node.map typeFieldNormalizeParens
                                        )
                            )
                    )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            typeFunctionExpand outType
                |> List.foldl
                    (\next soFar ->
                        Elm.Syntax.Node.Node
                            { start = soFar |> Elm.Syntax.Node.range |> .end
                            , end = next |> Elm.Syntax.Node.range |> .start
                            }
                            (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation soFar next)
                    )
                    inType


typeFieldNormalizeParens :
    Elm.Syntax.TypeAnnotation.RecordField
    -> Elm.Syntax.TypeAnnotation.RecordField
typeFieldNormalizeParens ( typeFieldName, typeFieldValue ) =
    ( typeFieldName, typeNormalizeParens typeFieldValue )


{-| `typeNormalizeParens` but keep one layer of parens
-}
typeNormalizeParensButKeepOneOuter :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
typeNormalizeParensButKeepOneOuter (Elm.Syntax.Node.Node typeRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [ inParens ] ->
                    Elm.Syntax.Node.Node typeRange
                        (Elm.Syntax.TypeAnnotation.Tupled [ typeNormalizeParens inParens ])

                [] ->
                    -- should be handled by Unit
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [])
                        |> typeNormalizeParens

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ part0, part1 ])
                        |> typeNormalizeParens

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ part0, part1, part2 ])
                        |> typeNormalizeParens

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled (part0 :: part1 :: part2 :: part3 :: part4Up))
                        |> typeNormalizeParens

        Elm.Syntax.TypeAnnotation.Unit ->
            Elm.Syntax.Node.Node typeRange Elm.Syntax.TypeAnnotation.Unit
                |> typeNormalizeParens

        Elm.Syntax.TypeAnnotation.GenericType name ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.GenericType name)
                |> typeNormalizeParens

        Elm.Syntax.TypeAnnotation.Typed reference arguments ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Typed reference arguments)
                |> typeNormalizeParens

        Elm.Syntax.TypeAnnotation.Record fields ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Record fields)
                |> typeNormalizeParens

        Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode)
                |> typeNormalizeParens

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType)
                |> typeNormalizeParens


{-| Print an [`Elm.Syntax.TypeAnnotation.TypeAnnotation`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments (Elm.Syntax.Node.Node fullRange syntaxType) =
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
                            (\argument -> typeParenthesizedIfSpaceSeparated syntaxComments argument)

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
                    typeNotParenthesized syntaxComments inParens

                [ part0, part1 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized syntaxComments part0))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized syntaxComments part1))
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
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized syntaxComments part0))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized syntaxComments part1))
                        |> Print.followedBy (Print.emptiableLayout lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.indented 2 (typeNotParenthesized syntaxComments part2))
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
                            (Print.inSequence
                                ((part0 :: part1 :: part2 :: part3 :: part4Up)
                                    |> List.map
                                        (\part ->
                                            Print.indented 2 (typeNotParenthesized syntaxComments part)
                                        )
                                    |> List.intersperse
                                        (Print.emptiableLayout lineOffset
                                            |> Print.followedBy (Print.symbol ",")
                                            |> Print.followedBy Print.space
                                        )
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
                            (Print.inSequence
                                ((field0 :: field1Up)
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                            Print.symbol fieldName
                                                |> Print.followedBy Print.space
                                                |> Print.followedBy (Print.symbol ":")
                                                |> Print.followedBy
                                                    (Print.indentedByNextMultipleOf4
                                                        (Print.layout (lineOffsetInNode fieldValue)
                                                            |> Print.followedBy
                                                                (typeNotParenthesized syntaxComments fieldValue)
                                                        )
                                                    )
                                        )
                                    |> List.intersperse
                                        (Print.emptiableLayout lineOffset
                                            |> Print.followedBy (Print.symbol ",")
                                            |> Print.followedBy Print.space
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
                                (Print.inSequence
                                    (fields
                                        |> List.map
                                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                                Print.symbol fieldName
                                                    |> Print.followedBy Print.space
                                                    |> Print.followedBy (Print.symbol ":")
                                                    |> Print.followedBy
                                                        (Print.indentedByNextMultipleOf4
                                                            (Print.layout (lineOffsetInNode fieldValue)
                                                                |> Print.followedBy (typeNotParenthesized syntaxComments fieldValue)
                                                            )
                                                        )
                                            )
                                        |> List.intersperse
                                            (Print.emptiableLayout lineOffset
                                                |> Print.followedBy (Print.symbol ",")
                                                |> Print.followedBy Print.space
                                            )
                                    )
                                )
                        )
                    )
                |> Print.followedBy (Print.layout lineOffset)
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            { inType = inType, outType = outType }
                |> typeFunctionNotParenthesized syntaxComments


{-| Print a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
and comments in between
-}
declarations :
    { portDocumentationComments : List (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    , previousEnd : Elm.Syntax.Range.Location
    }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)
    -> Print
declarations context syntaxDeclarations =
    case syntaxDeclarations of
        [] ->
            -- invalid syntax
            Print.empty

        (Elm.Syntax.Node.Node declaration0Range declaration0) :: declarations1Up ->
            declaration
                { comments = context.comments
                , portDocumentationComment =
                    case declaration0 of
                        Elm.Syntax.Declaration.PortDeclaration _ ->
                            firstCommentInRange { start = context.previousEnd, end = declaration0Range.start } context.portDocumentationComments

                        _ ->
                            Nothing
                }
                declaration0
                |> Print.followedBy
                    (declarations1Up
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node declarationRange syntaxDeclaration) soFar ->
                                let
                                    maybeDeclarationPortDocumentationComment : Maybe (Elm.Syntax.Node.Node String)
                                    maybeDeclarationPortDocumentationComment =
                                        case syntaxDeclaration of
                                            Elm.Syntax.Declaration.PortDeclaration _ ->
                                                firstCommentInRange { start = soFar.previousRange.end, end = declarationRange.start } context.portDocumentationComments

                                            _ ->
                                                Nothing
                                in
                                { print =
                                    soFar.print
                                        |> Print.followedBy
                                            (case commentsInRange { start = soFar.previousRange.end, end = declarationRange.start } context.comments of
                                                comment0 :: comment1Up ->
                                                    Print.linebreak
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy
                                                            (moduleLevelCommentsBeforeDeclaration
                                                                { comment0 = comment0, comment1Up = comment1Up }
                                                            )
                                                        |> Print.followedBy
                                                            (declaration
                                                                { comments = context.comments
                                                                , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                                }
                                                                syntaxDeclaration
                                                            )

                                                [] ->
                                                    linebreaksFollowedByDeclaration
                                                        { comments = context.comments
                                                        , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                        }
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


firstCommentInRange :
    Elm.Syntax.Range.Range
    -> List (Elm.Syntax.Node.Node String)
    -> Maybe (Elm.Syntax.Node.Node String)
firstCommentInRange range sortedComments =
    case sortedComments of
        [] ->
            Nothing

        (Elm.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case Elm.Syntax.Range.compareLocations headCommentRange.start range.start of
                LT ->
                    firstCommentInRange range tailComments

                EQ ->
                    Just (Elm.Syntax.Node.Node headCommentRange headComment)

                GT ->
                    case Elm.Syntax.Range.compareLocations headCommentRange.end range.end of
                        GT ->
                            Nothing

                        LT ->
                            Just (Elm.Syntax.Node.Node headCommentRange headComment)

                        EQ ->
                            Just (Elm.Syntax.Node.Node headCommentRange headComment)


linebreaksFollowedByDeclaration :
    { portDocumentationComment : Maybe (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Declaration.Declaration
    -> Print
linebreaksFollowedByDeclaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        Elm.Syntax.Declaration.FunctionDeclaration syntaxExpressionDeclaration ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationExpression syntaxComments.comments syntaxExpressionDeclaration)

        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration)

        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy (declarationChoiceType syntaxComments.comments syntaxChoiceTypeDeclaration)

        Elm.Syntax.Declaration.PortDeclaration signature ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (declarationPort
                        { documentationComment = syntaxComments.portDocumentationComment
                        , comments = syntaxComments.comments
                        }
                        signature
                    )

        Elm.Syntax.Declaration.InfixDeclaration syntaxInfixDeclaration ->
            Print.linebreak
                |> Print.followedBy (declarationInfix syntaxInfixDeclaration)

        Elm.Syntax.Declaration.Destructuring destructuringPattern destructuringExpression ->
            -- invalid syntax
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (declarationDestructuring syntaxComments.comments destructuringPattern destructuringExpression)


listFilledLast : ( a, List a ) -> a
listFilledLast ( head, tail ) =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast ( tailHead, tailTail )


declarationDestructuring :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
declarationDestructuring syntaxComments destructuringPattern destructuringExpression =
    -- invalid syntax
    patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "=")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (expressionNotParenthesized [] destructuringExpression)
                )
            )


{-| Print an [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)
-}
declaration :
    { portDocumentationComment : Maybe (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Declaration.Declaration
    -> Print
declaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        Elm.Syntax.Declaration.FunctionDeclaration syntaxExpressionDeclaration ->
            declarationExpression syntaxComments.comments syntaxExpressionDeclaration

        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
            declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration

        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
            declarationChoiceType syntaxComments.comments syntaxChoiceTypeDeclaration

        Elm.Syntax.Declaration.PortDeclaration signature ->
            declarationPort
                { documentationComment = syntaxComments.portDocumentationComment
                , comments = syntaxComments.comments
                }
                signature

        Elm.Syntax.Declaration.InfixDeclaration syntaxInfixDeclaration ->
            declarationInfix syntaxInfixDeclaration

        Elm.Syntax.Declaration.Destructuring destructuringPattern destructuringExpression ->
            -- invalid syntax
            declarationDestructuring syntaxComments.comments destructuringPattern destructuringExpression


declarationSignature :
    --3
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Signature.Signature
    -> Print
declarationSignature syntaxComments signature =
    let
        typePrint : Print
        typePrint =
            typeNotParenthesized syntaxComments signature.typeAnnotation

        typeNormal : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        typeNormal =
            signature.typeAnnotation |> typeNormalizeParens

        rangeBetweenNameAndType : Elm.Syntax.Range.Range
        rangeBetweenNameAndType =
            { start = signature.name |> Elm.Syntax.Node.range |> .end
            , end = typeNormal |> Elm.Syntax.Node.range |> .start
            }
    in
    Print.symbol (signature.name |> Elm.Syntax.Node.value)
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol ":")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                ((case commentsInRange rangeBetweenNameAndType syntaxComments of
                    [] ->
                        Print.layout (Print.lineOffset typePrint)

                    comment0 :: comment1Up ->
                        Print.layout Print.NextLine
                            |> Print.followedBy
                                (comments (comment0 :: comment1Up))
                            |> Print.followedBy (Print.layout Print.NextLine)
                 )
                    |> Print.followedBy typePrint
                )
            )


{-| Print a `port` [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)
-}
declarationPort :
    { documentationComment : Maybe (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Signature.Signature
    -> Print
declarationPort syntaxComments signature =
    (case syntaxComments.documentationComment of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.symbol documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                signature.name
                                    |> Elm.Syntax.Node.range
                                    |> .start
                            }
                            syntaxComments.comments
                        )
                    )
    )
        |> Print.followedBy (Print.symbol "port")
        |> Print.followedBy Print.space
        |> Print.followedBy (declarationSignature syntaxComments.comments signature)


{-| Print an [`Elm.Syntax.TypeAlias.TypeAlias`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration =
    let
        parameterPrints : List Print
        parameterPrints =
            syntaxTypeAliasDeclaration.generics
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterPattern |> Elm.Syntax.Node.range
                        in
                        { prints =
                            ((case
                                commentsInRange
                                    { start = soFar.end, end = parameterPrintedRange.start }
                                    syntaxComments
                              of
                                [] ->
                                    Print.empty

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.layout Print.NextLine)
                             )
                                |> Print.followedBy
                                    (Print.symbol (parameterPattern |> Elm.Syntax.Node.value))
                            )
                                :: soFar.prints
                        , end = parameterPrintedRange.end
                        }
                    )
                    { prints = []
                    , end =
                        syntaxTypeAliasDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }
                |> .prints
                |> List.reverse

        parametersLineOffset : Print.LineOffset
        parametersLineOffset =
            Print.listCombineLineOffset (parameterPrints |> List.map Print.lineOffset)

        typeNotParenthesizedRange : Elm.Syntax.Range.Range
        typeNotParenthesizedRange =
            syntaxTypeAliasDeclaration.typeAnnotation
                |> typeNormalizeParens
                |> Elm.Syntax.Node.range

        rangeBetweenParametersAndType : Elm.Syntax.Range.Range
        rangeBetweenParametersAndType =
            case syntaxTypeAliasDeclaration.generics of
                [] ->
                    { start =
                        syntaxTypeAliasDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    , end = typeNotParenthesizedRange.start
                    }

                parameter0 :: parameter1Up ->
                    { start =
                        listFilledLast ( parameter0, parameter1Up )
                            |> Elm.Syntax.Node.range
                            |> .end
                    , end = typeNotParenthesizedRange.start
                    }
    in
    (case syntaxTypeAliasDeclaration.documentation of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.symbol documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                syntaxTypeAliasDeclaration.name
                                    |> Elm.Syntax.Node.range
                                    |> .start
                            }
                            syntaxComments
                        )
                    )
    )
        |> Print.followedBy (Print.symbol "type")
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "alias")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout parametersLineOffset
                    |> Print.followedBy
                        (Print.symbol (syntaxTypeAliasDeclaration.name |> Elm.Syntax.Node.value))
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
                    |> Print.followedBy (Print.layout parametersLineOffset)
                    |> Print.followedBy (Print.symbol "=")
                    |> Print.followedBy
                        (Print.layout Print.NextLine
                            |> Print.followedBy
                                (case commentsInRange rangeBetweenParametersAndType syntaxComments of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy (Print.layout Print.NextLine)
                                )
                            |> Print.followedBy
                                (typeNotParenthesized syntaxComments
                                    syntaxTypeAliasDeclaration.typeAnnotation
                                )
                        )
                )
            )


{-| Print an [`Elm.Syntax.Type.Type`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Type#Type) declaration
-}
declarationChoiceType :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Type.Type
    -> Print
declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration =
    let
        parameterPrints : List Print
        parameterPrints =
            syntaxChoiceTypeDeclaration.generics
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterPattern |> Elm.Syntax.Node.range
                        in
                        { prints =
                            ((case
                                commentsInRange
                                    { start = soFar.end, end = parameterPrintedRange.start }
                                    syntaxComments
                              of
                                [] ->
                                    Print.empty

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.layout Print.NextLine)
                             )
                                |> Print.followedBy
                                    (Print.symbol (parameterPattern |> Elm.Syntax.Node.value))
                            )
                                :: soFar.prints
                        , end = parameterPrintedRange.end
                        }
                    )
                    { prints = []
                    , end =
                        syntaxChoiceTypeDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }
                |> .prints
                |> List.reverse

        parametersLineOffset : Print.LineOffset
        parametersLineOffset =
            Print.listCombineLineOffset (parameterPrints |> List.map Print.lineOffset)

        endBeforeVariants : Elm.Syntax.Range.Location
        endBeforeVariants =
            case syntaxChoiceTypeDeclaration.generics of
                [] ->
                    syntaxChoiceTypeDeclaration.name
                        |> Elm.Syntax.Node.range
                        |> .end

                parameter0 :: parameter1Up ->
                    listFilledLast ( parameter0, parameter1Up )
                        |> Elm.Syntax.Node.range
                        |> .end

        variantPrints : List Print
        variantPrints =
            syntaxChoiceTypeDeclaration.constructors
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ variant) soFar ->
                        let
                            variantParameterPrints : List Print
                            variantParameterPrints =
                                variant.arguments
                                    |> List.map
                                        (\parameter ->
                                            typeParenthesizedIfSpaceSeparated syntaxComments parameter
                                        )

                            variantParametersLineOffset : Print.LineOffset
                            variantParametersLineOffset =
                                Print.listCombineLineOffset (variantParameterPrints |> List.map Print.lineOffset)

                            variantPrint : Print
                            variantPrint =
                                (case commentsInRange { start = soFar.end, end = variant.name |> Elm.Syntax.Node.range |> .start } syntaxComments of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy (Print.layout Print.NextLine)
                                )
                                    |> Print.followedBy
                                        (Print.symbol (variant.name |> Elm.Syntax.Node.value)
                                            |> Print.followedBy
                                                (Print.indentedByNextMultipleOf4
                                                    (Print.inSequence
                                                        (variantParameterPrints
                                                            |> List.map
                                                                (\parameterPrint ->
                                                                    Print.layout variantParametersLineOffset
                                                                        |> Print.followedBy parameterPrint
                                                                )
                                                        )
                                                    )
                                                )
                                        )
                        in
                        { prints = variantPrint :: soFar.prints
                        , end =
                            case variant.arguments of
                                [] ->
                                    variant.name |> Elm.Syntax.Node.range |> .end

                                variantParameter0 :: variantParameter1Up ->
                                    listFilledLast ( variantParameter0, variantParameter1Up )
                                        |> typeNormalizeParens
                                        |> Elm.Syntax.Node.range
                                        |> .end
                        }
                    )
                    { prints = []
                    , end = endBeforeVariants
                    }
                |> .prints
                |> List.reverse
    in
    (case syntaxChoiceTypeDeclaration.documentation of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.symbol documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                syntaxChoiceTypeDeclaration.name
                                    |> Elm.Syntax.Node.range
                                    |> .start
                            }
                            syntaxComments
                        )
                    )
    )
        |> Print.followedBy (Print.symbol "type")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout parametersLineOffset
                    |> Print.followedBy
                        (Print.symbol (syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value))
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
                    |> Print.followedBy
                        (Print.layout Print.NextLine)
                    |> Print.followedBy (Print.symbol "=")
                    |> Print.followedBy Print.space
                    |> Print.followedBy
                        (Print.inSequence
                            (variantPrints
                                |> List.map (\variantPrint -> Print.indented 2 variantPrint)
                                |> List.intersperse
                                    (Print.layout Print.NextLine
                                        |> Print.followedBy (Print.symbol "|")
                                        |> Print.followedBy Print.space
                                    )
                            )
                        )
                )
            )


{-| Print an [`Elm.Syntax.Infix.Infix`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Infix#Infix) declaration
-}
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


declarationExpressionImplementation :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.FunctionImplementation
    -> Print
declarationExpressionImplementation syntaxComments implementation =
    let
        parameterPrints : List Print
        parameterPrints =
            implementation.arguments
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                if parameterPattern |> Elm.Syntax.Node.value |> patternIsSpaceSeparated then
                                    parameterPattern |> Elm.Syntax.Node.range

                                else
                                    parameterPattern |> patternToNotParenthesized |> Elm.Syntax.Node.range
                        in
                        { prints =
                            ((case
                                commentsInRange
                                    { start = soFar.end, end = parameterPrintedRange.start }
                                    syntaxComments
                              of
                                [] ->
                                    Print.empty

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.layout Print.NextLine)
                             )
                                |> Print.followedBy
                                    (patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern)
                            )
                                :: soFar.prints
                        , end = parameterPrintedRange.end
                        }
                    )
                    { prints = []
                    , end =
                        implementation.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }
                |> .prints
                |> List.reverse

        parametersLineOffset : Print.LineOffset
        parametersLineOffset =
            Print.listCombineLineOffset (parameterPrints |> List.map Print.lineOffset)

        rangeBetweenParametersAndResult : Elm.Syntax.Range.Range
        rangeBetweenParametersAndResult =
            case implementation.arguments of
                [] ->
                    { start =
                        implementation.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    , end =
                        implementation.expression
                            |> Elm.Syntax.Node.range
                            |> .start
                    }

                parameter0 :: parameter1Up ->
                    { start =
                        listFilledLast ( parameter0, parameter1Up )
                            |> Elm.Syntax.Node.range
                            |> .end
                    , end =
                        implementation.expression
                            |> Elm.Syntax.Node.range
                            |> .start
                    }
    in
    Print.symbol (implementation.name |> Elm.Syntax.Node.value)
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout parametersLineOffset
                    |> Print.followedBy
                        (Print.inSequence
                            (parameterPrints
                                |> List.map
                                    (\parameterPrint ->
                                        parameterPrint
                                            |> Print.followedBy (Print.layout parametersLineOffset)
                                    )
                            )
                        )
                    |> Print.followedBy (Print.symbol "=")
                    |> Print.followedBy
                        (Print.layout Print.NextLine
                            |> Print.followedBy
                                (case commentsInRange rangeBetweenParametersAndResult syntaxComments of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy (Print.layout Print.NextLine)
                                )
                            |> Print.followedBy
                                (expressionNotParenthesized syntaxComments
                                    implementation.expression
                                )
                        )
                )
            )


commentsBetweenDocumentationAndDeclaration : List String -> Print
commentsBetweenDocumentationAndDeclaration syntaxComments =
    case syntaxComments of
        [] ->
            Print.empty

        comment0 :: comment1Up ->
            Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (moduleLevelComments (comment0 :: comment1Up))
                |> Print.followedBy Print.linebreak
                |> Print.followedBy Print.linebreak


{-| Print an [`Elm.Syntax.Expression.Function`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.Function
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration =
    let
        maybeSignaturePrint : Print
        maybeSignaturePrint =
            case syntaxExpressionDeclaration.signature of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ signature) ->
                    declarationSignature syntaxComments signature
                        |> Print.followedBy Print.linebreak
    in
    (case syntaxExpressionDeclaration.documentation of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.symbol documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                case syntaxExpressionDeclaration.signature of
                                    Nothing ->
                                        syntaxExpressionDeclaration.declaration |> Elm.Syntax.Node.range |> .start

                                    Just (Elm.Syntax.Node.Node signatureRange _) ->
                                        signatureRange.start
                            }
                            syntaxComments
                        )
                    )
    )
        |> Print.followedBy maybeSignaturePrint
        |> Print.followedBy
            (declarationExpressionImplementation
                syntaxComments
                (syntaxExpressionDeclaration.declaration |> Elm.Syntax.Node.value)
            )


expressionParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesized syntaxComments expressionNode =
    let
        expressionPrint : Print
        expressionPrint =
            expressionNotParenthesized syntaxComments expressionNode
    in
    Print.symbol "("
        |> Print.followedBy (Print.indented 1 expressionPrint)
        |> Print.followedBy
            (Print.emptiableLayout (Print.lineOffset expressionPrint))
        |> Print.followedBy (Print.symbol ")")


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


expressionParenthesizedIfSpaceSeparated :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparated syntaxComments expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> Elm.Syntax.Node.value) then
        expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


{-| Print an [`Elm.Syntax.Expression.Expression`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Expression)
-}
expressionNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionNotParenthesized syntaxComments (Elm.Syntax.Node.Node fullRange syntaxExpression) =
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
                    expressionNotParenthesized syntaxComments notAppliedAfterAll

                applied :: argument0 :: argument1Up ->
                    expressionCall syntaxComments
                        { fullRange = fullRange
                        , applied = applied
                        , argument0 = argument0
                        , argument1Up = argument1Up
                        }

        Elm.Syntax.Expression.OperatorApplication operator _ left right ->
            expressionOperation syntaxComments
                { fullRange = fullRange
                , operator = operator
                , left = left
                , right = right
                }

        Elm.Syntax.Expression.FunctionOrValue qualification unqualified ->
            qualifiedTuple ( qualification, unqualified )

        Elm.Syntax.Expression.IfBlock condition onTrue onFalse ->
            expressionIfThenElse syntaxComments condition onTrue onFalse

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
            cappedFloat float

        Elm.Syntax.Expression.Negation negated ->
            Print.symbol "-"
                |> Print.followedBy
                    (expressionParenthesizedIfSpaceSeparated syntaxComments
                        negated
                    )

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
                    expressionNotParenthesized syntaxComments inParens

                [ part0, part1 ] ->
                    expressionTuple syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1 }

                [ part0, part1, part2 ] ->
                    expressionTriple syntaxComments
                        { fullRange = fullRange
                        , part0 = part0
                        , part1 = part1
                        , part2 = part2
                        }

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
                            (Print.inSequence
                                ((part0 :: part1 :: part2 :: part3 :: part4Up)
                                    |> List.map
                                        (\part ->
                                            Print.indented 2
                                                (expressionNotParenthesized syntaxComments part)
                                        )
                                    |> List.intersperse
                                        (Print.emptiableLayout lineOffset
                                            |> Print.followedBy (Print.symbol ",")
                                            |> Print.followedBy Print.space
                                        )
                                )
                            )
                        |> Print.followedBy (Print.layout lineOffset)
                        |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            expressionNotParenthesized syntaxComments inParens

        Elm.Syntax.Expression.LetExpression syntaxLetIn ->
            expressionLetIn syntaxComments syntaxLetIn

        Elm.Syntax.Expression.CaseExpression syntaxCaseOf ->
            expressionCaseOf syntaxComments syntaxCaseOf

        Elm.Syntax.Expression.LambdaExpression syntaxLambda ->
            expressionLambda syntaxComments (Elm.Syntax.Node.Node fullRange syntaxLambda)

        Elm.Syntax.Expression.RecordExpr fields ->
            expressionRecord syntaxComments { fullRange = fullRange, fields = fields }

        Elm.Syntax.Expression.ListExpr elements ->
            expressionList syntaxComments { fullRange = fullRange, elements = elements }

        Elm.Syntax.Expression.RecordAccess record (Elm.Syntax.Node.Node _ accessedFieldName) ->
            expressionParenthesizedIfSpaceSeparated syntaxComments record
                |> Print.followedBy (Print.symbol ".")
                |> Print.followedBy (Print.symbol accessedFieldName)

        Elm.Syntax.Expression.RecordAccessFunction dotFieldName ->
            Print.symbol dotFieldName

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ recordVariable) fields ->
            expressionRecordUpdate syntaxComments
                { fullRange = fullRange
                , recordVariable = recordVariable
                , fields = fields
                }

        Elm.Syntax.Expression.GLSLExpression glsl ->
            Print.symbol glsl


cappedFloat : Float -> Print
cappedFloat float =
    if (float |> Basics.truncate |> Basics.toFloat) == float then
        Print.symbol (String.fromFloat float)
            |> Print.followedBy (Print.symbol ".0")

    else
        Print.symbol (String.fromFloat float)


expressionCall :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , applied : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , argument0 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , argument1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
        }
    -> Print
expressionCall syntaxComments syntaxCall =
    let
        argument1UpLineOffset : Print.LineOffset
        argument1UpLineOffset =
            lineOffsetInRange syntaxCall.fullRange
    in
    expressionParenthesizedIfSpaceSeparated syntaxComments syntaxCall.applied
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout (lineOffsetBetweenNodes syntaxCall.applied syntaxCall.argument0)
                    |> Print.followedBy
                        (expressionParenthesizedIfSpaceSeparated syntaxComments
                            syntaxCall.argument0
                        )
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxCall.argument1Up
                                |> List.map
                                    (\argument ->
                                        Print.layout argument1UpLineOffset
                                            |> Print.followedBy
                                                (expressionParenthesizedIfSpaceSeparated syntaxComments
                                                    argument
                                                )
                                    )
                            )
                        )
                )
            )


expressionOperation :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , left : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , operator : String
        , right : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> { indent : Int }
    -> String
expressionOperation syntaxComments syntaxOperation =
    let
        lineOffset : Print.LineOffset
        lineOffset =
            lineOffsetInRange syntaxOperation.fullRange

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
            expressionOperationExpand syntaxOperation.left syntaxOperation.operator syntaxOperation.right

        leftestPrint : Print
        leftestPrint =
            expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                operationExpanded.leftest
    in
    leftestPrint
        |> Print.followedBy
            (List.foldr
                (\operatorExpression chainRightPrint ->
                    \previousLineOffset ->
                        let
                            expressionPrint : Print
                            expressionPrint =
                                expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                    operatorExpression.expression
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
                                    expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments
                                        operationExpanded.rightestExpression
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
                                    expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                        operationExpanded.rightestExpression
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
        rightExpanded :
            { beforeRightestOperatorExpressionChain :
                List { operator : String, expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression }
            , rightestOperator : String
            , rightestExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
            }
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

                rightNotOperation ->
                    { beforeRightestOperatorExpressionChain = []
                    , rightestOperator = operator
                    , rightestExpression = rightNotOperation
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

        leftNotOperation ->
            { leftest = leftNotOperation
            , beforeRightestOperatorExpressionChain = rightExpanded.beforeRightestOperatorExpressionChain
            , rightestOperator = rightExpanded.rightestOperator
            , rightestExpression = rightExpanded.rightestExpression
            }


expressionParenthesizedIfSpaceSeparatedExceptApplication :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments expressionNode =
    let
        (Elm.Syntax.Node.Node _ syntaxExpression) =
            expressionNode
    in
    if expressionIsSpaceSeparated syntaxExpression then
        case syntaxExpression |> expressionToNotParenthesized of
            Elm.Syntax.Expression.Application _ ->
                expressionNotParenthesized syntaxComments expressionNode

            _ ->
                expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments expressionNode =
    let
        (Elm.Syntax.Node.Node _ syntaxExpression) =
            expressionNode
    in
    if expressionIsSpaceSeparated syntaxExpression then
        case syntaxExpression |> expressionToNotParenthesized of
            Elm.Syntax.Expression.Application _ ->
                expressionNotParenthesized syntaxComments expressionNode

            Elm.Syntax.Expression.LambdaExpression _ ->
                expressionNotParenthesized syntaxComments expressionNode

            _ ->
                expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionTriple :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , part1 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , part2 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Print
expressionTriple syntaxComments syntaxTuple =
    let
        lineOffset : Print.LineOffset
        lineOffset =
            lineOffsetInRange syntaxTuple.fullRange
    in
    Print.symbol "("
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                (expressionNotParenthesized syntaxComments
                    syntaxTuple.part0
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ",")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                (expressionNotParenthesized syntaxComments
                    syntaxTuple.part1
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ",")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                (expressionNotParenthesized syntaxComments
                    syntaxTuple.part2
                )
            )
        |> Print.followedBy (Print.layout lineOffset)
        |> Print.followedBy (Print.symbol ")")


expressionTuple :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , part1 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Print
expressionTuple syntaxComments syntaxTuple =
    let
        lineOffset : Print.LineOffset
        lineOffset =
            lineOffsetInRange syntaxTuple.fullRange
    in
    Print.symbol "("
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                (expressionNotParenthesized syntaxComments
                    syntaxTuple.part0
                )
            )
        |> Print.followedBy (Print.emptiableLayout lineOffset)
        |> Print.followedBy (Print.symbol ",")
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.indented 2
                (expressionNotParenthesized syntaxComments
                    syntaxTuple.part1
                )
            )
        |> Print.followedBy (Print.layout lineOffset)
        |> Print.followedBy (Print.symbol ")")


expressionList :
    List (Elm.Syntax.Node.Node String)
    ->
        { elements : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
expressionList syntaxComments syntaxList =
    case syntaxList.elements of
        [] ->
            Print.symbol "[]"

        element0 :: element1Up ->
            let
                lineOffset : Print.LineOffset
                lineOffset =
                    lineOffsetInRange syntaxList.fullRange
            in
            Print.symbol "["
                |> Print.followedBy Print.space
                |> Print.followedBy
                    (Print.inSequence
                        ((element0 :: element1Up)
                            |> List.map
                                (\element ->
                                    Print.indented 2
                                        (expressionNotParenthesized syntaxComments
                                            element
                                        )
                                )
                            |> List.intersperse
                                (Print.emptiableLayout lineOffset
                                    |> Print.followedBy (Print.symbol ",")
                                    |> Print.followedBy Print.space
                                )
                        )
                    )
                |> Print.followedBy (Print.layout lineOffset)
                |> Print.followedBy (Print.symbol "]")


expressionRecord :
    List (Elm.Syntax.Node.Node String)
    ->
        { fields : List (Elm.Syntax.Node.Node ( Elm.Syntax.Node.Node String, Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression ))
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
expressionRecord syntaxComments syntaxRecord =
    case syntaxRecord.fields of
        [] ->
            Print.symbol "{}"

        field0 :: field1Up ->
            let
                lineOffset : Print.LineOffset
                lineOffset =
                    lineOffsetInRange syntaxRecord.fullRange
            in
            Print.symbol "{"
                |> Print.followedBy Print.space
                |> Print.followedBy
                    (Print.inSequence
                        ((field0 :: field1Up)
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                    Print.symbol fieldName
                                        |> Print.followedBy Print.space
                                        |> Print.followedBy (Print.symbol "=")
                                        |> Print.followedBy
                                            (Print.indentedByNextMultipleOf4
                                                (Print.layout (lineOffsetInNode fieldValue)
                                                    |> Print.followedBy
                                                        (expressionNotParenthesized syntaxComments
                                                            fieldValue
                                                        )
                                                )
                                            )
                                )
                            |> List.intersperse
                                (Print.emptiableLayout lineOffset
                                    |> Print.followedBy (Print.symbol ",")
                                    |> Print.followedBy Print.space
                                )
                        )
                    )
                |> Print.followedBy (Print.layout lineOffset)
                |> Print.followedBy (Print.symbol "}")


expressionRecordUpdate :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , recordVariable : String
        , fields : List (Elm.Syntax.Node.Node ( Elm.Syntax.Node.Node String, Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression ))
        }
    -> Print
expressionRecordUpdate syntaxComments syntaxRecordUpdate =
    let
        lineOffset : Print.LineOffset
        lineOffset =
            lineOffsetInRange syntaxRecordUpdate.fullRange
    in
    Print.symbol "{"
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol syntaxRecordUpdate.recordVariable)
        |> Print.followedBy (Print.layout lineOffset)
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.symbol "|"
                    |> Print.followedBy Print.space
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxRecordUpdate.fields
                                |> List.map
                                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValue )) ->
                                        Print.symbol fieldName
                                            |> Print.followedBy Print.space
                                            |> Print.followedBy (Print.symbol "=")
                                            |> Print.followedBy
                                                (Print.indentedByNextMultipleOf4
                                                    (Print.layout (lineOffsetInNode fieldValue)
                                                        |> Print.followedBy
                                                            (expressionNotParenthesized syntaxComments
                                                                fieldValue
                                                            )
                                                    )
                                                )
                                    )
                                |> List.intersperse
                                    (Print.emptiableLayout lineOffset
                                        |> Print.followedBy (Print.symbol ",")
                                        |> Print.followedBy Print.space
                                    )
                            )
                        )
                )
            )
        |> Print.followedBy (Print.layout lineOffset)
        |> Print.followedBy (Print.symbol "}")


expressionLambda :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Lambda
    -> Print
expressionLambda syntaxComments (Elm.Syntax.Node.Node lambdaRange syntaxLambda) =
    Print.symbol "\\"
        |> Print.followedBy
            (Print.inSequence
                (syntaxLambda.args
                    |> List.map
                        (\parameterPattern ->
                            patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
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
            (expressionNotParenthesized syntaxComments
                syntaxLambda.expression
            )


expressionIfThenElse :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionIfThenElse syntaxComments condition onTrue onFalse =
    let
        conditionPrint : Print
        conditionPrint =
            expressionNotParenthesized syntaxComments condition

        conditionLineOffset : Print.LineOffset
        conditionLineOffset =
            Print.lineOffset conditionPrint

        onTruePrint : Print
        onTruePrint =
            expressionNotParenthesized syntaxComments onTrue
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
                            (expressionIfThenElse syntaxComments
                                onFalseCondition
                                onFalseOnTrue
                                onFalseOnFalse
                            )

                _ ->
                    let
                        onFalsePrint : Print
                        onFalsePrint =
                            expressionNotParenthesized syntaxComments onFalse
                    in
                    Print.indentedByNextMultipleOf4
                        (Print.layout Print.NextLine
                            |> Print.followedBy onFalsePrint
                        )
            )


expressionCaseOf :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.CaseBlock
    -> Print
expressionCaseOf syntaxComments syntaxCaseOf =
    let
        casedExpressionLineOffset : Print.LineOffset
        casedExpressionLineOffset =
            lineOffsetInNode syntaxCaseOf.expression
    in
    Print.symbol "case"
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout casedExpressionLineOffset
                    |> Print.followedBy
                        (expressionNotParenthesized syntaxComments
                            syntaxCaseOf.expression
                        )
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
                                |> List.map (\syntaxCase -> case_ syntaxComments syntaxCase)
                                |> List.intersperse
                                    (Print.linebreak
                                        |> Print.followedBy (Print.layout Print.NextLine)
                                    )
                            )
                        )
                )
            )


expressionLetIn :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.LetBlock
    -> Print
expressionLetIn syntaxComments syntaxLetIn =
    Print.symbol "let"
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxLetIn.declarations
                                |> List.map
                                    (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                        expressionLetDeclaration syntaxComments letDeclaration
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
        |> Print.followedBy (expressionNotParenthesized syntaxComments syntaxLetIn.expression)


expressionLetDeclaration :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.LetDeclaration
    -> Print
expressionLetDeclaration syntaxComments letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction letDeclarationExpression ->
            let
                maybeSignaturePrint : Print
                maybeSignaturePrint =
                    case letDeclarationExpression.signature of
                        Nothing ->
                            Print.empty

                        Just (Elm.Syntax.Node.Node _ signature) ->
                            declarationSignature syntaxComments signature
                                |> Print.followedBy (Print.layout Print.NextLine)
            in
            maybeSignaturePrint
                |> Print.followedBy
                    (declarationExpressionImplementation syntaxComments
                        (letDeclarationExpression.declaration |> Elm.Syntax.Node.value)
                    )

        Elm.Syntax.Expression.LetDestructuring destructuringPattern destructuredExpression ->
            patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "=")
                |> Print.followedBy
                    (Print.indentedByNextMultipleOf4
                        (Print.layout Print.NextLine
                            |> Print.followedBy
                                (expressionNotParenthesized syntaxComments
                                    destructuredExpression
                                )
                        )
                    )


expressionToNotParenthesized : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
expressionToNotParenthesized syntaxExpression =
    case syntaxExpression of
        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ inParens) ->
            inParens

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [ Elm.Syntax.Node.Node _ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    inParens

                [] ->
                    Elm.Syntax.Expression.TupledExpression []

                [ part0, part1 ] ->
                    Elm.Syntax.Expression.TupledExpression [ part0, part1 ]

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Expression.TupledExpression [ part0, part1, part2 ]

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    Elm.Syntax.Expression.TupledExpression (part0 :: part1 :: part2 :: part3 :: part4Up)

        syntaxExpressionNotParenthesized ->
            syntaxExpressionNotParenthesized


{-| Print a single [`Elm.Syntax.Expression.Case`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Case)
-}
case_ :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.Case
    -> Print
case_ syntaxComments ( casePattern, caseResult ) =
    patternNotParenthesized syntaxComments casePattern
        |> Print.followedBy Print.space
        |> Print.followedBy (Print.symbol "->")
        |> Print.followedBy
            (Print.indentedByNextMultipleOf4
                (Print.layout Print.NextLine
                    |> Print.followedBy (expressionNotParenthesized syntaxComments caseResult)
                )
            )
