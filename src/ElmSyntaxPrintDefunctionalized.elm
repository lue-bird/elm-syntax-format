module ElmSyntaxPrintDefunctionalized exposing
    ( module_
    , moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
    , moduleName, qualifiedReference
    )

{-|

@docs module_

@docs moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
@docs moduleName, qualifiedReference

-}

import Bitwise
import Char.Extra
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


{-| Print an [`Elm.Syntax.File.File`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-File#File)
-}
module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    let
        maybeModuleDocumentation : Maybe (Elm.Syntax.Node.Node String)
        maybeModuleDocumentation =
            moduleDocumentation syntaxModule

        commentsAndPortDocumentationComments :
            { portDocumentationComments : List (Elm.Syntax.Node.Node String)
            , remainingComments : List (Elm.Syntax.Node.Node String)
            }
        commentsAndPortDocumentationComments =
            (case maybeModuleDocumentation of
                Nothing ->
                    syntaxModule.comments

                Just syntaxModuleDocumentation ->
                    syntaxModule.comments
                        |> List.filter (\c -> c /= syntaxModuleDocumentation)
            )
                |> splitOffPortDocumentationComments

        maybeModuleDocumentationParsed :
            Maybe
                { whileAtDocsLines : List { rawBefore : String, atDocsLine : List String }
                , rawAfterAtDocsLines : String
                }
        maybeModuleDocumentationParsed =
            case maybeModuleDocumentation of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ syntaxModuleDocumentation) ->
                    Just
                        (syntaxModuleDocumentation
                            |> moduleDocumentationParse
                        )

        atDocsLines : List (List String)
        atDocsLines =
            case maybeModuleDocumentationParsed of
                Nothing ->
                    []

                Just moduleDocumentationParsed ->
                    moduleDocumentationParsed.whileAtDocsLines
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

        commentsBeforeDeclarations : List String
        commentsBeforeDeclarations =
            case syntaxModule.declarations of
                [] ->
                    -- invalid syntax
                    []

                (Elm.Syntax.Node.Node declaration0Range _) :: _ ->
                    commentsInRange
                        { start = lastSyntaxLocationBeforeDeclarations
                        , end = declaration0Range.start
                        }
                        commentsAndPortDocumentationComments.remainingComments
    in
    syntaxModule.moduleDefinition
        |> Elm.Syntax.Node.value
        |> moduleHeader { atDocsLines = atDocsLines, comments = commentsAndPortDocumentationComments.remainingComments }
        |> Print.followedBy
            (case maybeModuleDocumentationParsed of
                Nothing ->
                    Print.empty

                Just moduleDocumentationParsed ->
                    printLinebreakLinebreak
                        |> Print.followedBy (printModuleDocumentation moduleDocumentationParsed)
            )
        |> Print.followedBy
            (case syntaxModule.imports of
                [] ->
                    case maybeModuleDocumentation of
                        Nothing ->
                            printLinebreakLinebreak

                        Just _ ->
                            case commentsBeforeDeclarations of
                                [] ->
                                    printLinebreakLinebreak

                                _ :: _ ->
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
                            commentsAndPortDocumentationComments.remainingComments
                     of
                        [] ->
                            Print.linebreak

                        comment0 :: comment1Up ->
                            printLinebreakLinebreak
                                |> Print.followedBy
                                    (moduleLevelComments (comment0 :: comment1Up))
                    )
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            ((Elm.Syntax.Node.Node import0Range import0 :: import1Up)
                                |> imports commentsAndPortDocumentationComments.remainingComments
                            )
                        |> Print.followedBy printLinebreakLinebreak
            )
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (case commentsBeforeDeclarations of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    moduleLevelCommentsBeforeDeclaration
                        { comment0 = comment0, comment1Up = comment1Up }
            )
        |> Print.followedBy
            (syntaxModule.declarations
                |> declarations
                    { comments = commentsAndPortDocumentationComments.remainingComments
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
                            (listFilledLast declaration0 declaration1Up
                                |> Elm.Syntax.Node.range
                                |> .end
                            )
                            commentsAndPortDocumentationComments.remainingComments
                    of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            printLinebreakLinebreakLinebreak
                                |> Print.followedBy
                                    (moduleLevelComments (comment0 :: comment1Up))
            )


printModuleDocumentation :
    { whileAtDocsLines : List { rawBefore : String, atDocsLine : List String }
    , rawAfterAtDocsLines : String
    }
    -> Print
printModuleDocumentation moduleDocumentationBlocks =
    let
        content : String
        content =
            ((moduleDocumentationBlocks.whileAtDocsLines
                |> listMapAndFlattenToString
                    (\atDocsLineAndBefore ->
                        atDocsLineAndBefore.rawBefore
                            ++ (case atDocsLineAndBefore.atDocsLine of
                                    [] ->
                                        ""

                                    atDocsExpose0 :: atDocsExpose1Up ->
                                        "@docs "
                                            ++ ((atDocsExpose0 :: atDocsExpose1Up)
                                                    |> String.join ", "
                                               )
                                            ++ "\n"
                               )
                    )
             )
                ++ moduleDocumentationBlocks.rawAfterAtDocsLines
            )
                |> String.trimRight
    in
    Print.exactly
        ("{-|"
            ++ (if content |> String.startsWith "@docs " then
                    "\n\n"

                else
                    " "
               )
            ++ content
            ++ (if content |> String.contains "\n" then
                    "\n\n-}"

                else
                    "\n-}"
               )
        )


{-| Resulting lists are sorted by range
-}
splitOffPortDocumentationComments :
    List (Elm.Syntax.Node.Node String)
    ->
        { portDocumentationComments : List (Elm.Syntax.Node.Node String)
        , remainingComments : List (Elm.Syntax.Node.Node String)
        }
splitOffPortDocumentationComments commentsAndPortDocumentationComments =
    commentsAndPortDocumentationComments
        |> List.foldr
            (\commentOrPortDocumentationComments soFar ->
                if commentOrPortDocumentationComments |> Elm.Syntax.Node.value |> String.startsWith "{-|" then
                    { remainingComments = soFar.remainingComments
                    , portDocumentationComments = commentOrPortDocumentationComments :: soFar.portDocumentationComments
                    }

                else
                    { remainingComments = commentOrPortDocumentationComments :: soFar.remainingComments
                    , portDocumentationComments = soFar.portDocumentationComments
                    }
            )
            commentsEmptyPortDocumentationRemainingCommentsEmpty


commentsEmptyPortDocumentationRemainingCommentsEmpty : { remainingComments : List a_, portDocumentationComments : List b_ }
commentsEmptyPortDocumentationRemainingCommentsEmpty =
    { remainingComments = [], portDocumentationComments = [] }


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
                |> String.slice
                    -- String.length "{-|"
                    3
                    ((moduleDocumentationContent |> String.length)
                        - -- String.length "-}"
                          2
                    )
                |> String.trimLeft
                |> String.lines
                |> List.foldl
                    (\line soFar ->
                        if line |> String.startsWith "@docs " then
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
                            case line of
                                "@docs" ->
                                    { rawSinceAtDocs = ""
                                    , finishedBlocks =
                                        { rawBefore = soFar.rawSinceAtDocs
                                        , atDocsLine = []
                                        }
                                            :: soFar.finishedBlocks
                                    }

                                _ ->
                                    { rawSinceAtDocs = soFar.rawSinceAtDocs ++ line ++ "\n"
                                    , finishedBlocks = soFar.finishedBlocks
                                    }
                    )
                    rawSinceAtDocsEmptyFinishedBlocksEmpty
    in
    { whileAtDocsLines = parsed.finishedBlocks |> List.reverse
    , rawAfterAtDocsLines = parsed.rawSinceAtDocs
    }


rawSinceAtDocsEmptyFinishedBlocksEmpty : { rawSinceAtDocs : String, finishedBlocks : List a_ }
rawSinceAtDocsEmptyFinishedBlocksEmpty =
    { rawSinceAtDocs = "", finishedBlocks = [] }


commentsAfter : Elm.Syntax.Range.Location -> List (Elm.Syntax.Node.Node String) -> List String
commentsAfter end sortedComments =
    case sortedComments of
        [] ->
            []

        (Elm.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case locationCompareFast headCommentRange.start end of
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
            (case listFilledLast syntaxComments.comment0 syntaxComments.comment1Up of
                "{--}" ->
                    -- don't ask me why elm-format formats it that way
                    Print.empty

                _ ->
                    printLinebreakLinebreak
            )


commentNodesInRange : Elm.Syntax.Range.Range -> List (Elm.Syntax.Node.Node String) -> List (Elm.Syntax.Node.Node String)
commentNodesInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        headCommentNode :: tailComments ->
            let
                (Elm.Syntax.Node.Node headCommentRange _) =
                    headCommentNode
            in
            case locationCompareFast headCommentRange.start range.start of
                LT ->
                    commentNodesInRange range tailComments

                EQ ->
                    headCommentNode :: commentNodesInRange range tailComments

                GT ->
                    case locationCompareFast headCommentRange.end range.end of
                        GT ->
                            []

                        LT ->
                            headCommentNode :: commentNodesInRange range tailComments

                        EQ ->
                            headCommentNode :: commentNodesInRange range tailComments


commentsInRange : Elm.Syntax.Range.Range -> List (Elm.Syntax.Node.Node String) -> List String
commentsInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        (Elm.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case locationCompareFast headCommentRange.start range.start of
                LT ->
                    commentsInRange range tailComments

                EQ ->
                    headComment :: commentsInRange range tailComments

                GT ->
                    case locationCompareFast headCommentRange.end range.end of
                        GT ->
                            []

                        LT ->
                            headComment :: commentsInRange range tailComments

                        EQ ->
                            headComment :: commentsInRange range tailComments


lineSpreadInRange : Elm.Syntax.Range.Range -> Print.LineSpread
lineSpreadInRange range =
    if range.end.row - range.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


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

        lineSpread : Print.LineSpread
        lineSpread =
            case containedComments of
                _ :: _ ->
                    Print.MultipleLines

                [] ->
                    lineSpreadInRange syntaxExposing.fullRange
    in
    (case lineSpread of
        Print.SingleLine ->
            printExactlyParensOpening

        Print.MultipleLines ->
            printExactlyParensOpeningSpace
    )
        |> Print.followedBy
            ((syntaxExposing.expose0 :: syntaxExposing.expose1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    (\(Elm.Syntax.Node.Node _ syntaxExpose) ->
                        Print.exactly (expose syntaxExpose)
                    )
                    (Print.emptyOrLinebreakIndented lineSpread
                        |> Print.followedBy printExactlyCommaSpace
                    )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy
            (case containedComments of
                [] ->
                    printExactlyParensClosing

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                        |> Print.followedBy printExactlyParensClosing
            )


listMapAndFlattenToString : (a -> String) -> List a -> String
listMapAndFlattenToString elementToString elements =
    elements
        |> List.foldl
            (\next soFar -> soFar ++ elementToString next ++ "")
            ""


listMapAndIntersperseAndFlattenToString : (a -> String) -> String -> List a -> String
listMapAndIntersperseAndFlattenToString elementToString betweenElements elements =
    case elements of
        [] ->
            ""

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar -> soFar ++ betweenElements ++ elementToString next ++ "")
                    (elementToString head)


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
        |> List.sortWith
            (\(Elm.Syntax.Node.Node _ a) (Elm.Syntax.Node.Node _ b) ->
                exposeCompare a b
            )
        |> exposesCombine


exposesCombine :
    List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
exposesCombine syntaxExposes =
    case syntaxExposes of
        [] ->
            []

        [ _ ] as onlyExposeList ->
            onlyExposeList

        expose0Node :: (expose1Node :: expose2Up) ->
            let
                (Elm.Syntax.Node.Node _ expose1) =
                    expose1Node

                (Elm.Syntax.Node.Node expose0Range expose0) =
                    expose0Node
            in
            case exposeCompare expose0 expose1 of
                EQ ->
                    exposesCombine
                        (Elm.Syntax.Node.Node expose0Range
                            (exposeMerge expose0 expose1)
                            :: expose2Up
                        )

                LT ->
                    expose0Node :: exposesCombine (expose1Node :: expose2Up)

                GT ->
                    expose0Node :: exposesCombine (expose1Node :: expose2Up)


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


{-| Print the stuff after `exposing` in a module header.
For import exposing: [`importExposing`](#importExposing)
-}
moduleExposing :
    { atDocsLines : List (List String), comments : List (Elm.Syntax.Node.Node String) }
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
moduleExposing context (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Elm.Syntax.Exposing.All _ ->
            printExactlyParensOpeningDotDotParensClosing

        Elm.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet |> exposeListToNormal of
                [] ->
                    printExactlyParensOpeningParensClosed

                [ Elm.Syntax.Node.Node _ onlySyntaxExpose ] ->
                    let
                        containedComments : List String
                        containedComments =
                            commentsInRange exposingRange context.comments

                        lineSpread : Print.LineSpread
                        lineSpread =
                            case containedComments of
                                _ :: _ ->
                                    Print.MultipleLines

                                [] ->
                                    Print.SingleLine
                    in
                    Print.exactly
                        ((case lineSpread of
                            Print.SingleLine ->
                                "("

                            Print.MultipleLines ->
                                "( "
                         )
                            ++ expose onlySyntaxExpose
                            ++ ""
                        )
                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                        |> Print.followedBy
                            (case containedComments of
                                [] ->
                                    printExactlyParensClosing

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                                        |> Print.followedBy printExactlyParensClosing
                            )

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
                                                        { remaining : List Elm.Syntax.Exposing.TopLevelExpose
                                                        , exposes : List Elm.Syntax.Exposing.TopLevelExpose
                                                        }
                                                    atDocsExposeLine =
                                                        atDocsLineToExposesAndRemaining atDocsLine soFar.remainingExposes
                                                in
                                                { atDocsExposeLines =
                                                    atDocsExposeLine.exposes :: soFar.atDocsExposeLines
                                                , remainingExposes = atDocsExposeLine.remaining
                                                }
                                            )
                                            { remainingExposes =
                                                (expose0 :: expose1 :: expose2Up)
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
                                    printExactlyParensOpeningSpace
                                        |> Print.followedBy
                                            ((atDocsExposeLine0 :: atDocsExposeLine1Up)
                                                |> Print.listMapAndIntersperseAndFlatten
                                                    (\atDocsLine ->
                                                        Print.exactly
                                                            (atDocsLine
                                                                |> listMapAndIntersperseAndFlattenToString
                                                                    expose
                                                                    ", "
                                                            )
                                                    )
                                                    printLinebreakIndentedCommaSpace
                                            )
                                        |> Print.followedBy Print.linebreakIndented
                                        |> Print.followedBy
                                            (case atDocsExposeLines.remainingExposes of
                                                [] ->
                                                    printExactlyParensClosing

                                                remainingExpose0 :: remainingExpose1Up ->
                                                    Print.exactly
                                                        (", "
                                                            ++ ((remainingExpose0 :: remainingExpose1Up)
                                                                    |> listMapAndIntersperseAndFlattenToString
                                                                        expose
                                                                        ", "
                                                               )
                                                        )
                                                        |> Print.followedBy Print.linebreakIndented
                                                        |> Print.followedBy printExactlyParensClosing
                                            )

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
        { remaining : List Elm.Syntax.Exposing.TopLevelExpose
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
                case soFar.remaining |> listFirstJustMap toExposeReferencedByAtDocsString of
                    Nothing ->
                        soFar

                    Just exposeReferencedByAtDocsString ->
                        { remaining =
                            soFar.remaining
                                |> List.filter (\ex -> ex /= exposeReferencedByAtDocsString)
                        , exposes = exposeReferencedByAtDocsString :: soFar.exposes
                        }
            )
            { remaining = remainingExposes
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


listFirstJustMap : (a -> Maybe b) -> List a -> Maybe b
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

                lineSpread : Print.LineSpread
                lineSpread =
                    exposingPrint |> Print.lineSpread
            in
            Print.exactly
                ("module "
                    ++ moduleName (defaultModuleData.moduleName |> Elm.Syntax.Node.value)
                    ++ " exposing"
                )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy exposingPrint
                        )
                    )

        Elm.Syntax.Module.PortModule defaultModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    defaultModuleData.exposingList
                        |> moduleExposing context

                lineSpread : Print.LineSpread
                lineSpread =
                    exposingPrint |> Print.lineSpread
            in
            Print.exactly
                ("port module "
                    ++ moduleName
                        (defaultModuleData.moduleName |> Elm.Syntax.Node.value)
                    ++ " exposing"
                )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy exposingPrint
                        )
                    )

        Elm.Syntax.Module.EffectModule effectModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    effectModuleData.exposingList
                        |> moduleExposing context

                lineSpread : Print.LineSpread
                lineSpread =
                    exposingPrint |> Print.lineSpread
            in
            Print.exactly
                ("effect module "
                    ++ moduleName
                        (effectModuleData.moduleName |> Elm.Syntax.Node.value)
                    ++ " where { "
                    ++ ([ case effectModuleData.command of
                            Nothing ->
                                Nothing

                            Just (Elm.Syntax.Node.Node _ name) ->
                                Just ("command = " ++ name)
                        , case effectModuleData.subscription of
                            Nothing ->
                                Nothing

                            Just (Elm.Syntax.Node.Node _ name) ->
                                Just ("subscription = " ++ name)
                        ]
                            |> List.filterMap identity
                            |> String.join ", "
                       )
                    ++ " } exposing"
                )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
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
                                , commentsBetweenImports =
                                    soFar.commentsBetweenImports
                                        ++ commentsInRange { start = soFar.previousImportRange.end, end = importRange.start }
                                            syntaxComments
                                }
                            )
                            { previousImportRange = import0Range
                            , commentsBetweenImports = []
                            }
                        |> .commentsBetweenImports
            in
            (case commentsBetweenImports of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    moduleLevelComments (comment0 :: comment1Up)
                        |> Print.followedBy Print.linebreak
            )
                |> Print.followedBy
                    ((Elm.Syntax.Node.Node import0Range import0 :: imports1Up)
                        |> List.sortWith
                            (\(Elm.Syntax.Node.Node _ a) (Elm.Syntax.Node.Node _ b) ->
                                compare (a.moduleName |> Elm.Syntax.Node.value) (b.moduleName |> Elm.Syntax.Node.value)
                            )
                        |> importsCombine
                        |> Print.listMapAndIntersperseAndFlatten
                            (\syntaxImport -> import_ syntaxComments syntaxImport)
                            Print.linebreak
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
                            (case lineSpreadInRange earlierExposingExplicitRange of
                                Print.MultipleLines ->
                                    earlierExposingExplicitRange

                                Print.SingleLine ->
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
    printExactImport
        |> Print.followedBy
            (case syntaxImport.moduleAlias of
                Nothing ->
                    case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                        [] ->
                            Print.exactly
                                (" "
                                    ++ moduleName syntaxModuleName
                                )

                        comment0 :: comment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly (moduleName syntaxModuleName))
                                )

                Just (Elm.Syntax.Node.Node moduleAliasRange moduleAlias) ->
                    case commentsInRange { start = moduleNameRange.end, end = moduleAliasRange.start } syntaxComments of
                        [] ->
                            (case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                                [] ->
                                    Print.exactly
                                        (" "
                                            ++ moduleName syntaxModuleName
                                        )

                                moduleNameComment0 :: moduleNameComment1Up ->
                                    Print.withIndentAtNextMultipleOf4
                                        (Print.linebreakIndented
                                            |> Print.followedBy (comments (moduleNameComment0 :: moduleNameComment1Up))
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy (Print.exactly (moduleName syntaxModuleName))
                                        )
                            )
                                |> Print.followedBy (Print.exactly (" as " ++ moduleName moduleAlias))

                        aliasComment0 :: aliasComment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                                            [] ->
                                                Print.exactly (moduleName syntaxModuleName)

                                            moduleNameComment0 :: moduleNameComment1Up ->
                                                comments (moduleNameComment0 :: moduleNameComment1Up)
                                                    |> Print.followedBy Print.linebreakIndented
                                                    |> Print.followedBy (Print.exactly (moduleName syntaxModuleName))
                                        )
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (printLinebreakIndentedAs
                                                |> Print.followedBy
                                                    (Print.withIndentAtNextMultipleOf4
                                                        (Print.linebreakIndented
                                                            |> Print.followedBy (comments (aliasComment0 :: aliasComment1Up))
                                                            |> Print.followedBy Print.linebreakIndented
                                                            |> Print.followedBy
                                                                (Print.exactly (moduleName moduleAlias))
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
                                lineSpread : Print.LineSpread
                                lineSpread =
                                    Print.lineSpread exposingPrint
                            in
                            Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyExposing
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented lineSpread
                                                |> Print.followedBy exposingPrint
                                            )
                                        )
                                )

                        exposingComment0 :: exposingComment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (printLinebreakIndentedExposing
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.linebreakIndented
                                                |> Print.followedBy (comments (exposingComment0 :: exposingComment1Up))
                                                |> Print.followedBy Print.linebreakIndented
                                                |> Print.followedBy exposingPrint
                                            )
                                        )
                                )
            )


{-| Print the stuff after `exposing` in an import.
For module header exposing: [`moduleExposing`](#moduleExposing)
-}
importExposing :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
importExposing syntaxComments (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Elm.Syntax.Exposing.All _ ->
            printExactlyParensOpeningDotDotParensClosing

        Elm.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet of
                [] ->
                    -- invalid syntax
                    printExactlyParensOpeningParensClosed

                expose0 :: expose1Up ->
                    exposingMulti syntaxComments
                        { fullRange = exposingRange, expose0 = expose0, expose1Up = expose1Up }


{-| Print `--` or `{- -}` comments placed _within a declaration_.
For top-level comments: [`moduleLevelComments`](#moduleLevelComments)
-}
comments : List String -> Print
comments syntaxComments =
    syntaxComments
        |> Print.listMapAndIntersperseAndFlatten
            comment
            Print.linebreakIndented


collapsibleComments : List String -> { print : Print, lineSpread : Print.LineSpread }
collapsibleComments commentsToPrint =
    case commentsToPrint of
        [] ->
            printEmptyLineSpreadSingleLine

        comment0 :: comment1Up ->
            let
                commentPrints : List Print
                commentPrints =
                    (comment0 :: comment1Up) |> List.map comment
            in
            if
                commentPrints
                    |> List.all
                        (\commentPrint ->
                            commentPrint |> Print.toString |> commentCanBePartOfCollapsible
                        )
            then
                { print =
                    commentPrints
                        |> Print.listIntersperseAndFlatten printExactlySpace
                , lineSpread = Print.SingleLine
                }

            else
                { print = comments (comment0 :: comment1Up)
                , lineSpread = Print.MultipleLines
                }


printEmptyLineSpreadSingleLine : { print : Print.Print, lineSpread : Print.LineSpread }
printEmptyLineSpreadSingleLine =
    { print = Print.empty, lineSpread = Print.SingleLine }


commentCanBePartOfCollapsible : String -> Bool
commentCanBePartOfCollapsible syntaxComment =
    case syntaxComment of
        "{--}" ->
            False

        commentNotDirectlyClosed ->
            (commentNotDirectlyClosed |> String.startsWith "{-")
                && Basics.not (commentNotDirectlyClosed |> String.contains "\n")


{-| Print `--` or `{- -}` comments placed outside of declarations at the top level.
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
                    (comment1Up
                        |> Print.listMapAndFlatten
                            (\syntaxComment ->
                                case syntaxComment of
                                    "{--}" ->
                                        moduleLevelMultiLneCommentWithoutWhitespace

                                    notEmptyMultiLineComment ->
                                        comment notEmptyMultiLineComment
                                            |> Print.followedBy Print.linebreak
                            )
                    )


moduleLevelMultiLneCommentWithoutWhitespace : Print.Print
moduleLevelMultiLneCommentWithoutWhitespace =
    printLinebreakLinebreak
        |> Print.followedBy printExactlyCurlyOpeningDotDotCurlyClosing
        |> Print.followedBy Print.linebreak


{-| Print a single `--` or `{- -}` comment.
-}
comment : String -> Print
comment syntaxComment =
    case syntaxComment of
        "{--}" ->
            printExactlyCurlyOpeningDotDotCurlyClosing

        nonDirectlyClosingMultiLineComment ->
            if nonDirectlyClosingMultiLineComment |> String.startsWith "--" then
                Print.exactly (nonDirectlyClosingMultiLineComment |> String.trimRight)

            else
                -- comment starts with {-
                let
                    commentContentLines : List String
                    commentContentLines =
                        nonDirectlyClosingMultiLineComment
                            |> String.slice
                                -- {-
                                2
                                ((nonDirectlyClosingMultiLineComment |> String.length)
                                    - -- -}
                                      2
                                )
                            |> String.lines

                    commentContentNormal : List String
                    commentContentNormal =
                        case commentContentLines of
                            [] ->
                                []

                            commentContentLine0 :: commentContentLine1Up ->
                                (commentContentLine0 |> String.trim)
                                    :: (commentContentLine1Up
                                            |> listDropLastIfEmpty
                                            |> linesUnindentAndTrimRight
                                       )
                in
                printExactlyCurlyOpeningMinus
                    |> Print.followedBy
                        -- if original commentContent contains >= 2 lines, keep but
                        (case commentContentNormal of
                            -- only spaces
                            [] ->
                                printExactlySpaceSpace

                            [ singleLine ] ->
                                Print.exactly
                                    (" " ++ singleLine ++ " ")

                            firstLine :: secondLine :: thirdLineUp ->
                                (case firstLine of
                                    "" ->
                                        Print.linebreakIndented

                                    lineNotEmpty ->
                                        Print.exactly (" " ++ lineNotEmpty)
                                            |> Print.followedBy Print.linebreakIndented
                                )
                                    |> Print.followedBy
                                        ((secondLine :: thirdLineUp)
                                            |> Print.listMapAndFlatten
                                                (\line ->
                                                    case line of
                                                        "" ->
                                                            Print.linebreakIndented

                                                        lineNotEmpty ->
                                                            Print.exactly ("   " ++ lineNotEmpty)
                                                                |> Print.followedBy
                                                                    Print.linebreakIndented
                                                )
                                        )
                        )
                    |> Print.followedBy printExactlyMinusCurlyClosing


linesUnindentAndTrimRight : List String -> List String
linesUnindentAndTrimRight lines =
    let
        nonBlankLinesIndentationMinimum : Maybe Int
        nonBlankLinesIndentationMinimum =
            lines
                |> List.foldl
                    (\line soFarMinimumIndentationOrNothing ->
                        case line |> String.trim of
                            "" ->
                                soFarMinimumIndentationOrNothing

                            _ ->
                                let
                                    currentLineIndentation : Int
                                    currentLineIndentation =
                                        line |> lineIndentation
                                in
                                Just
                                    (case soFarMinimumIndentationOrNothing of
                                        Nothing ->
                                            currentLineIndentation

                                        Just soFarMinimumIndentation ->
                                            Basics.min
                                                soFarMinimumIndentation
                                                currentLineIndentation
                                    )
                    )
                    Nothing
    in
    case nonBlankLinesIndentationMinimum of
        Nothing ->
            lines |> List.map (\_ -> "")

        Just minimumIndentation ->
            lines
                |> List.map
                    (\line ->
                        line
                            |> String.dropLeft minimumIndentation
                            |> String.trimRight
                    )


lineIndentation : String -> Int
lineIndentation line =
    line
        |> String.foldl
            (\char soFar ->
                if soFar.onlySpaces then
                    case char of
                        ' ' ->
                            { spaceCount = soFar.spaceCount + 1, onlySpaces = True }

                        _ ->
                            { spaceCount = soFar.spaceCount, onlySpaces = False }

                else
                    soFar
            )
            spaceCount0OnlySpacesTrue
        |> .spaceCount


spaceCount0OnlySpacesTrue : { spaceCount : Int, onlySpaces : Bool }
spaceCount0OnlySpacesTrue =
    { spaceCount = 0, onlySpaces = True }


listDropLastIfEmpty : List String -> List String
listDropLastIfEmpty list =
    case list of
        [] ->
            []

        [ onlyElement ] ->
            case onlyElement |> String.trim of
                "" ->
                    []

                _ ->
                    [ onlyElement ]

        element0 :: element1 :: element2Up ->
            element0 :: listDropLastIfEmpty (element1 :: element2Up)


{-| Print an [`Elm.Syntax.ModuleName.ModuleName`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
-}
moduleName : Elm.Syntax.ModuleName.ModuleName -> String
moduleName syntaxModuleName =
    syntaxModuleName |> String.join "."


{-| Print a single [`Elm.Syntax.Exposing.TopLevelExpose`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#TopLevelExpose)
-}
expose : Elm.Syntax.Exposing.TopLevelExpose -> String
expose syntaxExpose =
    case syntaxExpose of
        Elm.Syntax.Exposing.InfixExpose operatorSymbol ->
            "(" ++ operatorSymbol ++ ")"

        Elm.Syntax.Exposing.FunctionExpose name ->
            name

        Elm.Syntax.Exposing.TypeOrAliasExpose name ->
            name

        Elm.Syntax.Exposing.TypeExpose syntaxExposeType ->
            case syntaxExposeType.open of
                Nothing ->
                    syntaxExposeType.name

                Just _ ->
                    syntaxExposeType.name ++ "(..)"


patternParenthesizedIfSpaceSeparated :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Print
patternParenthesizedIfSpaceSeparated syntaxComments syntaxPattern =
    if patternIsSpaceSeparated (syntaxPattern |> Elm.Syntax.Node.value) then
        patternParenthesized syntaxComments syntaxPattern

    else
        patternNotParenthesized syntaxComments syntaxPattern


patternParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Print
patternParenthesized syntaxComments patternNode =
    parenthesized patternNotParenthesized
        { fullRange = patternNode |> Elm.Syntax.Node.range
        , notParenthesized = patternNode |> patternToNotParenthesized
        }
        syntaxComments


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


stringLiteral : Elm.Syntax.Node.Node String -> Print
stringLiteral (Elm.Syntax.Node.Node range stringContent) =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""

        wasProbablyTripleDoubleQuoteOriginally : Bool
        wasProbablyTripleDoubleQuoteOriginally =
            (range.start.row /= range.end.row)
                || ((range.end.column - range.start.column)
                        - (singleDoubleQuotedStringContentEscaped |> stringUnicodeLength)
                        /= 2
                   )
    in
    if wasProbablyTripleDoubleQuoteOriginally then
        printExactlyDoubleQuoteDoubleQuoteDoubleQuote
            |> Print.followedBy
                (stringContent
                    |> String.foldl
                        (\contentChar soFar ->
                            soFar ++ tripleDoubleQuotedStringCharToEscaped contentChar ++ ""
                        )
                        ""
                    |> tripleDoubleQuotedStringEscapeDoubleQuotes
                    |> String.lines
                    |> Print.listMapAndIntersperseAndFlatten
                        Print.exactly
                        Print.linebreak
                )
            |> Print.followedBy printExactlyDoubleQuoteDoubleQuoteDoubleQuote

    else
        Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


stringUnicodeLength : String -> Int
stringUnicodeLength string =
    string |> String.foldl (\_ soFar -> soFar + 1) 0


tripleDoubleQuotedStringEscapeDoubleQuotes : String -> String
tripleDoubleQuotedStringEscapeDoubleQuotes string =
    let
        beforeLastCharEscaped : { consecutiveDoubleQuoteCount : Int, result : String }
        beforeLastCharEscaped =
            -- escape continuous double quotes if combined length >= 3
            string
                |> String.foldl
                    (\char soFar ->
                        case char of
                            '"' ->
                                { consecutiveDoubleQuoteCount =
                                    soFar.consecutiveDoubleQuoteCount + 1
                                , result = soFar.result
                                }

                            firstCharNotDoubleQuote ->
                                { consecutiveDoubleQuoteCount = 0
                                , result =
                                    soFar.result
                                        ++ (case soFar.consecutiveDoubleQuoteCount of
                                                0 ->
                                                    ""

                                                1 ->
                                                    "\""

                                                2 ->
                                                    "\"\""

                                                atLeast3ConsecutiveDoubleQuoteCount ->
                                                    String.repeat atLeast3ConsecutiveDoubleQuoteCount "\\\""
                                           )
                                        ++ String.fromChar firstCharNotDoubleQuote
                                        ++ ""
                                }
                    )
                    consecutiveDoubleQuoteCountResultStringEmpty
    in
    beforeLastCharEscaped.result
        ++ (-- escape preceding continuous double quotes if they connect to the last char double quote
            String.repeat beforeLastCharEscaped.consecutiveDoubleQuoteCount "\\\""
           )
        ++ ""


consecutiveDoubleQuoteCountResultStringEmpty : { consecutiveDoubleQuoteCount : Int, result : String }
consecutiveDoubleQuoteCountResultStringEmpty =
    { consecutiveDoubleQuoteCount = 0
    , result = ""
    }


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\u{000D}"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


tripleDoubleQuotedStringCharToEscaped : Char -> String
tripleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            -- edge cases handled in a later step
            "\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\n"

        '\u{000D}' ->
            "\u{000D}"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
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
            ++ ""


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
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    let
        charCode : Int
        charCode =
            Char.toCode character
    in
    String.toUpper
        (unsafeHexDigitIntToString
            (charCode
                |> Bitwise.and 0xF000
                |> Bitwise.shiftRightBy 12
            )
            ++ unsafeHexDigitIntToString
                (charCode
                    |> Bitwise.and 0x0F00
                    |> Bitwise.shiftRightBy 8
                )
            ++ unsafeHexDigitIntToString
                (charCode
                    |> Bitwise.and 0xF0
                    |> Bitwise.shiftRightBy 4
                )
            ++ unsafeHexDigitIntToString
                (charCode |> Bitwise.and 0x0F)
            ++ ""
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        Char.Extra.isLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

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
                        True

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


charLiteral : Char -> String
charLiteral charContent =
    "'"
        ++ quotedCharToEscaped charContent
        ++ "'"


quotedCharToEscaped : Char -> String
quotedCharToEscaped character =
    case character of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\u{000D}"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intLiteral : Int -> String
intLiteral =
    String.fromInt


hexLiteral : Int -> String
hexLiteral int =
    let
        maybeSignPrint : String
        maybeSignPrint =
            if int < 0 then
                "-"

            else
                ""

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
        ++ "0x"
        ++ (intToHexString int
                |> stringResizePadLeftWith0s digitCountToPrint
           )


stringResizePadLeftWith0s : Int -> String -> String
stringResizePadLeftWith0s length unpaddedString =
    if length < (unpaddedString |> String.length) then
        String.left length unpaddedString

    else
        String.repeat (length - (unpaddedString |> String.length)) "0"
            ++ unpaddedString
            ++ ""


patternToNotParenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
patternToNotParenthesized (Elm.Syntax.Node.Node fullRange syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternToNotParenthesized

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    inParens |> patternToNotParenthesized

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1, part2 ])

                [] ->
                    -- should be covered by UnitPattern
                    Elm.Syntax.Node.Node fullRange Elm.Syntax.Pattern.UnitPattern

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up))

        Elm.Syntax.Pattern.AllPattern ->
            Elm.Syntax.Node.Node fullRange Elm.Syntax.Pattern.AllPattern

        Elm.Syntax.Pattern.UnitPattern ->
            Elm.Syntax.Node.Node fullRange Elm.Syntax.Pattern.UnitPattern

        Elm.Syntax.Pattern.VarPattern name ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.VarPattern name)

        Elm.Syntax.Pattern.CharPattern char ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.CharPattern char)

        Elm.Syntax.Pattern.StringPattern string ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.StringPattern string)

        Elm.Syntax.Pattern.IntPattern int ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.IntPattern int)

        Elm.Syntax.Pattern.HexPattern int ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.HexPattern int)

        Elm.Syntax.Pattern.FloatPattern float ->
            -- invalid syntax
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.FloatPattern float)

        Elm.Syntax.Pattern.RecordPattern fields ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.RecordPattern fields)

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.ListPattern elementPatterns)

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns)

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode ->
            Elm.Syntax.Node.Node fullRange (Elm.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode)


{-| Print an [`Elm.Syntax.Pattern.Pattern`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Print
patternNotParenthesized syntaxComments (Elm.Syntax.Node.Node fullRange syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            printExactlyUnderscore

        Elm.Syntax.Pattern.UnitPattern ->
            printExactlyParensOpeningParensClosed

        Elm.Syntax.Pattern.VarPattern name ->
            Print.exactly name

        Elm.Syntax.Pattern.CharPattern char ->
            Print.exactly (charLiteral char)

        Elm.Syntax.Pattern.StringPattern string ->
            stringLiteral (Elm.Syntax.Node.Node fullRange string)

        Elm.Syntax.Pattern.IntPattern int ->
            Print.exactly (intLiteral int)

        Elm.Syntax.Pattern.HexPattern int ->
            Print.exactly (hexLiteral int)

        Elm.Syntax.Pattern.FloatPattern float ->
            -- invalid syntax
            Print.exactly (String.fromFloat float)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange { start = fullRange.start, end = inParens |> Elm.Syntax.Node.range |> .start } syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange { start = inParens |> Elm.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    patternNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized patternNotParenthesized
                        { notParenthesized = inParens |> patternToNotParenthesized
                        , fullRange = fullRange
                        }
                        syntaxComments

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    { part0 = part0, part1 = part1, fullRange = fullRange }
                        |> tuple
                            { printPartNotParenthesized = patternNotParenthesized
                            , lineSpreadMinimum = Print.SingleLine
                            }
                            syntaxComments

                [ part0, part1, part2 ] ->
                    { part0 = part0, part1 = part1, part2 = part2, fullRange = fullRange }
                        |> triple
                            { printPartNotParenthesized = patternNotParenthesized
                            , lineSpreadMinimum = Print.SingleLine
                            }
                            syntaxComments

                [] ->
                    -- should be covered by UnitPattern
                    printExactlyParensOpeningParensClosed

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    let
                        commentsBeforeInParens : List String
                        commentsBeforeInParens =
                            commentsInRange { start = fullRange.start, end = inParens |> Elm.Syntax.Node.range |> .start } syntaxComments

                        commentsAfterInParens : List String
                        commentsAfterInParens =
                            commentsInRange { start = inParens |> Elm.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
                    in
                    case ( commentsBeforeInParens, commentsAfterInParens ) of
                        ( [], [] ) ->
                            patternNotParenthesized syntaxComments inParens

                        _ ->
                            parenthesized patternNotParenthesized
                                { notParenthesized = inParens |> patternToNotParenthesized
                                , fullRange = fullRange
                                }
                                syntaxComments

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    invalidNTuple patternNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1, part2 = part2, part3 = part3, part4Up = part4Up }

        Elm.Syntax.Pattern.RecordPattern fields ->
            patternRecord syntaxComments
                { fullRange = fullRange, fields = fields }

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            patternCons syntaxComments
                { head = headPattern, tail = tailPattern }

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            patternList syntaxComments
                { fullRange = fullRange, elements = elementPatterns }

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            construct
                { printArgumentParenthesizedIfSpaceSeparated =
                    patternParenthesizedIfSpaceSeparated
                , lineSpreadMinimum = Print.SingleLine
                }
                syntaxComments
                { fullRange = fullRange
                , start =
                    qualifiedReference
                        { qualification = syntaxQualifiedNameRef.moduleName
                        , unqualified = syntaxQualifiedNameRef.name
                        }
                , arguments = argumentPatterns
                }

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode ->
            patternAs syntaxComments
                { aliasedPattern = aliasedPattern, aliasNameNode = aliasNameNode }


patternList :
    List (Elm.Syntax.Node.Node String)
    ->
        { elements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
patternList syntaxComments syntaxList =
    case syntaxList.elements of
        [] ->
            printExactlySquareOpening
                |> Print.followedBy
                    (case commentsInRange syntaxList.fullRange syntaxComments of
                        [] ->
                            printExactlySquareClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlySquareClosing
                    )

        element0 :: element1Up ->
            let
                elementPrintsWithCommentsBefore :
                    { endLocation : Elm.Syntax.Range.Location
                    , reverse : List Print
                    }
                elementPrintsWithCommentsBefore =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\elementNode soFar ->
                                let
                                    (Elm.Syntax.Node.Node elementRange _) =
                                        elementNode

                                    elementPrint : Print
                                    elementPrint =
                                        patternNotParenthesized syntaxComments
                                            elementNode
                                in
                                { endLocation = elementRange.end
                                , reverse =
                                    (case
                                        commentsInRange { start = soFar.endLocation, end = elementRange.start }
                                            syntaxComments
                                     of
                                        [] ->
                                            elementPrint

                                        comment0 :: comment1Up ->
                                            let
                                                commentsBeforeElement : { print : Print, lineSpread : Print.LineSpread }
                                                commentsBeforeElement =
                                                    collapsibleComments (comment0 :: comment1Up)
                                            in
                                            commentsBeforeElement.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsBeforeElement.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> elementPrint |> Print.lineSpread)
                                                        )
                                                    )
                                                |> Print.followedBy elementPrint
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxList.fullRange.start
                            , reverse = []
                            }

                commentsAfterElements : Maybe { print : Print, lineSpread : Print.LineSpread }
                commentsAfterElements =
                    case
                        commentsInRange { start = elementPrintsWithCommentsBefore.endLocation, end = syntaxList.fullRange.end }
                            syntaxComments
                    of
                        [] ->
                            Nothing

                        comment0 :: comment1Up ->
                            Just (collapsibleComments (comment0 :: comment1Up))

                lineSpread : Print.LineSpread
                lineSpread =
                    elementPrintsWithCommentsBefore.reverse
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
                        |> Print.lineSpreadMergeWith
                            (\() -> commentsAfterElements |> maybeLineSpread .lineSpread)
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    (elementPrintsWithCommentsBefore.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\elementPrintWithCommentsBefore ->
                                Print.withIndentIncreasedBy 2
                                    elementPrintWithCommentsBefore
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterElements of
                        Nothing ->
                            Print.spaceOrLinebreakIndented lineSpread

                        Just commentsCollapsibleAfterElements ->
                            Print.withIndentIncreasedBy 2
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy commentsCollapsibleAfterElements.print
                                )
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlySquareClosing


maybeLineSpread : (a -> Print.LineSpread) -> Maybe a -> Print.LineSpread
maybeLineSpread valueToLineSpread maybe =
    case maybe of
        Nothing ->
            Print.SingleLine

        Just value ->
            value |> valueToLineSpread


patternCons :
    List (Elm.Syntax.Node.Node String)
    ->
        { head : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
    -> Print
patternCons syntaxComments syntaxCons =
    let
        headPrint : Print
        headPrint =
            patternParenthesizedIfSpaceSeparated syntaxComments syntaxCons.head

        tailPatterns : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        tailPatterns =
            syntaxCons.tail |> patternConsExpand

        tailPatternPrintsAndCommentsBeforeReverse : List Print
        tailPatternPrintsAndCommentsBeforeReverse =
            tailPatterns
                |> List.foldl
                    (\tailPatternNode soFar ->
                        let
                            (Elm.Syntax.Node.Node tailPatternRange _) =
                                tailPatternNode

                            print : Print
                            print =
                                patternParenthesizedIfSpaceSeparated syntaxComments
                                    tailPatternNode
                        in
                        { reverse =
                            (case
                                commentsInRange { start = soFar.endLocation, end = tailPatternRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = tailPatternRange.end
                        }
                    )
                    { reverse = []
                    , endLocation = syntaxCons.head |> Elm.Syntax.Node.range |> .end
                    }
                |> .reverse

        lineSpread : Print.LineSpread
        lineSpread =
            headPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        tailPatternPrintsAndCommentsBeforeReverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
    in
    headPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy
                        (tailPatternPrintsAndCommentsBeforeReverse
                            |> Print.listReverseAndMapAndIntersperseAndFlatten
                                (\tailPatternElementPrintWithCommentsBefore ->
                                    printExactlyColonColonSpace
                                        |> Print.followedBy
                                            (Print.withIndentIncreasedBy 3
                                                tailPatternElementPrintWithCommentsBefore
                                            )
                                )
                                (Print.spaceOrLinebreakIndented lineSpread)
                        )
                )
            )


patternConsExpand :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
patternConsExpand (Elm.Syntax.Node.Node fulRange syntaxPattern) =
    case syntaxPattern of
        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            headPattern :: patternConsExpand tailPattern

        Elm.Syntax.Pattern.AllPattern ->
            [ Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.AllPattern ]

        Elm.Syntax.Pattern.UnitPattern ->
            [ Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern ]

        Elm.Syntax.Pattern.CharPattern char ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.CharPattern char) ]

        Elm.Syntax.Pattern.StringPattern string ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.StringPattern string) ]

        Elm.Syntax.Pattern.IntPattern int ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.IntPattern int) ]

        Elm.Syntax.Pattern.HexPattern int ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.HexPattern int) ]

        Elm.Syntax.Pattern.FloatPattern float ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.FloatPattern float) ]

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1 ]) ]

                [ part0, part1, part2 ] ->
                    [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1, part2 ]) ]

                [] ->
                    -- should be handled by UnitPattern
                    [ Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern ]

                [ inParens ] ->
                    -- should be handled by ParenthesizedPattern
                    [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ inParens ]) ]

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- should be handled by ParenthesizedPattern
                    [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up)) ]

        Elm.Syntax.Pattern.RecordPattern fields ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.RecordPattern fields) ]

        Elm.Syntax.Pattern.ListPattern elements ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.ListPattern elements) ]

        Elm.Syntax.Pattern.VarPattern variableName ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.VarPattern variableName) ]

        Elm.Syntax.Pattern.NamedPattern reference parameters ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.NamedPattern reference parameters) ]

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName) ]

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            [ Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.ParenthesizedPattern inParens) ]


patternAs :
    List (Elm.Syntax.Node.Node String)
    ->
        { aliasNameNode : Elm.Syntax.Node.Node String
        , aliasedPattern : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
    -> Print
patternAs syntaxComments syntaxAs =
    let
        aliasedPatternPrint : Print
        aliasedPatternPrint =
            patternParenthesizedIfSpaceSeparated syntaxComments syntaxAs.aliasedPattern

        commentsBeforeAliasName : List String
        commentsBeforeAliasName =
            commentsInRange
                { start = syntaxAs.aliasedPattern |> Elm.Syntax.Node.range |> .end
                , end = syntaxAs.aliasNameNode |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeAliasName : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeAliasName =
            collapsibleComments commentsBeforeAliasName

        lineSpread : Print.LineSpread
        lineSpread =
            commentsCollapsibleBeforeAliasName.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> aliasedPatternPrint |> Print.lineSpread)

        namePrint : Print
        namePrint =
            Print.exactly (syntaxAs.aliasNameNode |> Elm.Syntax.Node.value)
    in
    aliasedPatternPrint
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyAs
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy
                        (case commentsBeforeAliasName of
                            [] ->
                                namePrint

                            _ :: _ ->
                                commentsCollapsibleBeforeAliasName.print
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            commentsCollapsibleBeforeAliasName.lineSpread
                                        )
                                    |> Print.followedBy namePrint
                        )
                )
            )


patternRecord :
    List (Elm.Syntax.Node.Node String)
    ->
        { fields : List (Elm.Syntax.Node.Node String)
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
patternRecord syntaxComments syntaxRecord =
    case syntaxRecord.fields of
        [] ->
            printExactlyCurlyOpening
                |> Print.followedBy
                    (case commentsInRange syntaxRecord.fullRange syntaxComments of
                        [] ->
                            printExactlyCurlyClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlyCurlyClosing
                    )

        field0 :: field1Up ->
            let
                fieldPrintsWithCommentsBefore :
                    { endLocation : Elm.Syntax.Range.Location
                    , reverse : List Print
                    }
                fieldPrintsWithCommentsBefore =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node elementRange fieldName) soFar ->
                                { endLocation = elementRange.end
                                , reverse =
                                    (case commentsInRange { start = soFar.endLocation, end = elementRange.start } syntaxComments of
                                        [] ->
                                            Print.exactly fieldName

                                        comment0 :: comment1Up ->
                                            let
                                                commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                                commentsBefore =
                                                    collapsibleComments (comment0 :: comment1Up)
                                            in
                                            commentsBefore.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented commentsBefore.lineSpread)
                                                |> Print.followedBy (Print.exactly fieldName)
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxRecord.fullRange.start
                            , reverse = []
                            }

                maybeCommentsAfterFields : Maybe { print : Print, lineSpread : Print.LineSpread }
                maybeCommentsAfterFields =
                    case
                        commentsInRange
                            { start = fieldPrintsWithCommentsBefore.endLocation
                            , end = syntaxRecord.fullRange.end
                            }
                            syntaxComments
                    of
                        [] ->
                            Nothing

                        comment0 :: comment1Up ->
                            -- yes, in record patterns trailing comments
                            -- are indented and not separated with an extra linebreak
                            Just (collapsibleComments (comment0 :: comment1Up))

                lineSpread : Print.LineSpread
                lineSpread =
                    fieldPrintsWithCommentsBefore.reverse
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
                        |> Print.lineSpreadMergeWith
                            (\() -> maybeCommentsAfterFields |> maybeLineSpread .lineSpread)
            in
            printExactlyCurlyOpeningSpace
                |> Print.followedBy
                    (fieldPrintsWithCommentsBefore.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\fieldPrintWithComments ->
                                Print.withIndentIncreasedBy 2
                                    fieldPrintWithComments
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case maybeCommentsAfterFields of
                        Nothing ->
                            Print.spaceOrLinebreakIndented lineSpread

                        Just commentsAfterFields ->
                            -- yes, in record patterns trailing comments
                            -- are indented and not separated with an extra linebreak
                            Print.withIndentIncreasedBy 2
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy commentsAfterFields.print
                                )
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlyCurlyClosing


typeRecordExtension :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , recordVariable : Elm.Syntax.Node.Node String
        , fields :
            List
                (Elm.Syntax.Node.Node
                    ( Elm.Syntax.Node.Node String
                    , Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
                    )
                )
        }
    -> Print
typeRecordExtension syntaxComments syntaxRecordExtension =
    let
        commentsBeforeRecordVariable : List String
        commentsBeforeRecordVariable =
            commentsInRange
                { start = syntaxRecordExtension.fullRange.start
                , end = syntaxRecordExtension.recordVariable |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRecordVariable : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRecordVariable =
            collapsibleComments commentsBeforeRecordVariable

        fieldPrintsAndComments :
            { endLocation : Elm.Syntax.Range.Location
            , reverse :
                List
                    { syntax :
                        ( Elm.Syntax.Node.Node String
                        , Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
                        )
                    , valuePrint : Print
                    , maybeCommentsBeforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , maybeCommentsBetweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            }
        fieldPrintsAndComments =
            syntaxRecordExtension.fields
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )) soFar ->
                        let
                            (Elm.Syntax.Node.Node fieldValueRange _) =
                                fieldValueNode

                            commentsBeforeName : List String
                            commentsBeforeName =
                                commentsInRange
                                    { start = soFar.endLocation, end = fieldNameRange.start }
                                    syntaxComments

                            commentsBetweenNameAndValue : List String
                            commentsBetweenNameAndValue =
                                commentsInRange
                                    { start = fieldNameRange.start, end = fieldValueRange.start }
                                    syntaxComments
                        in
                        { endLocation = fieldValueRange.end
                        , reverse =
                            { syntax = ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )
                            , valuePrint = typeNotParenthesized syntaxComments fieldValueNode
                            , maybeCommentsBeforeName =
                                case commentsBeforeName of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            , maybeCommentsBetweenNameAndValue =
                                case commentsBetweenNameAndValue of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            }
                                :: soFar.reverse
                        }
                    )
                    { endLocation =
                        syntaxRecordExtension.recordVariable
                            |> Elm.Syntax.Node.range
                            |> .end
                    , reverse = []
                    }

        commentsAfterFields : List String
        commentsAfterFields =
            commentsInRange
                { start = fieldPrintsAndComments.endLocation
                , end = syntaxRecordExtension.fullRange.end
                }
                syntaxComments

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxRecordExtension.fullRange
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRecordVariable.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        fieldPrintsAndComments.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\field ->
                                    field.valuePrint
                                        |> Print.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                case field.maybeCommentsBeforeName of
                                                    Nothing ->
                                                        Print.SingleLine

                                                    Just commentsBeforeName ->
                                                        commentsBeforeName.lineSpread
                                            )
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                case field.maybeCommentsBetweenNameAndValue of
                                                    Nothing ->
                                                        Print.SingleLine

                                                    Just commentsBetweenNameAndValue ->
                                                        commentsBetweenNameAndValue.lineSpread
                                            )
                                )
                    )
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case commentsAfterFields of
                            [] ->
                                Print.SingleLine

                            _ :: _ ->
                                Print.MultipleLines
                    )

        recordVariablePrint : Print
        recordVariablePrint =
            Print.exactly
                (syntaxRecordExtension.recordVariable |> Elm.Syntax.Node.value)
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case commentsBeforeRecordVariable of
                    [] ->
                        recordVariablePrint

                    _ :: _ ->
                        commentsCollapsibleBeforeRecordVariable.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    commentsCollapsibleBeforeRecordVariable.lineSpread
                                )
                            |> Print.followedBy recordVariablePrint
                )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy printExactlyVerticalBarSpace
                    |> Print.followedBy
                        (fieldPrintsAndComments.reverse
                            |> Print.listReverseAndMapAndIntersperseAndFlatten
                                (\field ->
                                    let
                                        ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode ) =
                                            field.syntax

                                        lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                        lineSpreadBetweenNameAndValueNotConsideringComments () =
                                            lineSpreadInRange
                                                { start = fieldNameRange.start
                                                , end = fieldValueNode |> Elm.Syntax.Node.range |> .end
                                                }
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> field.valuePrint |> Print.lineSpread)
                                    in
                                    Print.withIndentIncreasedBy 2
                                        ((case field.maybeCommentsBeforeName of
                                            Nothing ->
                                                Print.exactly (fieldName ++ " :")

                                            Just commentsBeforeName ->
                                                commentsBeforeName.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            commentsBeforeName.lineSpread
                                                        )
                                                    |> Print.followedBy (Print.exactly (fieldName ++ " :"))
                                         )
                                            |> Print.followedBy
                                                (Print.withIndentAtNextMultipleOf4
                                                    ((case field.maybeCommentsBetweenNameAndValue of
                                                        Nothing ->
                                                            Print.spaceOrLinebreakIndented
                                                                (lineSpreadBetweenNameAndValueNotConsideringComments ())

                                                        Just commentsBetweenNameAndValue ->
                                                            Print.spaceOrLinebreakIndented
                                                                (commentsBetweenNameAndValue.lineSpread
                                                                    |> Print.lineSpreadMergeWith
                                                                        lineSpreadBetweenNameAndValueNotConsideringComments
                                                                )
                                                                |> Print.followedBy commentsBetweenNameAndValue.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBetweenNameAndValue.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> field.valuePrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                     )
                                                        |> Print.followedBy field.valuePrint
                                                    )
                                                )
                                        )
                                )
                                (Print.emptyOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyCommaSpace
                                )
                        )
                    |> -- yes, elm-format indents trailing comments
                       Print.followedBy
                        (case commentsAfterFields of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreak
                                    |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCurlyClosing


construct :
    { printArgumentParenthesizedIfSpaceSeparated :
        List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node a -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (Elm.Syntax.Node.Node String)
    ->
        { arguments : List (Elm.Syntax.Node.Node a)
        , fullRange : Elm.Syntax.Range.Range
        , start : String
        }
    -> Print
construct specific syntaxComments syntaxConstruct =
    let
        argumentPrintsAndCommentsBeforeReverse : List Print
        argumentPrintsAndCommentsBeforeReverse =
            syntaxConstruct.arguments
                |> List.foldl
                    (\argument soFar ->
                        let
                            print : Print
                            print =
                                specific.printArgumentParenthesizedIfSpaceSeparated syntaxComments
                                    argument
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = argument |> Elm.Syntax.Node.range |> .start
                                    }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBeforeArgument : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBeforeArgument =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBeforeArgument.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBeforeArgument.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = argument |> Elm.Syntax.Node.range |> .end
                        }
                    )
                    { reverse = []
                    , endLocation = syntaxConstruct.fullRange.start
                    }
                |> .reverse

        lineSpread : Print.LineSpread
        lineSpread =
            specific.lineSpreadMinimum
                |> Print.lineSpreadMergeWith
                    (\() ->
                        argumentPrintsAndCommentsBeforeReverse
                            |> Print.lineSpreadListMapAndCombine
                                Print.lineSpread
                    )
    in
    Print.exactly syntaxConstruct.start
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (argumentPrintsAndCommentsBeforeReverse
                    |> Print.listReverseAndMapAndFlatten
                        (\argumentPrintWithCommentsBefore ->
                            Print.spaceOrLinebreakIndented lineSpread
                                |> Print.followedBy
                                    argumentPrintWithCommentsBefore
                        )
                )
            )


tuple :
    { printPartNotParenthesized :
        List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node part -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : Elm.Syntax.Node.Node part
        , part1 : Elm.Syntax.Node.Node part
        }
    -> Print
tuple config syntaxComments syntaxTuple =
    let
        beforePart0Comments : List String
        beforePart0Comments =
            commentsInRange
                { start = syntaxTuple.fullRange.start
                , end = syntaxTuple.part0 |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart0CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart0CommentsCollapsible =
            collapsibleComments beforePart0Comments

        beforePart1Comments : List String
        beforePart1Comments =
            commentsInRange
                { start = syntaxTuple.part0 |> Elm.Syntax.Node.range |> .end
                , end = syntaxTuple.part1 |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart1CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart1CommentsCollapsible =
            collapsibleComments beforePart1Comments

        afterPart1Comments : List String
        afterPart1Comments =
            commentsInRange
                { start = syntaxTuple.part1 |> Elm.Syntax.Node.range |> .end
                , end = syntaxTuple.fullRange.end
                }
                syntaxComments

        part0Print : Print
        part0Print =
            config.printPartNotParenthesized syntaxComments syntaxTuple.part0

        part1Print : Print
        part1Print =
            config.printPartNotParenthesized syntaxComments syntaxTuple.part1

        lineSpread : Print.LineSpread
        lineSpread =
            config.lineSpreadMinimum
                |> Print.lineSpreadMergeWithStrict
                    beforePart0CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    beforePart1CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case afterPart1Comments of
                            _ :: _ ->
                                Print.MultipleLines

                            [] ->
                                Print.SingleLine
                    )
                |> Print.lineSpreadMergeWith
                    (\() -> part0Print |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> part1Print |> Print.lineSpread)
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case beforePart0Comments of
                    [] ->
                        part0Print

                    _ :: _ ->
                        beforePart0CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart0CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part0Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part0Print
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCommaSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((case beforePart1Comments of
                    [] ->
                        part1Print

                    _ :: _ ->
                        beforePart1CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart1CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part1Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part1Print
                 )
                    |> Print.followedBy
                        (case afterPart1Comments of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


triple :
    { printPartNotParenthesized :
        List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node part -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : Elm.Syntax.Node.Node part
        , part1 : Elm.Syntax.Node.Node part
        , part2 : Elm.Syntax.Node.Node part
        }
    -> Print
triple config syntaxComments syntaxTriple =
    let
        beforePart0Comments : List String
        beforePart0Comments =
            commentsInRange
                { start = syntaxTriple.fullRange.start
                , end = syntaxTriple.part0 |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart0CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart0CommentsCollapsible =
            collapsibleComments beforePart0Comments

        beforePart1Comments : List String
        beforePart1Comments =
            commentsInRange
                { start = syntaxTriple.part0 |> Elm.Syntax.Node.range |> .end
                , end = syntaxTriple.part1 |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart1CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart1CommentsCollapsible =
            collapsibleComments beforePart1Comments

        beforePart2Comments : List String
        beforePart2Comments =
            commentsInRange
                { start = syntaxTriple.part1 |> Elm.Syntax.Node.range |> .end
                , end = syntaxTriple.part2 |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart2CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart2CommentsCollapsible =
            collapsibleComments beforePart2Comments

        afterPart2Comments : List String
        afterPart2Comments =
            commentsInRange
                { start = syntaxTriple.part2 |> Elm.Syntax.Node.range |> .end
                , end = syntaxTriple.fullRange.end
                }
                syntaxComments

        part0Print : Print
        part0Print =
            config.printPartNotParenthesized syntaxComments syntaxTriple.part0

        part1Print : Print
        part1Print =
            config.printPartNotParenthesized syntaxComments syntaxTriple.part1

        part2Print : Print
        part2Print =
            config.printPartNotParenthesized syntaxComments syntaxTriple.part2

        lineSpread : Print.LineSpread
        lineSpread =
            config.lineSpreadMinimum
                |> Print.lineSpreadMergeWithStrict
                    beforePart0CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    beforePart1CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    beforePart2CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case afterPart2Comments of
                            _ :: _ ->
                                Print.MultipleLines

                            [] ->
                                Print.SingleLine
                    )
                |> Print.lineSpreadMergeWith
                    (\() -> part0Print |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> part1Print |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> part2Print |> Print.lineSpread)
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case beforePart0Comments of
                    [] ->
                        part0Print

                    _ :: _ ->
                        beforePart0CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart0CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part0Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part0Print
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCommaSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case beforePart1Comments of
                    [] ->
                        part1Print

                    _ :: _ ->
                        beforePart1CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart1CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part1Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part1Print
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCommaSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((case beforePart2Comments of
                    [] ->
                        part2Print

                    _ :: _ ->
                        beforePart2CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart2CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part2Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part2Print
                 )
                    |> Print.followedBy
                        (case afterPart2Comments of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


invalidNTuple :
    (b -> a -> Print)
    -> b
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : a
        , part1 : a
        , part2 : a
        , part3 : a
        , part4Up : List a
        }
    -> Print
invalidNTuple printPartNotParenthesized syntaxComments syntaxTuple =
    -- low-effort (eating comments etc) bc this shouldn't parse in the first place
    let
        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxTuple.fullRange
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            ((syntaxTuple.part0 :: syntaxTuple.part1 :: syntaxTuple.part2 :: syntaxTuple.part3 :: syntaxTuple.part4Up)
                |> Print.listMapAndIntersperseAndFlatten
                    (\part ->
                        Print.withIndentIncreasedBy 2
                            (printPartNotParenthesized syntaxComments part)
                    )
                    (Print.emptyOrLinebreakIndented lineSpread
                        |> Print.followedBy printExactlyCommaSpace
                    )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


recordLiteral :
    { nameValueSeparator : String
    , printValueNotParenthesized : List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node fieldValue -> Print
    }
    -> List (Elm.Syntax.Node.Node String)
    ->
        { fields :
            List
                (Elm.Syntax.Node.Node
                    ( Elm.Syntax.Node.Node String
                    , Elm.Syntax.Node.Node fieldValue
                    )
                )
        , fullRange : Elm.Syntax.Range.Range
        }
    -> Print
recordLiteral fieldSpecific syntaxComments syntaxRecord =
    case syntaxRecord.fields of
        [] ->
            printExactlyCurlyOpening
                |> Print.followedBy
                    (case commentsInRange syntaxRecord.fullRange syntaxComments of
                        [] ->
                            printExactlyCurlyClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlyCurlyClosing
                    )

        field0 :: field1Up ->
            let
                fieldPrintsAndComments :
                    { endLocation : Elm.Syntax.Range.Location
                    , reverse :
                        List
                            { syntax :
                                ( Elm.Syntax.Node.Node String
                                , Elm.Syntax.Node.Node fieldValue
                                )
                            , valuePrint : Print
                            , maybeCommentsBeforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                            , maybeCommentsBetweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                            }
                    }
                fieldPrintsAndComments =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )) soFar ->
                                let
                                    (Elm.Syntax.Node.Node fieldValueRange _) =
                                        fieldValueNode

                                    commentsBeforeName : List String
                                    commentsBeforeName =
                                        commentsInRange
                                            { start = soFar.endLocation, end = fieldNameRange.start }
                                            syntaxComments

                                    commentsBetweenNameAndValue : List String
                                    commentsBetweenNameAndValue =
                                        commentsInRange
                                            { start = fieldNameRange.start, end = fieldValueRange.start }
                                            syntaxComments
                                in
                                { endLocation = fieldValueRange.end
                                , reverse =
                                    { syntax = ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )
                                    , valuePrint = fieldSpecific.printValueNotParenthesized syntaxComments fieldValueNode
                                    , maybeCommentsBeforeName =
                                        case commentsBeforeName of
                                            [] ->
                                                Nothing

                                            comment0 :: comment1Up ->
                                                Just (collapsibleComments (comment0 :: comment1Up))
                                    , maybeCommentsBetweenNameAndValue =
                                        case commentsBetweenNameAndValue of
                                            [] ->
                                                Nothing

                                            comment0 :: comment1Up ->
                                                Just (collapsibleComments (comment0 :: comment1Up))
                                    }
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxRecord.fullRange.start
                            , reverse = []
                            }

                commentsAfterFields : List String
                commentsAfterFields =
                    commentsInRange
                        { start = fieldPrintsAndComments.endLocation
                        , end = syntaxRecord.fullRange.end
                        }
                        syntaxComments

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxRecord.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                fieldPrintsAndComments.reverse
                                    |> Print.lineSpreadListMapAndCombine
                                        (\field ->
                                            field.valuePrint
                                                |> Print.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() ->
                                                        case field.maybeCommentsBeforeName of
                                                            Nothing ->
                                                                Print.SingleLine

                                                            Just commentsBeforeName ->
                                                                commentsBeforeName.lineSpread
                                                    )
                                                |> Print.lineSpreadMergeWith
                                                    (\() ->
                                                        case field.maybeCommentsBetweenNameAndValue of
                                                            Nothing ->
                                                                Print.SingleLine

                                                            Just commentsBetweenNameAndValue ->
                                                                commentsBetweenNameAndValue.lineSpread
                                                    )
                                        )
                            )
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                case commentsAfterFields of
                                    [] ->
                                        Print.SingleLine

                                    _ :: _ ->
                                        Print.MultipleLines
                            )
            in
            printExactlyCurlyOpeningSpace
                |> Print.followedBy
                    (fieldPrintsAndComments.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\field ->
                                let
                                    ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValue ) =
                                        field.syntax

                                    lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                    lineSpreadBetweenNameAndValueNotConsideringComments () =
                                        lineSpreadInRange
                                            { start = fieldNameRange.start
                                            , end = fieldValue |> Elm.Syntax.Node.range |> .end
                                            }
                                            |> Print.lineSpreadMergeWith
                                                (\() -> field.valuePrint |> Print.lineSpread)

                                    nameSeparatorValuePrint : Print
                                    nameSeparatorValuePrint =
                                        Print.exactly (fieldName ++ " " ++ fieldSpecific.nameValueSeparator)
                                            |> Print.followedBy
                                                (Print.withIndentAtNextMultipleOf4
                                                    ((case field.maybeCommentsBetweenNameAndValue of
                                                        Nothing ->
                                                            Print.spaceOrLinebreakIndented
                                                                (lineSpreadBetweenNameAndValueNotConsideringComments ())

                                                        Just commentsBetweenNameAndValue ->
                                                            Print.spaceOrLinebreakIndented
                                                                (commentsBetweenNameAndValue.lineSpread
                                                                    |> Print.lineSpreadMergeWith
                                                                        lineSpreadBetweenNameAndValueNotConsideringComments
                                                                )
                                                                |> Print.followedBy commentsBetweenNameAndValue.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBetweenNameAndValue.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> field.valuePrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                     )
                                                        |> Print.followedBy field.valuePrint
                                                    )
                                                )
                                in
                                Print.withIndentIncreasedBy 2
                                    (case field.maybeCommentsBeforeName of
                                        Nothing ->
                                            nameSeparatorValuePrint

                                        Just commentsBeforeName ->
                                            commentsBeforeName.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        commentsBeforeName.lineSpread
                                                    )
                                                |> Print.followedBy nameSeparatorValuePrint
                                    )
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterFields of
                        [] ->
                            Print.spaceOrLinebreakIndented lineSpread

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                |> Print.followedBy (comments (comment0 :: comment1Up))
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlyCurlyClosing


{-| Print a name with its qualification (`[]` for no qualification)
-}
qualifiedReference : { qualification : List String, unqualified : String } -> String
qualifiedReference syntaxReference =
    case syntaxReference.qualification of
        [] ->
            syntaxReference.unqualified

        modulePartHead :: modulePartTail ->
            modulePartHead
                ++ (modulePartTail
                        |> listMapAndFlattenToString
                            (\modulePart -> "." ++ modulePart)
                   )
                ++ "."
                ++ syntaxReference.unqualified


lineSpreadBetweenRanges : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Print.LineSpread
lineSpreadBetweenRanges earlierRange laterRange =
    if laterRange.end.row - earlierRange.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


lineSpreadBetweenNodes : Elm.Syntax.Node.Node a_ -> Elm.Syntax.Node.Node b_ -> Print.LineSpread
lineSpreadBetweenNodes (Elm.Syntax.Node.Node earlierRange _) (Elm.Syntax.Node.Node laterRange _) =
    if laterRange.end.row - earlierRange.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


lineSpreadInNode : Elm.Syntax.Node.Node a_ -> Print.LineSpread
lineSpreadInNode (Elm.Syntax.Node.Node range _) =
    lineSpreadInRange range


typeFunctionNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , inType : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        , outType : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
typeFunctionNotParenthesized syntaxComments function =
    let
        afterArrowTypes :
            { beforeRightest : List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
            , rightest : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
            }
        afterArrowTypes =
            typeFunctionExpand function.outType

        afterArrowTypesBeforeRightestPrintsWithCommentsBefore :
            { endLocation : Elm.Syntax.Range.Location
            , reverse : List Print
            }
        afterArrowTypesBeforeRightestPrintsWithCommentsBefore =
            afterArrowTypes.beforeRightest
                |> List.foldl
                    (\afterArrowTypeNode soFar ->
                        let
                            (Elm.Syntax.Node.Node afterArrowTypeRange _) =
                                afterArrowTypeNode

                            print : Print
                            print =
                                typeParenthesizedIfFunction syntaxComments
                                    afterArrowTypeNode
                        in
                        { endLocation = afterArrowTypeRange.end
                        , reverse =
                            ((case
                                commentsInRange
                                    { start = soFar.endLocation, end = afterArrowTypeRange.start }
                                    syntaxComments
                              of
                                [] ->
                                    Print.spaceOrLinebreakIndented
                                        (print |> Print.lineSpread)

                                comment0 :: comment1Up ->
                                    let
                                        commentsBeforeAfterArrowType : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBeforeAfterArrowType =
                                            collapsibleComments (comment0 :: comment1Up)

                                        lineSpread : Print.LineSpread
                                        lineSpread =
                                            commentsBeforeAfterArrowType.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> print |> Print.lineSpread)
                                    in
                                    Print.spaceOrLinebreakIndented lineSpread
                                        |> Print.followedBy commentsBeforeAfterArrowType.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                lineSpread
                                            )
                             )
                                |> Print.followedBy print
                            )
                                :: soFar.reverse
                        }
                    )
                    { endLocation = function.inType |> Elm.Syntax.Node.range |> .end
                    , reverse = []
                    }

        commentsBeforeRightestAfterArrowType : List String
        commentsBeforeRightestAfterArrowType =
            commentsInRange
                { start = afterArrowTypesBeforeRightestPrintsWithCommentsBefore.endLocation
                , end = afterArrowTypes.rightest |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRightestAfterArrowType : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRightestAfterArrowType =
            collapsibleComments commentsBeforeRightestAfterArrowType

        inTypePrint : Print
        inTypePrint =
            typeParenthesizedIfFunction syntaxComments function.inType

        rightestAfterArrowTypePrint : Print
        rightestAfterArrowTypePrint =
            typeParenthesizedIfParenthesizedFunction syntaxComments
                afterArrowTypes.rightest

        rightestAfterArrowTypeWithCommentsBeforePrint : Print.Print
        rightestAfterArrowTypeWithCommentsBeforePrint =
            (case commentsBeforeRightestAfterArrowType of
                [] ->
                    Print.spaceOrLinebreakIndented
                        (rightestAfterArrowTypePrint |> Print.lineSpread)

                comment0 :: comment1Up ->
                    let
                        commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                        commentsCollapsible =
                            collapsibleComments (comment0 :: comment1Up)

                        lineSpread : Print.LineSpread
                        lineSpread =
                            commentsCollapsible.lineSpread
                                |> Print.lineSpreadMergeWith
                                    (\() -> rightestAfterArrowTypePrint |> Print.lineSpread)
                    in
                    Print.spaceOrLinebreakIndented lineSpread
                        |> Print.followedBy commentsCollapsible.print
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented
                                lineSpread
                            )
            )
                |> Print.followedBy rightestAfterArrowTypePrint

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            lineSpreadInRange function.fullRange
                |> Print.lineSpreadMergeWith
                    (\() -> inTypePrint |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> rightestAfterArrowTypeWithCommentsBeforePrint |> Print.lineSpread)
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRightestAfterArrowType.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        afterArrowTypesBeforeRightestPrintsWithCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
    in
    inTypePrint
        |> Print.followedBy
            (afterArrowTypesBeforeRightestPrintsWithCommentsBefore.reverse
                |> Print.listReverseAndMapAndFlatten
                    (\printWithCommentsBefore ->
                        Print.spaceOrLinebreakIndented fullLineSpread
                            |> Print.followedBy printExactlyMinusGreaterThan
                            |> Print.followedBy
                                (Print.withIndentAtNextMultipleOf4
                                    printWithCommentsBefore
                                )
                    )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread
                |> Print.followedBy printExactlyMinusGreaterThan
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        rightestAfterArrowTypeWithCommentsBeforePrint
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
        Just _ ->
            typeParenthesized syntaxComments typeNode

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

        Elm.Syntax.TypeAnnotation.GenericType _ ->
            Nothing

        Elm.Syntax.TypeAnnotation.Typed _ _ ->
            Nothing

        Elm.Syntax.TypeAnnotation.Unit ->
            Nothing

        Elm.Syntax.TypeAnnotation.Tupled _ ->
            Nothing

        Elm.Syntax.TypeAnnotation.Record _ ->
            Nothing

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
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
                    Elm.Syntax.Node.Node typeRange Elm.Syntax.TypeAnnotation.Unit

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled [ part0, part1, part2 ])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Tupled (part0 :: part1 :: part2 :: part3 :: part4Up))

        Elm.Syntax.TypeAnnotation.Record fields ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.Record fields)

        Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode)

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Elm.Syntax.Node.Node typeRange (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType)


typeFunctionExpand :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        { beforeRightest :
            List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
        , rightest : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }
typeFunctionExpand typeNode =
    case typeNode of
        Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType) ->
            let
                outTypeExpanded :
                    { beforeRightest : List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
                    , rightest : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
                    }
                outTypeExpanded =
                    typeFunctionExpand outType
            in
            { beforeRightest = inType :: outTypeExpanded.beforeRightest
            , rightest = outTypeExpanded.rightest
            }

        typeNodeNotFunction ->
            { beforeRightest = [], rightest = typeNodeNotFunction }


typeParenthesizedIfSpaceSeparated :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfSpaceSeparated syntaxComments typeNode =
    if typeIsSpaceSeparated (typeNode |> Elm.Syntax.Node.value) then
        typeParenthesized syntaxComments typeNode

    else
        typeNotParenthesized syntaxComments typeNode


typeParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesized syntaxComments typeNode =
    parenthesized typeNotParenthesized
        { notParenthesized = typeNode |> typeToNotParenthesized
        , fullRange = typeNode |> Elm.Syntax.Node.range
        }
        syntaxComments


parenthesized :
    (List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node a -> Print)
    ->
        { notParenthesized : Elm.Syntax.Node.Node a
        , fullRange : Elm.Syntax.Range.Range
        }
    -> List (Elm.Syntax.Node.Node String)
    -> Print
parenthesized printNotParenthesized syntax syntaxComments =
    let
        commentsBeforeInner : List String
        commentsBeforeInner =
            commentsInRange
                { start = syntax.fullRange.start
                , end = syntax.notParenthesized |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsAfterInner : List String
        commentsAfterInner =
            commentsInRange
                { start = syntax.notParenthesized |> Elm.Syntax.Node.range |> .end
                , end = syntax.fullRange.end
                }
                syntaxComments

        notParenthesizedPrint : Print
        notParenthesizedPrint =
            printNotParenthesized syntaxComments syntax.notParenthesized

        commentsBeforeInnerCollapsible : { print : Print, lineSpread : Print.LineSpread }
        commentsBeforeInnerCollapsible =
            collapsibleComments commentsBeforeInner

        commentsAfterInnerCollapsible : { print : Print, lineSpread : Print.LineSpread }
        commentsAfterInnerCollapsible =
            collapsibleComments commentsAfterInner

        lineSpread : Print.LineSpread
        lineSpread =
            notParenthesizedPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    commentsBeforeInnerCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    commentsAfterInnerCollapsible.lineSpread
    in
    printExactlyParensOpening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                ((case commentsBeforeInner of
                    [] ->
                        notParenthesizedPrint

                    _ :: _ ->
                        commentsBeforeInnerCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented lineSpread)
                            |> Print.followedBy notParenthesizedPrint
                 )
                    |> Print.followedBy
                        (case commentsAfterInner of
                            [] ->
                                Print.empty

                            _ :: _ ->
                                Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy
                                        commentsAfterInnerCollapsible.print
                        )
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


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


{-| Print an [`Elm.Syntax.TypeAnnotation.TypeAnnotation`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments (Elm.Syntax.Node.Node fullRange syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            printExactlyParensOpeningParensClosed

        Elm.Syntax.TypeAnnotation.GenericType name ->
            Print.exactly name

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( referenceQualification, referenceUnqualified )) arguments ->
            construct
                { printArgumentParenthesizedIfSpaceSeparated =
                    typeParenthesizedIfSpaceSeparated
                , lineSpreadMinimum = lineSpreadInRange fullRange
                }
                syntaxComments
                { fullRange = fullRange
                , start =
                    qualifiedReference
                        { qualification = referenceQualification
                        , unqualified = referenceUnqualified
                        }
                , arguments = arguments
                }

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    printExactlyParensOpeningParensClosed

                [ inParens ] ->
                    let
                        commentsBeforeInParens : List String
                        commentsBeforeInParens =
                            commentsInRange { start = fullRange.start, end = inParens |> Elm.Syntax.Node.range |> .start } syntaxComments

                        commentsAfterInParens : List String
                        commentsAfterInParens =
                            commentsInRange { start = inParens |> Elm.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
                    in
                    case ( commentsBeforeInParens, commentsAfterInParens ) of
                        ( [], [] ) ->
                            typeNotParenthesized syntaxComments inParens

                        _ ->
                            parenthesized typeNotParenthesized
                                { notParenthesized = inParens |> typeToNotParenthesized
                                , fullRange = fullRange
                                }
                                syntaxComments

                [ part0, part1 ] ->
                    tuple
                        { printPartNotParenthesized = typeNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { part0 = part0
                        , part1 = part1
                        , fullRange = fullRange
                        }

                [ part0, part1, part2 ] ->
                    triple
                        { printPartNotParenthesized = typeNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { part0 = part0
                        , part1 = part1
                        , part2 = part2
                        , fullRange = fullRange
                        }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    invalidNTuple typeNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1, part2 = part2, part3 = part3, part4Up = part4Up }

        Elm.Syntax.TypeAnnotation.Record fields ->
            recordLiteral
                { printValueNotParenthesized = typeNotParenthesized
                , nameValueSeparator = ":"
                }
                syntaxComments
                { fullRange = fullRange, fields = fields }

        Elm.Syntax.TypeAnnotation.GenericRecord recordVariable (Elm.Syntax.Node.Node _ fields) ->
            typeRecordExtension syntaxComments
                { fullRange = fullRange
                , recordVariable = recordVariable
                , fields = fields
                }

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            { fullRange = fullRange, inType = inType, outType = outType }
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

                        Elm.Syntax.Declaration.FunctionDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.AliasDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.Destructuring _ _ ->
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

                                            Elm.Syntax.Declaration.FunctionDeclaration _ ->
                                                Nothing

                                            Elm.Syntax.Declaration.AliasDeclaration _ ->
                                                Nothing

                                            Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                                Nothing

                                            Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                Nothing

                                            Elm.Syntax.Declaration.Destructuring _ _ ->
                                                Nothing
                                in
                                { print =
                                    soFar.print
                                        |> Print.followedBy
                                            (case commentsInRange { start = soFar.previousRange.end, end = declarationRange.start } context.comments of
                                                comment0 :: comment1Up ->
                                                    printLinebreakLinebreakLinebreak
                                                        |> Print.followedBy
                                                            (moduleLevelCommentsBeforeDeclaration
                                                                { comment0 = comment0, comment1Up = comment1Up }
                                                            )
                                                        |> Print.followedBy
                                                            (declaration
                                                                { comments = commentNodesInRange declarationRange context.comments
                                                                , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                                }
                                                                syntaxDeclaration
                                                            )

                                                [] ->
                                                    linebreaksFollowedByDeclaration
                                                        { comments = commentNodesInRange declarationRange context.comments
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
            case locationCompareFast headCommentRange.start range.start of
                LT ->
                    firstCommentInRange range tailComments

                EQ ->
                    Just (Elm.Syntax.Node.Node headCommentRange headComment)

                GT ->
                    case locationCompareFast headCommentRange.end range.end of
                        GT ->
                            Nothing

                        LT ->
                            Just (Elm.Syntax.Node.Node headCommentRange headComment)

                        EQ ->
                            Just (Elm.Syntax.Node.Node headCommentRange headComment)


locationCompareFast : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Basics.Order
locationCompareFast left right =
    if left.row - right.row < 0 then
        LT

    else if left.row - right.row > 0 then
        GT

    else
        Basics.compare left.column right.column


linebreaksFollowedByDeclaration :
    { portDocumentationComment : Maybe (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Declaration.Declaration
    -> Print
linebreaksFollowedByDeclaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        Elm.Syntax.Declaration.FunctionDeclaration syntaxExpressionDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationExpression syntaxComments.comments syntaxExpressionDeclaration)

        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration)

        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationChoiceType syntaxComments.comments syntaxChoiceTypeDeclaration)

        Elm.Syntax.Declaration.PortDeclaration signature ->
            printLinebreakLinebreakLinebreak
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
            printLinebreakLinebreakLinebreak
                |> Print.followedBy
                    (declarationDestructuring syntaxComments.comments destructuringPattern destructuringExpression)


listFilledLast : a -> List a -> a
listFilledLast head tail =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast tailHead tailTail


printDocumentationComment : String -> Print
printDocumentationComment documentationCommentIncludingOpeningAndClosingTokens =
    let
        contentLines : List String
        contentLines =
            documentationCommentIncludingOpeningAndClosingTokens
                |> String.dropLeft 3
                |> String.dropRight 2
                |> String.trim
                |> String.lines
                |> List.map String.trimRight
    in
    if contentLines |> List.all String.isEmpty then
        printDocumentationCommentEmpty

    else
        printExactlyCurlyBraceOpeningMinusVerticalBar
            |> Print.followedBy
                (contentLines
                    |> Print.listMapAndIntersperseAndFlatten Print.exactly
                        Print.linebreak
                )
            |> Print.followedBy
                (if contentLines |> List.any String.isEmpty then
                    linebreakFollowedByLinebreakFollowedByMinusCurlyBraceClosing

                 else
                    linebreakFollowedByMinusCurlyBraceClosing
                )


printExactlyCurlyBraceOpeningMinusVerticalBar : Print
printExactlyCurlyBraceOpeningMinusVerticalBar =
    Print.exactly "{-| "


printDocumentationCommentEmpty : Print
printDocumentationCommentEmpty =
    Print.exactly "{-| -}"


linebreakFollowedByLinebreakFollowedByMinusCurlyBraceClosing : Print
linebreakFollowedByLinebreakFollowedByMinusCurlyBraceClosing =
    Print.linebreak
        |> Print.followedBy Print.linebreak
        |> Print.followedBy (Print.exactly "-}")


linebreakFollowedByMinusCurlyBraceClosing : Print
linebreakFollowedByMinusCurlyBraceClosing =
    Print.linebreak
        |> Print.followedBy (Print.exactly "-}")


declarationDestructuring :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
declarationDestructuring syntaxComments destructuringPattern destructuringExpression =
    -- invalid syntax
    patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern
        |> Print.followedBy (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
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


{-| Print an [`Elm.Syntax.Signature.Signature`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Signature#Signature)
as `name : Type`
-}
declarationSignature :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Signature.Signature
    -> Print
declarationSignature syntaxComments signature =
    let
        typePrint : Print
        typePrint =
            typeNotParenthesized syntaxComments signature.typeAnnotation

        rangeBetweenNameAndType : Elm.Syntax.Range.Range
        rangeBetweenNameAndType =
            { start = signature.name |> Elm.Syntax.Node.range |> .end
            , end = signature.typeAnnotation |> Elm.Syntax.Node.range |> .start
            }
    in
    Print.exactly
        ((signature.name |> Elm.Syntax.Node.value)
            ++ " :"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case commentsInRange rangeBetweenNameAndType syntaxComments of
                    [] ->
                        Print.spaceOrLinebreakIndented (Print.lineSpread typePrint)

                    comment0 :: comment1Up ->
                        Print.linebreakIndented
                            |> Print.followedBy
                                (comments (comment0 :: comment1Up))
                            |> Print.followedBy Print.linebreakIndented
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
            printExactlyPortSpace

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            printDocumentationComment documentation
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
                |> Print.followedBy printExactlyPortSpace
    )
        |> Print.followedBy (declarationSignature syntaxComments.comments signature)


{-| Print an [`Elm.Syntax.TypeAlias.TypeAlias`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration =
    let
        parameterPrintsWithCommentsBeforeReverse : List Print
        parameterPrintsWithCommentsBeforeReverse =
            syntaxTypeAliasDeclaration.generics
                |> List.foldl
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterName |> Elm.Syntax.Node.range

                            parameterNamePrint : Print
                            parameterNamePrint =
                                Print.exactly (parameterName |> Elm.Syntax.Node.value)
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterPrintedRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    parameterNamePrint

                                comment0 :: comment1Up ->
                                    let
                                        commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                                        commentsCollapsible =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsCollapsible.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                commentsCollapsible.lineSpread
                                            )
                                        |> Print.followedBy parameterNamePrint
                            )
                                :: soFar.reverse
                        , endLocation = parameterPrintedRange.end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxTypeAliasDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }
                |> .reverse

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBeforeReverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        rangeBetweenParametersAndType : Elm.Syntax.Range.Range
        rangeBetweenParametersAndType =
            case syntaxTypeAliasDeclaration.generics of
                [] ->
                    { start =
                        syntaxTypeAliasDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    , end = syntaxTypeAliasDeclaration.typeAnnotation |> Elm.Syntax.Node.range |> .start
                    }

                parameter0 :: parameter1Up ->
                    { start =
                        listFilledLast parameter0 parameter1Up
                            |> Elm.Syntax.Node.range
                            |> .end
                    , end = syntaxTypeAliasDeclaration.typeAnnotation |> Elm.Syntax.Node.range |> .start
                    }

        aliasedTypePrint : Print
        aliasedTypePrint =
            typeNotParenthesized syntaxComments
                syntaxTypeAliasDeclaration.typeAnnotation
    in
    (case syntaxTypeAliasDeclaration.documentation of
        Nothing ->
            printExactlyTypeSpaceAlias

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            printDocumentationComment documentation
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
                |> Print.followedBy printExactlyTypeSpaceAlias
    )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (Print.exactly (syntaxTypeAliasDeclaration.name |> Elm.Syntax.Node.value))
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (parameterPrintsWithCommentsBeforeReverse
                                |> Print.listReverseAndMapAndFlatten
                                    (\parameterPrint ->
                                        Print.spaceOrLinebreakIndented parametersLineSpread
                                            |> Print.followedBy parameterPrint
                                    )
                            )
                        )
                    |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                    |> Print.followedBy printExactlyEquals
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsInRange rangeBetweenParametersAndType syntaxComments of
                                    [] ->
                                        aliasedTypePrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy aliasedTypePrint
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
        parameterPrints :
            { endLocation : Elm.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrints =
            syntaxChoiceTypeDeclaration.generics
                |> List.foldl
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterName |> Elm.Syntax.Node.range

                            parameterNamePrint : Print
                            parameterNamePrint =
                                Print.exactly (parameterName |> Elm.Syntax.Node.value)
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterPrintedRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    parameterNamePrint

                                comment0 :: comment1Up ->
                                    let
                                        commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                                        commentsCollapsible =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsCollapsible.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                commentsCollapsible.lineSpread
                                            )
                                        |> Print.followedBy parameterNamePrint
                            )
                                :: soFar.reverse
                        , endLocation = parameterPrintedRange.end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxChoiceTypeDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints.reverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        variantPrintsWithCommentsBeforeReverse : List Print
        variantPrintsWithCommentsBeforeReverse =
            syntaxChoiceTypeDeclaration.constructors
                |> List.foldl
                    (\(Elm.Syntax.Node.Node variantRange variant) soFar ->
                        let
                            variantPrint : Print
                            variantPrint =
                                construct
                                    { printArgumentParenthesizedIfSpaceSeparated =
                                        typeParenthesizedIfSpaceSeparated
                                    , lineSpreadMinimum = Print.SingleLine
                                    }
                                    syntaxComments
                                    { start = variant.name |> Elm.Syntax.Node.value
                                    , fullRange = variantRange
                                    , arguments = variant.arguments
                                    }

                            commentsVariantPrint : Print
                            commentsVariantPrint =
                                case commentsInRange { start = soFar.endLocation, end = variant.name |> Elm.Syntax.Node.range |> .start } syntaxComments of
                                    [] ->
                                        variantPrint

                                    comment0 :: comment1Up ->
                                        let
                                            commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                                            commentsCollapsible =
                                                collapsibleComments (comment0 :: comment1Up)
                                        in
                                        commentsCollapsible.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsCollapsible.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> variantPrint |> Print.lineSpread)
                                                    )
                                                )
                                            |> Print.followedBy variantPrint
                        in
                        { reverse = commentsVariantPrint :: soFar.reverse
                        , endLocation = variantRange.end
                        }
                    )
                    { reverse = []
                    , endLocation = parameterPrints.endLocation
                    }
                |> .reverse
    in
    (case syntaxChoiceTypeDeclaration.documentation of
        Nothing ->
            printExactlyType

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            printDocumentationComment documentation
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
                |> Print.followedBy printExactlyType
    )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (Print.exactly
                            (syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                        )
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (parameterPrints.reverse
                                |> Print.listReverseAndMapAndFlatten
                                    (\parameterPrint ->
                                        Print.spaceOrLinebreakIndented parametersLineSpread
                                            |> Print.followedBy parameterPrint
                                    )
                            )
                        )
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy printExactlyEqualsSpace
                    |> Print.followedBy
                        (variantPrintsWithCommentsBeforeReverse
                            |> Print.listReverseAndMapAndIntersperseAndFlatten
                                (\variantPrint -> Print.withIndentIncreasedBy 2 variantPrint)
                                printLinebreakIndentedVerticalBarSpace
                        )
                )
            )


{-| Print an [`Elm.Syntax.Infix.Infix`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Infix#Infix) declaration
-}
declarationInfix : Elm.Syntax.Infix.Infix -> Print
declarationInfix syntaxInfixDeclaration =
    Print.exactly
        ("infix "
            ++ infixDirection (syntaxInfixDeclaration.direction |> Elm.Syntax.Node.value)
            ++ " "
            ++ String.fromInt (syntaxInfixDeclaration.precedence |> Elm.Syntax.Node.value)
            ++ " ("
            ++ (syntaxInfixDeclaration.operator |> Elm.Syntax.Node.value)
            ++ ") = "
            ++ (syntaxInfixDeclaration.function |> Elm.Syntax.Node.value)
        )


infixDirection : Elm.Syntax.Infix.InfixDirection -> String
infixDirection syntaxInfixDirection =
    case syntaxInfixDirection of
        Elm.Syntax.Infix.Left ->
            "left "

        Elm.Syntax.Infix.Right ->
            "right"

        Elm.Syntax.Infix.Non ->
            "non  "


declarationExpressionImplementation :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.FunctionImplementation
    -> Print
declarationExpressionImplementation syntaxComments implementation =
    let
        parameterPrintsWithCommentsBefore :
            { endLocation : Elm.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrintsWithCommentsBefore =
            implementation.arguments
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : Elm.Syntax.Range.Range
                            parameterRange =
                                parameterPattern |> Elm.Syntax.Node.range

                            parameterPrint : Print
                            parameterPrint =
                                patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    parameterPrint

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> parameterPrint |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy parameterPrint
                            )
                                :: soFar.reverse
                        , endLocation = parameterRange.end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        implementation.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBefore.reverse
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread

        commentsBetweenParametersAndResult : List String
        commentsBetweenParametersAndResult =
            commentsInRange
                { start = parameterPrintsWithCommentsBefore.endLocation
                , end =
                    implementation.expression
                        |> Elm.Syntax.Node.range
                        |> .start
                }
                syntaxComments

        expressionPrint : Print
        expressionPrint =
            expressionNotParenthesized syntaxComments
                implementation.expression
    in
    Print.exactly (implementation.name |> Elm.Syntax.Node.value)
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (parameterPrintsWithCommentsBefore.reverse
                    |> Print.listReverseAndMapAndFlatten
                        (\parameterPrintWithCommentsBefore ->
                            Print.spaceOrLinebreakIndented parametersLineSpread
                                |> Print.followedBy parameterPrintWithCommentsBefore
                        )
                    |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                    |> Print.followedBy printExactlyEquals
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsBetweenParametersAndResult of
                                    [] ->
                                        expressionPrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy expressionPrint
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
            printLinebreakLinebreakLinebreak
                |> Print.followedBy
                    (moduleLevelComments (comment0 :: comment1Up))
                |> Print.followedBy printLinebreakLinebreak


{-| Print an [`Elm.Syntax.Expression.Function`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.Function
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration =
    let
        implementationPrint : Print
        implementationPrint =
            declarationExpressionImplementation
                syntaxComments
                (syntaxExpressionDeclaration.declaration |> Elm.Syntax.Node.value)

        withoutDocumentationPrint : Print
        withoutDocumentationPrint =
            case syntaxExpressionDeclaration.signature of
                Nothing ->
                    implementationPrint

                Just (Elm.Syntax.Node.Node signatureRange signature) ->
                    let
                        commentsBetweenSignatureAndImplementationName : List String
                        commentsBetweenSignatureAndImplementationName =
                            commentsInRange
                                { start = signatureRange.end
                                , end =
                                    syntaxExpressionDeclaration.declaration
                                        |> Elm.Syntax.Node.range
                                        |> .start
                                }
                                syntaxComments
                    in
                    declarationSignature syntaxComments signature
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            (case commentsBetweenSignatureAndImplementationName of
                                [] ->
                                    implementationPrint

                                comment0 :: comment1Up ->
                                    printLinebreakLinebreakLinebreak
                                        |> Print.followedBy
                                            (moduleLevelComments (comment0 :: comment1Up))
                                        |> Print.followedBy printLinebreakLinebreak
                                        |> Print.followedBy implementationPrint
                            )
    in
    case syntaxExpressionDeclaration.documentation of
        Nothing ->
            withoutDocumentationPrint

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            printDocumentationComment documentation
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
                |> Print.followedBy withoutDocumentationPrint


expressionParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesized syntaxComments expressionNode =
    parenthesized expressionNotParenthesized
        { notParenthesized = expressionNode |> expressionToNotParenthesized
        , fullRange = expressionNode |> Elm.Syntax.Node.range
        }
        syntaxComments


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
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            printExactlyParensOpeningParensClosed

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
            Print.exactly
                (qualifiedReference { qualification = qualification, unqualified = unqualified })

        Elm.Syntax.Expression.IfBlock condition onTrue onFalse ->
            expressionIfThenElse syntaxComments
                { fullRange = fullRange
                , condition = condition
                , conditionLineSpreadMinimum = Print.SingleLine
                , onTrue = onTrue
                , onFalse = onFalse
                }

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Print.exactly ("(" ++ operatorSymbol ++ ")")

        Elm.Syntax.Expression.Operator operatorSymbol ->
            -- invalid syntax
            Print.exactly operatorSymbol

        Elm.Syntax.Expression.Integer int ->
            Print.exactly (intLiteral int)

        Elm.Syntax.Expression.Hex int ->
            Print.exactly (hexLiteral int)

        Elm.Syntax.Expression.Floatable float ->
            Print.exactly (floatLiteral float)

        Elm.Syntax.Expression.Negation negated ->
            printExpressionNegation syntaxComments negated

        Elm.Syntax.Expression.Literal string ->
            stringLiteral (Elm.Syntax.Node.Node fullRange string)

        Elm.Syntax.Expression.CharLiteral char ->
            Print.exactly (charLiteral char)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    printExactlyParensOpeningParensClosed

                [ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    let
                        commentsBeforeInParens : List String
                        commentsBeforeInParens =
                            commentsInRange { start = fullRange.start, end = inParens |> Elm.Syntax.Node.range |> .start } syntaxComments

                        commentsAfterInParens : List String
                        commentsAfterInParens =
                            commentsInRange { start = inParens |> Elm.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
                    in
                    case ( commentsBeforeInParens, commentsAfterInParens ) of
                        ( [], [] ) ->
                            expressionNotParenthesized syntaxComments inParens

                        _ ->
                            parenthesized expressionNotParenthesized
                                { notParenthesized = inParens |> expressionToNotParenthesized
                                , fullRange = fullRange
                                }
                                syntaxComments

                [ part0, part1 ] ->
                    tuple
                        { printPartNotParenthesized = expressionNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1 }

                [ part0, part1, part2 ] ->
                    triple
                        { printPartNotParenthesized = expressionNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { fullRange = fullRange
                        , part0 = part0
                        , part1 = part1
                        , part2 = part2
                        }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    invalidNTuple expressionNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1, part2 = part2, part3 = part3, part4Up = part4Up }

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange { start = fullRange.start, end = inParens |> Elm.Syntax.Node.range |> .start } syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange { start = inParens |> Elm.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    expressionNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized expressionNotParenthesized
                        { notParenthesized = inParens |> expressionToNotParenthesized
                        , fullRange = fullRange
                        }
                        syntaxComments

        Elm.Syntax.Expression.LetExpression syntaxLetIn ->
            case syntaxLetIn.declarations of
                [] ->
                    -- invalid syntax
                    expressionNotParenthesized syntaxComments syntaxLetIn.expression

                letDeclaration0 :: letDeclaration1Up ->
                    expressionLetIn syntaxComments
                        { fullRange = fullRange
                        , letDeclaration0 = letDeclaration0
                        , letDeclaration1Up = letDeclaration1Up
                        , result = syntaxLetIn.expression
                        }

        Elm.Syntax.Expression.CaseExpression syntaxCaseOf ->
            expressionCaseOf syntaxComments
                { fullRange = fullRange
                , expression = syntaxCaseOf.expression
                , cases = syntaxCaseOf.cases
                }

        Elm.Syntax.Expression.LambdaExpression syntaxLambda ->
            expressionLambda syntaxComments (Elm.Syntax.Node.Node fullRange syntaxLambda)

        Elm.Syntax.Expression.RecordExpr fields ->
            recordLiteral
                { printValueNotParenthesized = expressionNotParenthesized
                , nameValueSeparator = "="
                }
                syntaxComments
                { fullRange = fullRange, fields = fields }

        Elm.Syntax.Expression.ListExpr elements ->
            expressionList syntaxComments { fullRange = fullRange, elements = elements }

        Elm.Syntax.Expression.RecordAccess syntaxRecord (Elm.Syntax.Node.Node _ accessedFieldName) ->
            expressionParenthesizedIfSpaceSeparated syntaxComments syntaxRecord
                |> Print.followedBy (Print.exactly ("." ++ accessedFieldName))

        Elm.Syntax.Expression.RecordAccessFunction dotFieldName ->
            Print.exactly ("." ++ (dotFieldName |> String.replace "." ""))

        Elm.Syntax.Expression.RecordUpdateExpression recordVariableNode fields ->
            expressionRecordUpdate syntaxComments
                { fullRange = fullRange
                , recordVariable = recordVariableNode
                , fields = fields
                }

        Elm.Syntax.Expression.GLSLExpression glsl ->
            expressionGlsl glsl


printExpressionNegation :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
printExpressionNegation syntaxComments negated =
    if negated |> expressionIsBase10Zero then
        printExactlyZero

    else if negated |> expressionIsBase16Zero then
        printExactlyZeroXZeroZero

    else
        printExactlyMinus
            |> Print.followedBy
                (Print.withIndentIncreasedBy 1
                    (case negated |> expressionToNotParenthesized of
                        Elm.Syntax.Node.Node doublyNegatedRange (Elm.Syntax.Expression.Negation doublyNegated) ->
                            expressionParenthesized syntaxComments
                                (Elm.Syntax.Node.Node doublyNegatedRange (Elm.Syntax.Expression.Negation doublyNegated))

                        negatedNotNegationOrIntegerZero ->
                            expressionParenthesizedIfSpaceSeparated syntaxComments
                                negatedNotNegationOrIntegerZero
                    )
                )


expressionIsBase10Zero : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Bool
expressionIsBase10Zero expression =
    case expression |> expressionToNotParenthesized |> Elm.Syntax.Node.value of
        Elm.Syntax.Expression.Integer 0 ->
            True

        Elm.Syntax.Expression.Negation doublyNegated ->
            expressionIsBase10Zero doublyNegated

        _ ->
            False


expressionIsBase16Zero : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Bool
expressionIsBase16Zero expression =
    case expression |> expressionToNotParenthesized |> Elm.Syntax.Node.value of
        Elm.Syntax.Expression.Hex 0 ->
            True

        Elm.Syntax.Expression.Negation doublyNegated ->
            expressionIsBase16Zero doublyNegated

        _ ->
            False


expressionGlsl : String -> Print
expressionGlsl glslContent =
    Print.exactly "[glsl|"
        |> Print.followedBy
            (glslContent
                |> String.lines
                |> Print.listMapAndIntersperseAndFlatten
                    Print.exactly
                    Print.linebreak
            )
        |> Print.followedBy (Print.exactly "|]")


floatLiteral : Float -> String
floatLiteral float =
    if (float |> Basics.truncate |> Basics.toFloat) == float then
        String.fromFloat float ++ ".0"

    else
        String.fromFloat float


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
        appliedPrint : Print
        appliedPrint =
            expressionParenthesizedIfSpaceSeparated syntaxComments
                syntaxCall.applied

        commentsBeforeArgument0 : List String
        commentsBeforeArgument0 =
            commentsInRange
                { start = syntaxCall.applied |> Elm.Syntax.Node.range |> .end
                , end =
                    syntaxCall.argument0
                        |> Elm.Syntax.Node.range
                        |> .start
                }
                syntaxComments

        collapsibleCommentsBeforeArgument0 : { print : Print, lineSpread : Print.LineSpread }
        collapsibleCommentsBeforeArgument0 =
            commentsBeforeArgument0 |> collapsibleComments

        argument0Print : Print
        argument0Print =
            expressionParenthesizedIfSpaceSeparated syntaxComments
                syntaxCall.argument0

        argument1UpPrintsWithCommentsBeforeReverse : List Print
        argument1UpPrintsWithCommentsBeforeReverse =
            syntaxCall.argument1Up
                |> List.foldl
                    (\argument soFar ->
                        let
                            print : Print
                            print =
                                expressionParenthesizedIfSpaceSeparated syntaxComments argument
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = argument |> Elm.Syntax.Node.range |> .start
                                    }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = argument |> Elm.Syntax.Node.range |> .end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxCall.argument0
                            |> Elm.Syntax.Node.range
                            |> .end
                    }
                |> .reverse

        argument0LineSpread : Print.LineSpread
        argument0LineSpread =
            appliedPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        lineSpreadBetweenNodes
                            syntaxCall.applied
                            syntaxCall.argument0
                    )
                |> Print.lineSpreadMergeWithStrict
                    collapsibleCommentsBeforeArgument0.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> argument0Print |> Print.lineSpread)

        argument1UpLineSpread : Print.LineSpread
        argument1UpLineSpread =
            lineSpreadInRange syntaxCall.fullRange
                |> Print.lineSpreadMergeWithStrict argument0LineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        argument1UpPrintsWithCommentsBeforeReverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
    in
    appliedPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented argument0LineSpread
                    |> Print.followedBy
                        (case commentsBeforeArgument0 of
                            [] ->
                                argument0Print

                            _ :: _ ->
                                collapsibleCommentsBeforeArgument0.print
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            (collapsibleCommentsBeforeArgument0.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> argument0Print |> Print.lineSpread)
                                            )
                                        )
                                    |> Print.followedBy argument0Print
                        )
                    |> Print.followedBy
                        (argument1UpPrintsWithCommentsBeforeReverse
                            |> Print.listReverseAndMapAndFlatten
                                (\argumentPrintWithCommentsBefore ->
                                    Print.spaceOrLinebreakIndented argument1UpLineSpread
                                        |> Print.followedBy
                                            argumentPrintWithCommentsBefore
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
    -> Print
expressionOperation syntaxComments syntaxOperation =
    let
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
            expressionOperationExpand syntaxOperation.left
                syntaxOperation.operator
                syntaxOperation.right

        beforeRightestPrintsAndComments :
            { reverse :
                List
                    { operator : String
                    , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    , maybeCommentsBeforeExpression :
                        Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            , endLocation : Elm.Syntax.Range.Location
            }
        beforeRightestPrintsAndComments =
            operationExpanded.beforeRightestOperatorExpressionChain
                |> List.foldl
                    (\operatorAndExpressionBeforeRightest soFar ->
                        let
                            expressionRange : Elm.Syntax.Range.Range
                            expressionRange =
                                operatorAndExpressionBeforeRightest.expression
                                    |> Elm.Syntax.Node.range

                            commentsBefore : List String
                            commentsBefore =
                                commentsInRange
                                    { start = soFar.endLocation, end = expressionRange.start }
                                    syntaxComments
                        in
                        { endLocation = expressionRange.end
                        , reverse =
                            { operator = operatorAndExpressionBeforeRightest.operator
                            , expression = operatorAndExpressionBeforeRightest.expression
                            , maybeCommentsBeforeExpression =
                                case commentsBefore of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            }
                                :: soFar.reverse
                        }
                    )
                    { endLocation =
                        operationExpanded.leftest |> Elm.Syntax.Node.range |> .end
                    , reverse = []
                    }

        commentsBeforeRightestExpression : List String
        commentsBeforeRightestExpression =
            commentsInRange
                { start = beforeRightestPrintsAndComments.endLocation
                , end =
                    operationExpanded.rightestExpression
                        |> Elm.Syntax.Node.range
                        |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRightestExpression : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRightestExpression =
            collapsibleComments commentsBeforeRightestExpression

        leftestPrint : Print
        leftestPrint =
            expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                operationExpanded.leftest

        leftestPrintLineSpread : Print.LineSpread
        leftestPrintLineSpread =
            leftestPrint |> Print.lineSpread

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxOperation.fullRange
                |> Print.lineSpreadMergeWith
                    (\() ->
                        beforeRightestPrintsAndComments.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\c ->
                                    c.maybeCommentsBeforeExpression
                                        |> maybeLineSpread .lineSpread
                                )
                    )
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRightestExpression.lineSpread

        beforeRightestOperatorExpressionChainWithPreviousLineSpread :
            { previousLineSpread : Print.LineSpread
            , lineSpreadIncludingExpressionPrintLineSpreads : Print.LineSpread
            , rightToLeft :
                List
                    { operator : String
                    , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    , expressionPrint : Print
                    , maybeCommentsBeforeExpression : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , previousLineSpread : Print.LineSpread
                    }
            }
        beforeRightestOperatorExpressionChainWithPreviousLineSpread =
            beforeRightestPrintsAndComments.reverse
                |> List.foldr
                    (\operatorExpression soFar ->
                        let
                            expressionPrint : Print
                            expressionPrint =
                                expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                    operatorExpression.expression

                            expressionPrintLineSpread : Print.LineSpread
                            expressionPrintLineSpread =
                                expressionPrint |> Print.lineSpread
                        in
                        { previousLineSpread = expressionPrintLineSpread
                        , lineSpreadIncludingExpressionPrintLineSpreads =
                            soFar.lineSpreadIncludingExpressionPrintLineSpreads
                                |> Print.lineSpreadMergeWith
                                    (\() -> expressionPrintLineSpread)
                        , rightToLeft =
                            { operator = operatorExpression.operator
                            , expression = operatorExpression.expression
                            , expressionPrint = expressionPrint
                            , maybeCommentsBeforeExpression = operatorExpression.maybeCommentsBeforeExpression
                            , previousLineSpread = soFar.previousLineSpread
                            }
                                :: soFar.rightToLeft
                        }
                    )
                    { previousLineSpread = leftestPrintLineSpread
                    , lineSpreadIncludingExpressionPrintLineSpreads =
                        lineSpread
                            |> Print.lineSpreadMergeWithStrict leftestPrintLineSpread
                    , rightToLeft = []
                    }

        rightestOperatorExpressionPrint : Print
        rightestOperatorExpressionPrint =
            case operationExpanded.rightestOperator of
                "<|" ->
                    let
                        expressionPrint : Print
                        expressionPrint =
                            expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments
                                operationExpanded.rightestExpression
                    in
                    Print.spaceOrLinebreakIndented
                        beforeRightestOperatorExpressionChainWithPreviousLineSpread.previousLineSpread
                        |> Print.followedBy printExactlyLessThanVerticalBar
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy
                                        (case commentsBeforeRightestExpression of
                                            [] ->
                                                expressionPrint

                                            _ :: _ ->
                                                commentsCollapsibleBeforeRightestExpression.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            (commentsCollapsibleBeforeRightestExpression.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() -> expressionPrint |> Print.lineSpread)
                                                            )
                                                        )
                                                    |> Print.followedBy expressionPrint
                                        )
                                )
                            )

                nonApLOperator ->
                    let
                        expressionPrint : Print
                        expressionPrint =
                            expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                operationExpanded.rightestExpression
                    in
                    Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented
                            (beforeRightestOperatorExpressionChainWithPreviousLineSpread.lineSpreadIncludingExpressionPrintLineSpreads
                                |> Print.lineSpreadMergeWith
                                    (\() -> expressionPrint |> Print.lineSpread)
                            )
                            |> Print.followedBy (Print.exactly (nonApLOperator ++ " "))
                            |> Print.followedBy
                                (Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                    (case commentsBeforeRightestExpression of
                                        [] ->
                                            expressionPrint

                                        _ :: _ ->
                                            commentsCollapsibleBeforeRightestExpression.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsCollapsibleBeforeRightestExpression.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> expressionPrint |> Print.lineSpread)
                                                        )
                                                    )
                                                |> Print.followedBy expressionPrint
                                    )
                                )
                        )
    in
    leftestPrint
        |> Print.followedBy
            (beforeRightestOperatorExpressionChainWithPreviousLineSpread.rightToLeft
                |> List.foldl
                    (\operatorExpression chainRightPrint ->
                        case operatorExpression.operator of
                            "<|" ->
                                Print.spaceOrLinebreakIndented operatorExpression.previousLineSpread
                                    |> Print.followedBy printExactlyLessThanVerticalBar
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented lineSpread
                                                |> Print.followedBy
                                                    (case operatorExpression.maybeCommentsBeforeExpression of
                                                        Nothing ->
                                                            operatorExpression.expressionPrint

                                                        Just commentsBeforeExpression ->
                                                            commentsBeforeExpression.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBeforeExpression.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> operatorExpression.expressionPrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                                |> Print.followedBy operatorExpression.expressionPrint
                                                    )
                                                |> Print.followedBy chainRightPrint
                                            )
                                        )

                            nonApLOperator ->
                                Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented
                                        beforeRightestOperatorExpressionChainWithPreviousLineSpread.lineSpreadIncludingExpressionPrintLineSpreads
                                        |> Print.followedBy (Print.exactly (nonApLOperator ++ " "))
                                        |> Print.followedBy
                                            (Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                                (case operatorExpression.maybeCommentsBeforeExpression of
                                                    Nothing ->
                                                        operatorExpression.expressionPrint

                                                    Just commentsBeforeExpression ->
                                                        commentsBeforeExpression.print
                                                            |> Print.followedBy
                                                                (Print.spaceOrLinebreakIndented
                                                                    (commentsBeforeExpression.lineSpread
                                                                        |> Print.lineSpreadMergeWith
                                                                            (\() -> operatorExpression.expressionPrint |> Print.lineSpread)
                                                                    )
                                                                )
                                                            |> Print.followedBy
                                                                operatorExpression.expressionPrint
                                                )
                                            )
                                    )
                                    |> Print.followedBy chainRightPrint
                    )
                    rightestOperatorExpressionPrint
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


expressionIsSpaceSeparatedExceptApplication : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Bool
expressionIsSpaceSeparatedExceptApplication expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> Elm.Syntax.Node.value) then
        case expressionNode |> expressionToNotParenthesized of
            Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.Application _) ->
                False

            _ ->
                True

    else
        False


expressionParenthesizedIfSpaceSeparatedExceptApplication :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments expressionNode =
    if expressionNode |> expressionIsSpaceSeparatedExceptApplication then
        expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments expressionNode =
    if expressionNode |> Elm.Syntax.Node.value |> expressionIsSpaceSeparated then
        case expressionNode |> expressionToNotParenthesized |> Elm.Syntax.Node.value of
            Elm.Syntax.Expression.Application _ ->
                expressionNotParenthesized syntaxComments expressionNode

            Elm.Syntax.Expression.LambdaExpression _ ->
                expressionNotParenthesized syntaxComments expressionNode

            _ ->
                expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


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
            printExactlySquareOpening
                |> Print.followedBy
                    (case commentsInRange syntaxList.fullRange syntaxComments of
                        [] ->
                            printExactlySquareClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlySquareClosing
                    )

        element0 :: element1Up ->
            let
                elementPrintsWithCommentsBefore :
                    { endLocation : Elm.Syntax.Range.Location
                    , reverse : List Print
                    }
                elementPrintsWithCommentsBefore =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\elementNode soFar ->
                                let
                                    (Elm.Syntax.Node.Node elementRange _) =
                                        elementNode

                                    print : Print
                                    print =
                                        expressionNotParenthesized syntaxComments
                                            elementNode
                                in
                                { endLocation = elementRange.end
                                , reverse =
                                    (case
                                        commentsInRange { start = soFar.endLocation, end = elementRange.start }
                                            syntaxComments
                                     of
                                        [] ->
                                            print

                                        comment0 :: comment1Up ->
                                            let
                                                commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                                commentsBefore =
                                                    collapsibleComments (comment0 :: comment1Up)
                                            in
                                            commentsBefore.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsBefore.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> print |> Print.lineSpread)
                                                        )
                                                    )
                                                |> Print.followedBy print
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxList.fullRange.start
                            , reverse = []
                            }

                commentsAfterElements : List String
                commentsAfterElements =
                    commentsInRange { start = elementPrintsWithCommentsBefore.endLocation, end = syntaxList.fullRange.end } syntaxComments

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxList.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                elementPrintsWithCommentsBefore.reverse
                                    |> Print.lineSpreadListMapAndCombine Print.lineSpread
                            )
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                case commentsAfterElements of
                                    [] ->
                                        Print.SingleLine

                                    _ :: _ ->
                                        Print.MultipleLines
                            )
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    (elementPrintsWithCommentsBefore.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\elementPrintWithCommentsBefore ->
                                Print.withIndentIncreasedBy 2
                                    elementPrintWithCommentsBefore
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterElements of
                        [] ->
                            Print.spaceOrLinebreakIndented lineSpread

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                |> Print.followedBy (comments (comment0 :: comment1Up))
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlySquareClosing


expressionRecordUpdate :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , recordVariable : Elm.Syntax.Node.Node String
        , fields :
            List
                (Elm.Syntax.Node.Node
                    ( Elm.Syntax.Node.Node String
                    , Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    )
                )
        }
    -> Print
expressionRecordUpdate syntaxComments syntaxRecordUpdate =
    let
        fieldPrintsWithCommentsBefore :
            { endLocation : Elm.Syntax.Range.Location
            , reverse : List Print
            }
        fieldPrintsWithCommentsBefore =
            syntaxRecordUpdate.fields
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ fieldSyntax) soFar ->
                        let
                            ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode ) =
                                fieldSyntax

                            valuePrint : Print
                            valuePrint =
                                expressionNotParenthesized syntaxComments fieldValueNode

                            (Elm.Syntax.Node.Node fieldValueRange _) =
                                fieldValueNode
                        in
                        { endLocation = fieldValueRange.end
                        , reverse =
                            (Print.withIndentIncreasedBy 2
                                (case
                                    commentsInRange
                                        { start = soFar.endLocation, end = fieldNameRange.start }
                                        syntaxComments
                                 of
                                    [] ->
                                        Print.exactly (fieldName ++ " =")

                                    comment0 :: comment1Up ->
                                        let
                                            commentsBeforeName : { print : Print, lineSpread : Print.LineSpread }
                                            commentsBeforeName =
                                                collapsibleComments (comment0 :: comment1Up)
                                        in
                                        commentsBeforeName.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented commentsBeforeName.lineSpread)
                                            |> Print.followedBy (Print.exactly (fieldName ++ " ="))
                                )
                                |> Print.followedBy
                                    (Print.withIndentAtNextMultipleOf4
                                        ((case
                                            commentsInRange
                                                { start = fieldNameRange.start, end = fieldValueRange.start }
                                                syntaxComments
                                          of
                                            [] ->
                                                Print.spaceOrLinebreakIndented
                                                    (lineSpreadBetweenRanges
                                                        fieldNameRange
                                                        (fieldValueNode |> Elm.Syntax.Node.range)
                                                        |> Print.lineSpreadMergeWith (\() -> valuePrint |> Print.lineSpread)
                                                    )

                                            comment0 :: comment1Up ->
                                                let
                                                    commentsBeforeValue : { print : Print, lineSpread : Print.LineSpread }
                                                    commentsBeforeValue =
                                                        collapsibleComments (comment0 :: comment1Up)

                                                    layout : Print.Print
                                                    layout =
                                                        Print.spaceOrLinebreakIndented
                                                            (commentsBeforeValue.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() ->
                                                                        lineSpreadBetweenRanges
                                                                            fieldNameRange
                                                                            (fieldValueNode |> Elm.Syntax.Node.range)
                                                                    )
                                                                |> Print.lineSpreadMergeWith (\() -> valuePrint |> Print.lineSpread)
                                                            )
                                                in
                                                layout
                                                    |> Print.followedBy commentsBeforeValue.print
                                                    |> Print.followedBy layout
                                         )
                                            |> Print.followedBy valuePrint
                                        )
                                    )
                            )
                                :: soFar.reverse
                        }
                    )
                    { endLocation =
                        syntaxRecordUpdate.recordVariable
                            |> Elm.Syntax.Node.range
                            |> .end
                    , reverse = []
                    }

        commentsAfterFields : List String
        commentsAfterFields =
            commentsInRange
                { start = fieldPrintsWithCommentsBefore.endLocation
                , end = syntaxRecordUpdate.fullRange.end
                }
                syntaxComments

        maybeCommentsBeforeRecordVariable : Maybe { print : Print, lineSpread : Print.LineSpread }
        maybeCommentsBeforeRecordVariable =
            case
                commentsInRange
                    { start = syntaxRecordUpdate.fullRange.start
                    , end = syntaxRecordUpdate.recordVariable |> Elm.Syntax.Node.range |> .start
                    }
                    syntaxComments
            of
                [] ->
                    Nothing

                comment0 :: comment1Up ->
                    Just (collapsibleComments (comment0 :: comment1Up))

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxRecordUpdate.fullRange
                |> Print.lineSpreadMergeWith
                    (\() -> maybeCommentsBeforeRecordVariable |> maybeLineSpread .lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case commentsAfterFields of
                            [] ->
                                Print.SingleLine

                            _ :: _ ->
                                Print.MultipleLines
                    )
                |> Print.lineSpreadMergeWith
                    (\() ->
                        fieldPrintsWithCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )

        recordVariablePrint : Print
        recordVariablePrint =
            Print.exactly
                (syntaxRecordUpdate.recordVariable |> Elm.Syntax.Node.value)
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case maybeCommentsBeforeRecordVariable of
                    Nothing ->
                        recordVariablePrint

                    Just commentsCollapsibleBeforeRecordVariable ->
                        commentsCollapsibleBeforeRecordVariable.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented commentsCollapsibleBeforeRecordVariable.lineSpread)
                            |> Print.followedBy recordVariablePrint
                )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy printExactlyVerticalBarSpace
                    |> Print.followedBy
                        (fieldPrintsWithCommentsBefore.reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                (Print.emptyOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyCommaSpace
                                )
                        )
                    |> -- yes, elm-format indents trailing comments
                       Print.followedBy
                        (case commentsAfterFields of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreak
                                    |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCurlyClosing


expressionLambda :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Lambda
    -> Print
expressionLambda syntaxComments (Elm.Syntax.Node.Node fullRange syntaxLambda) =
    let
        parameterPrintsWithCommentsBefore :
            { endLocation : Elm.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrintsWithCommentsBefore =
            syntaxLambda.args
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : Elm.Syntax.Range.Range
                            parameterRange =
                                parameterPattern |> Elm.Syntax.Node.range

                            print : Print
                            print =
                                patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = parameterRange.end
                        }
                    )
                    { reverse = []
                    , endLocation = fullRange.start
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = parameterPrintsWithCommentsBefore.endLocation
                , end = syntaxLambda.expression |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBefore.reverse
                |> Print.lineSpreadListMapAndCombine Print.lineSpread

        resultPrint : Print
        resultPrint =
            expressionNotParenthesized syntaxComments
                syntaxLambda.expression
    in
    printExactlyBackSlash
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                (Print.emptyOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (parameterPrintsWithCommentsBefore.reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                (Print.spaceOrLinebreakIndented parametersLineSpread)
                        )
                )
                |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                |> Print.followedBy printExactlyMinusGreaterThan
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        ((case commentsBeforeResult of
                            [] ->
                                Print.spaceOrLinebreakIndented
                                    (parametersLineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                lineSpreadInRange fullRange
                                            )
                                        |> Print.lineSpreadMergeWith
                                            (\() -> resultPrint |> Print.lineSpread)
                                    )

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy Print.linebreakIndented
                         )
                            |> Print.followedBy
                                resultPrint
                        )
                    )
            )


expressionIfThenElse :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , condition : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , conditionLineSpreadMinimum : Print.LineSpread
        , onTrue : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , onFalse : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Print
expressionIfThenElse syntaxComments syntaxIfThenElse =
    -- IGNORE TCO
    let
        commentsBeforeCondition : List String
        commentsBeforeCondition =
            commentsInRange
                { start = syntaxIfThenElse.fullRange.start
                , end = syntaxIfThenElse.condition |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsBeforeOnTrue : List String
        commentsBeforeOnTrue =
            commentsInRange
                { start = syntaxIfThenElse.condition |> Elm.Syntax.Node.range |> .end
                , end = syntaxIfThenElse.onTrue |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        onFalseNotParenthesized : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        onFalseNotParenthesized =
            syntaxIfThenElse.onFalse |> expressionToNotParenthesized

        commentsBeforeOnFalseNotParenthesizedInParens : List String
        commentsBeforeOnFalseNotParenthesizedInParens =
            commentsInRange
                { start = syntaxIfThenElse.onFalse |> Elm.Syntax.Node.range |> .start
                , end = onFalseNotParenthesized |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsBeforeOnFalse : List String
        commentsBeforeOnFalse =
            commentsInRange
                { start = syntaxIfThenElse.onTrue |> Elm.Syntax.Node.range |> .end
                , end = syntaxIfThenElse.onFalse |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        conditionPrint : Print
        conditionPrint =
            expressionNotParenthesized syntaxComments syntaxIfThenElse.condition

        conditionLineSpread : Print.LineSpread
        conditionLineSpread =
            syntaxIfThenElse.conditionLineSpreadMinimum
                |> Print.lineSpreadMergeWith
                    (\() -> Print.lineSpread conditionPrint)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case commentsBeforeCondition of
                            _ :: _ ->
                                Print.MultipleLines

                            [] ->
                                Print.SingleLine
                    )

        onTruePrint : Print
        onTruePrint =
            expressionNotParenthesized syntaxComments syntaxIfThenElse.onTrue
    in
    printExactlyIf
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented conditionLineSpread
                    |> Print.followedBy
                        (case commentsBeforeCondition of
                            [] ->
                                conditionPrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy conditionPrint
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented conditionLineSpread)
        |> Print.followedBy printExactlyThen
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (case commentsBeforeOnTrue of
                            [] ->
                                onTruePrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy onTruePrint
                        )
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyElse
        |> Print.followedBy
            (case ( commentsBeforeOnFalseNotParenthesizedInParens, onFalseNotParenthesized ) of
                ( [], Elm.Syntax.Node.Node onFalseNotParenthesizedRange (Elm.Syntax.Expression.IfBlock onFalseCondition onFalseOnTrue onFalseOnFalse) ) ->
                    case commentsBeforeOnFalse of
                        [] ->
                            printExactlySpace
                                |> Print.followedBy
                                    (expressionIfThenElse syntaxComments
                                        { fullRange = onFalseNotParenthesizedRange
                                        , condition = onFalseCondition
                                        , conditionLineSpreadMinimum = Print.SingleLine
                                        , onTrue = onFalseOnTrue
                                        , onFalse = onFalseOnFalse
                                        }
                                    )

                        comment0 :: comment1Up ->
                            Print.linebreakIndented
                                |> Print.followedBy
                                    (comments (comment0 :: comment1Up))
                                |> Print.followedBy Print.linebreakIndented
                                |> Print.followedBy
                                    (expressionIfThenElse syntaxComments
                                        { fullRange = onFalseNotParenthesizedRange
                                        , conditionLineSpreadMinimum =
                                            -- don't ask me why
                                            Print.MultipleLines
                                        , condition = onFalseCondition
                                        , onTrue = onFalseOnTrue
                                        , onFalse = onFalseOnFalse
                                        }
                                    )

                _ ->
                    let
                        onFalsePrint : Print
                        onFalsePrint =
                            expressionNotParenthesized syntaxComments syntaxIfThenElse.onFalse
                    in
                    Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsBeforeOnFalse of
                                    [] ->
                                        onFalsePrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy onFalsePrint
                                )
                        )
            )


expressionCaseOf :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , cases : Elm.Syntax.Expression.Cases
        }
    -> Print
expressionCaseOf syntaxComments syntaxCaseOf =
    let
        commentsBeforeCasedExpression : List String
        commentsBeforeCasedExpression =
            commentsInRange
                { start = syntaxCaseOf.fullRange.start
                , end = syntaxCaseOf.expression |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        casedExpressionLineSpread : Print.LineSpread
        casedExpressionLineSpread =
            case commentsBeforeCasedExpression of
                _ :: _ ->
                    Print.MultipleLines

                [] ->
                    lineSpreadInNode syntaxCaseOf.expression

        casedExpressionPrint : Print
        casedExpressionPrint =
            expressionNotParenthesized syntaxComments
                syntaxCaseOf.expression
    in
    printExactlyCase
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented casedExpressionLineSpread
                    |> Print.followedBy
                        (case commentsBeforeCasedExpression of
                            [] ->
                                casedExpressionPrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy casedExpressionPrint
                        )
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented casedExpressionLineSpread)
        |> Print.followedBy printExactlyOf
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (syntaxCaseOf.cases
                            |> List.foldl
                                (\( casePattern, caseResult ) soFar ->
                                    let
                                        commentsBeforeCasePattern : List String
                                        commentsBeforeCasePattern =
                                            commentsInRange
                                                { start = soFar.endLocation
                                                , end = casePattern |> Elm.Syntax.Node.range |> .start
                                                }
                                                syntaxComments

                                        casePrint : Print
                                        casePrint =
                                            case_ syntaxComments ( casePattern, caseResult )

                                        commentsAndCasePrint : Print
                                        commentsAndCasePrint =
                                            case commentsBeforeCasePattern of
                                                [] ->
                                                    casePrint

                                                comment0 :: comment1Up ->
                                                    comments (comment0 :: comment1Up)
                                                        |> Print.followedBy Print.linebreakIndented
                                                        |> Print.followedBy casePrint
                                    in
                                    { endLocation = caseResult |> Elm.Syntax.Node.range |> .end
                                    , reverse = commentsAndCasePrint :: soFar.reverse
                                    }
                                )
                                { endLocation = syntaxCaseOf.expression |> Elm.Syntax.Node.range |> .end
                                , reverse = []
                                }
                            |> .reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                printLinebreakLinebreakIndented
                        )
                )
            )


expressionLetIn :
    List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , letDeclaration0 : Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
        , letDeclaration1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration)
        , result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Print
expressionLetIn syntaxComments syntaxLetIn =
    let
        letDeclarationPrints : { endLocation : Elm.Syntax.Range.Location, reverse : List Print }
        letDeclarationPrints =
            (syntaxLetIn.letDeclaration0 :: syntaxLetIn.letDeclaration1Up)
                |> List.foldl
                    (\(Elm.Syntax.Node.Node letDeclarationRange letDeclaration) soFar ->
                        let
                            commentsBefore : List String
                            commentsBefore =
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = letDeclarationRange.start
                                    }
                                    syntaxComments

                            letDeclarationPrint : Print
                            letDeclarationPrint =
                                expressionLetDeclaration syntaxComments letDeclaration

                            letDeclarationWithCommentsBeforePrint : Print
                            letDeclarationWithCommentsBeforePrint =
                                case commentsBefore of
                                    [] ->
                                        letDeclarationPrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy letDeclarationPrint
                        in
                        { endLocation = letDeclarationRange.end
                        , reverse =
                            letDeclarationWithCommentsBeforePrint :: soFar.reverse
                        }
                    )
                    { endLocation = syntaxLetIn.fullRange.start
                    , reverse = []
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = letDeclarationPrints.endLocation
                , end =
                    syntaxLetIn.result
                        |> Elm.Syntax.Node.range
                        |> .start
                }
                syntaxComments

        letInResultPrint : Print
        letInResultPrint =
            expressionNotParenthesized syntaxComments syntaxLetIn.result
    in
    printExactlyLet
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (letDeclarationPrints.reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                printLinebreakLinebreakIndented
                        )
                )
            )
        |> Print.followedBy printLinebreakIndentedInLinebreakIndented
        |> Print.followedBy
            (case commentsBeforeResult of
                [] ->
                    letInResultPrint

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy Print.linebreakIndented
                        |> Print.followedBy letInResultPrint
            )


expressionLetDeclaration :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.LetDeclaration
    -> Print
expressionLetDeclaration syntaxComments letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction letDeclarationExpression ->
            let
                implementationPrint : Print
                implementationPrint =
                    declarationExpressionImplementation syntaxComments
                        (letDeclarationExpression.declaration |> Elm.Syntax.Node.value)
            in
            case letDeclarationExpression.signature of
                Nothing ->
                    implementationPrint

                Just (Elm.Syntax.Node.Node signatureRange signature) ->
                    let
                        commentsBetweenSignatureAndImplementationName : List String
                        commentsBetweenSignatureAndImplementationName =
                            commentsInRange
                                { start = signatureRange.end
                                , end =
                                    letDeclarationExpression.declaration
                                        |> Elm.Syntax.Node.range
                                        |> .start
                                }
                                syntaxComments
                    in
                    declarationSignature syntaxComments signature
                        |> Print.followedBy
                            (case commentsBetweenSignatureAndImplementationName of
                                [] ->
                                    Print.linebreakIndented

                                comment0 :: comment1Up ->
                                    Print.linebreakIndented
                                        |> Print.followedBy
                                            (comments (comment0 :: comment1Up))
                                        |> Print.followedBy Print.linebreakIndented
                            )
                        |> Print.followedBy implementationPrint

        Elm.Syntax.Expression.LetDestructuring destructuringPattern destructuredExpression ->
            let
                commentsBeforeDestructuredExpression : List String
                commentsBeforeDestructuredExpression =
                    commentsInRange
                        { start = destructuringPattern |> Elm.Syntax.Node.range |> .end
                        , end = destructuredExpression |> Elm.Syntax.Node.range |> .start
                        }
                        syntaxComments

                destructuringPatternPrint : Print
                destructuringPatternPrint =
                    patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern

                destructuredExpressionPrint : Print
                destructuredExpressionPrint =
                    expressionNotParenthesized syntaxComments
                        destructuredExpression
            in
            destructuringPatternPrint
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented
                            (destructuringPatternPrint |> Print.lineSpread)
                            |> Print.followedBy printEqualsLinebreakIndented
                            |> Print.followedBy
                                (case commentsBeforeDestructuredExpression of
                                    [] ->
                                        destructuredExpressionPrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy
                                                Print.linebreakIndented
                                            |> Print.followedBy destructuredExpressionPrint
                                )
                        )
                    )


expressionToNotParenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
expressionToNotParenthesized (Elm.Syntax.Node.Node fullRange syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expressionToNotParenthesized

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    inParens |> expressionToNotParenthesized

                [] ->
                    Elm.Syntax.Node.Node fullRange Elm.Syntax.Expression.UnitExpr

                [ part0, part1 ] ->
                    Elm.Syntax.Node.Node fullRange (Elm.Syntax.Expression.TupledExpression [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Elm.Syntax.Node.Node fullRange (Elm.Syntax.Expression.TupledExpression [ part0, part1, part2 ])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Elm.Syntax.Node.Node fullRange (Elm.Syntax.Expression.TupledExpression (part0 :: part1 :: part2 :: part3 :: part4Up))

        syntaxExpressionNotParenthesized ->
            Elm.Syntax.Node.Node fullRange syntaxExpressionNotParenthesized


{-| Print a single [`Elm.Syntax.Expression.Case`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Case)
-}
case_ :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.Case
    -> Print
case_ syntaxComments ( casePattern, caseResult ) =
    let
        patternPrint : Print
        patternPrint =
            patternNotParenthesized syntaxComments casePattern

        commentsBeforeExpression : List String
        commentsBeforeExpression =
            commentsInRange
                { start = casePattern |> Elm.Syntax.Node.range |> .end
                , end = caseResult |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        caseResultPrint : Print
        caseResultPrint =
            expressionNotParenthesized syntaxComments caseResult
    in
    patternPrint
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented (patternPrint |> Print.lineSpread))
        |> Print.followedBy printExactlyMinusGreaterThan
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (case commentsBeforeExpression of
                            [] ->
                                caseResultPrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy caseResultPrint
                        )
                )
            )


printLinebreakIndentedCommaSpace : Print.Print
printLinebreakIndentedCommaSpace =
    Print.linebreakIndented
        |> Print.followedBy printExactlyCommaSpace


printLinebreakIndentedAs : Print.Print
printLinebreakIndentedAs =
    Print.linebreakIndented
        |> Print.followedBy printExactlyAs


printLinebreakIndentedExposing : Print.Print
printLinebreakIndentedExposing =
    Print.linebreakIndented
        |> Print.followedBy printExactlyExposing


printLinebreakIndentedVerticalBarSpace : Print.Print
printLinebreakIndentedVerticalBarSpace =
    Print.linebreakIndented
        |> Print.followedBy printExactlyVerticalBarSpace


printLinebreakLinebreak : Print.Print
printLinebreakLinebreak =
    Print.linebreak
        |> Print.followedBy Print.linebreak


printLinebreakLinebreakIndented : Print.Print
printLinebreakLinebreakIndented =
    Print.linebreak
        |> Print.followedBy Print.linebreakIndented


printLinebreakLinebreakLinebreak : Print.Print
printLinebreakLinebreakLinebreak =
    Print.linebreak
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreak


printLinebreakIndentedInLinebreakIndented : Print.Print
printLinebreakIndentedInLinebreakIndented =
    Print.linebreakIndented
        |> Print.followedBy printExactlyIn
        |> Print.followedBy Print.linebreakIndented


printEqualsLinebreakIndented : Print.Print
printEqualsLinebreakIndented =
    printExactlyEquals
        |> Print.followedBy Print.linebreakIndented


printExactlyParensOpeningParensClosed : Print.Print
printExactlyParensOpeningParensClosed =
    Print.exactly "()"


printExactlySpaceSpace : Print.Print
printExactlySpaceSpace =
    Print.exactly "  "


printExactlySpace : Print.Print
printExactlySpace =
    Print.exactly " "


printExactlyEqualsSpace : Print.Print
printExactlyEqualsSpace =
    Print.exactly "= "


printExactlyCommaSpace : Print.Print
printExactlyCommaSpace =
    Print.exactly ", "


printExactlyColonColonSpace : Print.Print
printExactlyColonColonSpace =
    Print.exactly ":: "


printExactlyPortSpace : Print.Print
printExactlyPortSpace =
    Print.exactly "port "


printExactlyTypeSpaceAlias : Print.Print
printExactlyTypeSpaceAlias =
    Print.exactly "type alias"


printExactlySquareOpeningSpace : Print.Print
printExactlySquareOpeningSpace =
    Print.exactly "[ "


printExactlyParensOpeningSpace : Print.Print
printExactlyParensOpeningSpace =
    Print.exactly "( "


printExactlyCurlyOpeningSpace : Print.Print
printExactlyCurlyOpeningSpace =
    Print.exactly "{ "


printExactlyVerticalBarSpace : Print.Print
printExactlyVerticalBarSpace =
    Print.exactly "| "


printExactlyCurlyOpeningDotDotCurlyClosing : Print.Print
printExactlyCurlyOpeningDotDotCurlyClosing =
    Print.exactly "{--}"


printExactlyParensOpening : Print
printExactlyParensOpening =
    Print.exactly "("


printExactlyParensClosing : Print
printExactlyParensClosing =
    Print.exactly ")"


printExactlySquareOpening : Print
printExactlySquareOpening =
    Print.exactly "["


printExactlySquareClosing : Print
printExactlySquareClosing =
    Print.exactly "]"


printExactlyCurlyOpening : Print
printExactlyCurlyOpening =
    Print.exactly "{"


printExactlyCurlyClosing : Print
printExactlyCurlyClosing =
    Print.exactly "}"


printExactlyDoubleQuoteDoubleQuoteDoubleQuote : Print
printExactlyDoubleQuoteDoubleQuoteDoubleQuote =
    Print.exactly "\"\"\""


printExactlyMinusGreaterThan : Print
printExactlyMinusGreaterThan =
    Print.exactly "->"


printExactlyEquals : Print
printExactlyEquals =
    Print.exactly "="


printExactlyParensOpeningDotDotParensClosing : Print
printExactlyParensOpeningDotDotParensClosing =
    Print.exactly "(..)"


printExactlyBackSlash : Print
printExactlyBackSlash =
    Print.exactly "\\"


printExactlyLessThanVerticalBar : Print
printExactlyLessThanVerticalBar =
    Print.exactly "<|"


printExactlyMinus : Print
printExactlyMinus =
    Print.exactly "-"


printExactlyZero : Print
printExactlyZero =
    Print.exactly "0"


printExactlyZeroXZeroZero : Print
printExactlyZeroXZeroZero =
    Print.exactly "0x00"


printExactlyCurlyOpeningMinus : Print
printExactlyCurlyOpeningMinus =
    Print.exactly "{-"


printExactlyMinusCurlyClosing : Print
printExactlyMinusCurlyClosing =
    Print.exactly "-}"


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


printExactlyType : Print
printExactlyType =
    Print.exactly "type"


printExactlyAs : Print
printExactlyAs =
    Print.exactly "as"


printExactlyExposing : Print
printExactlyExposing =
    Print.exactly "exposing"


printExactlyLet : Print
printExactlyLet =
    Print.exactly "let"


printExactlyIn : Print
printExactlyIn =
    Print.exactly "in"


printExactlyCase : Print
printExactlyCase =
    Print.exactly "case"


printExactlyOf : Print
printExactlyOf =
    Print.exactly "of"


printExactlyIf : Print
printExactlyIf =
    Print.exactly "if"


printExactlyThen : Print
printExactlyThen =
    Print.exactly "then"


printExactlyElse : Print
printExactlyElse =
    Print.exactly "else"


printExactImport : Print
printExactImport =
    Print.exactly "import"
