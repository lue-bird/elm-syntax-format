module ElmSyntaxPrintDefunctionalizedShortcutEscapeMorePredefineMicroOptimized exposing
    ( toString, Print
    , module_
    , moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
    , moduleName, qualifiedReference
    )

{-| Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

@docs toString, Print
@docs module_

That's all you'll need most of the time.

Sometimes it's useful to print only some part of the syntax,
to, say, display only an expression in an article
or reformat only the touched declarations on save.

@docs moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
@docs moduleName, qualifiedReference

If you need other syntax printing like for collapsible comments to be exposed,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

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
import PrintWithDefunctionalizedAppendExact as Print
import Unicode



-- TODO -NotParenthesized re-use node argument
-- TODO use reversing combinators for parameterPrints etc instead of List.reverse


{-| Pretty printable intermediate representation.
See [`toString`](#toString)
-}
type alias Print =
    Print.Print


{-| All other helpers in this module produce a [`Print`](#Print)
which you can in the end convert to a String with [`toString`](#toString)
-}
toString : Print -> String
toString print =
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
                        commentsAndPortDocumentationComments.comments
    in
    syntaxModule.moduleDefinition
        |> Elm.Syntax.Node.value
        |> moduleHeader { atDocsLines = atDocsLines, comments = commentsAndPortDocumentationComments.comments }
        |> Print.followedBy
            (case maybeModuleDocumentation of
                Nothing ->
                    Print.empty

                Just (Elm.Syntax.Node.Node _ moduleDocumentationAsString) ->
                    printLinebreakLinebreak
                        |> Print.followedBy (Print.exactly moduleDocumentationAsString)
            )
        |> Print.followedBy
            (case syntaxModule.imports of
                [] ->
                    case maybeModuleDocumentation of
                        Nothing ->
                            Print.linebreak

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
                            commentsAndPortDocumentationComments.comments
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
                                |> imports commentsAndPortDocumentationComments.comments
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
                            (listFilledLast declaration0 declaration1Up
                                |> Elm.Syntax.Node.range
                                |> .end
                            )
                            commentsAndPortDocumentationComments.comments
                    of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            printLinebreakLinebreakLinebreak
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
            commentsEmptyPortDocumentationCommentsEmpty


commentsEmptyPortDocumentationCommentsEmpty : { comments : List a, portDocumentationComments : List b }
commentsEmptyPortDocumentationCommentsEmpty =
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
                    rawSinceAtDocsEmptyFinishedBlocksEmpty
    in
    { whileAtDocsLines = parsed.finishedBlocks |> List.reverse
    , rawAfterAtDocsLines = parsed.rawSinceAtDocs
    }


rawSinceAtDocsEmptyFinishedBlocksEmpty : { rawSinceAtDocs : String, finishedBlocks : List a }
rawSinceAtDocsEmptyFinishedBlocksEmpty =
    { rawSinceAtDocs = "", finishedBlocks = [] }


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
            (case listFilledLast syntaxComments.comment0 syntaxComments.comment1Up of
                "{--}" ->
                    -- don't ask me why elm-format formats it that way
                    Print.empty

                _ ->
                    printLinebreakLinebreak
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
                |> Print.listMapToExactAndIntersperseAndFlatten
                    (\(Elm.Syntax.Node.Node _ syntaxExpose) ->
                        expose syntaxExpose
                    )
                    (Print.emptyOrLinebreakIndented lineSpread
                        |> Print.followedBy printExactlyCommaSpace
                    )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy
            (case containedComments of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
            )
        |> Print.followedBy printExactlyParensClosing


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

        expose0Node :: (((Elm.Syntax.Node.Node _ expose1) :: expose2Up) as expose1Up) ->
            let
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
                    expose0Node :: exposesCombine expose1Up

                GT ->
                    expose0Node :: exposesCombine expose1Up


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
                                    Print.empty

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                            )
                        |> Print.followedBy printExactlyParensClosing

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
                                                |> Print.listMapToExactAndIntersperseAndFlatten
                                                    (\atDocsLine ->
                                                        atDocsLine
                                                            |> listMapAndIntersperseAndFlattenToString
                                                                expose
                                                                ", "
                                                    )
                                                    printLinebreakIndentedCommaSpace
                                            )
                                        |> Print.followedBy Print.linebreakIndented
                                        |> Print.followedBy
                                            (case atDocsExposeLines.remainingExposes of
                                                [] ->
                                                    Print.empty

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
                                            )
                                        |> Print.followedBy printExactlyParensClosing

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
                        |> Print.listIntersperseExactAndFlatten " "
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
                            |> -- {-
                               String.dropLeft 2
                            |> -- -}
                               String.dropRight 2
                            |> String.lines

                    commentContentNormal : List String
                    commentContentNormal =
                        case commentContentLines of
                            [] ->
                                []

                            commentContentLine0 :: commentContentLine1Up ->
                                (commentContentLine0 |> String.trimLeft)
                                    :: (commentContentLine1Up
                                            |> listDropLastIfIs
                                                (\line ->
                                                    case line |> String.trim of
                                                        "" ->
                                                            True

                                                        _ ->
                                                            False
                                                )
                                            |> unindent
                                       )
                                    |> List.map String.trimRight
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


unindent : List String -> List String
unindent lines =
    let
        nonBlankLines : List String
        nonBlankLines =
            lines
                |> List.filterMap
                    (\line ->
                        case line |> String.trim of
                            "" ->
                                Nothing

                            _ ->
                                Just line
                    )
    in
    case nonBlankLines |> List.map lineIndentation |> List.minimum of
        Nothing ->
            lines

        Just minimumIndentation ->
            lines
                |> List.map (\line -> line |> String.dropLeft minimumIndentation)


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
        stringContentEscaped : String
        stringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ quotedStringCharToEscaped contentChar ++ ""
                    )
                    ""

        wasProbablyTripleDoubleQuoteOriginally : Bool
        wasProbablyTripleDoubleQuoteOriginally =
            (range.start.row /= range.end.row)
                || ((range.end.column - range.start.column)
                        - (stringContentEscaped |> String.length)
                        /= 2
                   )
    in
    if wasProbablyTripleDoubleQuoteOriginally then
        printExactlyDoubleQuoteDoubleQuoteDoubleQuote
            |> Print.followedBy
                (stringContentEscaped
                    |> String.replace "\\n" "\n"
                    |> String.replace "\\r" "\u{000D}"
                    |> String.replace "\\u{000D}" "\u{000D}"
                    |> stringAlterAfterFirstBeforeLastChar
                        (String.replace "\\\"" "\"")
                    |> String.lines
                    |> Print.listMapToExactAndIntersperseAndFlatten
                        identity
                        Print.linebreak
                )
            |> Print.followedBy printExactlyDoubleQuoteDoubleQuoteDoubleQuote

    else
        Print.exactly ("\"" ++ stringContentEscaped ++ "\"")


stringAlterAfterFirstBeforeLastChar : (String -> String) -> (String -> String)
stringAlterAfterFirstBeforeLastChar alterAfterFirstBeforeLastChar string =
    (string |> String.slice 0 1)
        ++ (string
                |> String.slice 1 ((string |> String.length) - 1)
                |> alterAfterFirstBeforeLastChar
           )
        ++ (string |> String.slice ((string |> String.length) - 1) (string |> String.length))
        ++ ""


quotedStringCharToEscaped : Char -> String
quotedStringCharToEscaped character =
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
    String.toUpper (intToHexString (Char.toCode character))


characterIsPrint : Char -> Bool
characterIsPrint character =
    if
        -- Unicode.getCategory is extremely expensive so we shortcut if at all possible
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


charLiteral : Char -> String
charLiteral charContent =
    "\""
        ++ quotedCharToEscaped charContent
        ++ "\""


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
            if characterIsPrint otherCharacter then
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


stringResizePadLeftWith0s : Int -> (String -> String)
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
                        |> tuple patternNotParenthesized syntaxComments

                [ part0, part1, part2 ] ->
                    { part0 = part0, part1 = part1, part2 = part2, fullRange = fullRange }
                        |> triple patternNotParenthesized syntaxComments

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
                            Print.empty

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
                    )
                |> Print.followedBy printExactlySquareClosing

        element0 :: element1Up ->
            let
                commentsBeforeElements :
                    { end : Elm.Syntax.Range.Location
                    , reverse : List (Maybe { lineSpread : Print.LineSpread, print : Print })
                    }
                commentsBeforeElements =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node elementRange _) soFar ->
                                let
                                    commentsBeforeElement : List String
                                    commentsBeforeElement =
                                        commentsInRange { start = soFar.end, end = elementRange.start }
                                            syntaxComments
                                in
                                { end = elementRange.end
                                , reverse =
                                    (case commentsBeforeElement of
                                        [] ->
                                            Nothing

                                        comment0 :: comment1Up ->
                                            Just (collapsibleComments (comment0 :: comment1Up))
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { end = syntaxList.fullRange.start
                            , reverse = []
                            }

                commentsAfterElements : List String
                commentsAfterElements =
                    commentsInRange { start = commentsBeforeElements.end, end = syntaxList.fullRange.end } syntaxComments

                elementPrints : List Print
                elementPrints =
                    (element0 :: element1Up)
                        |> List.map
                            (\element ->
                                patternNotParenthesized syntaxComments
                                    element
                            )

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxList.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                elementPrints
                                    |> Print.lineSpreadListMapAndCombine Print.lineSpread
                            )
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                commentsBeforeElements.reverse
                                    |> Print.lineSpreadListMapAndCombine
                                        (\c -> c |> maybeLineSpread .lineSpread)
                            )
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    (List.map2
                        (\elementPrint maybeCommentsBeforeElement ->
                            Print.withIndentIncreasedBy 2
                                ((case maybeCommentsBeforeElement of
                                    Nothing ->
                                        Print.empty

                                    Just commentsBeforeElement ->
                                        commentsBeforeElement.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsBeforeElement.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> elementPrint |> Print.lineSpread)
                                                    )
                                                )
                                 )
                                    |> Print.followedBy elementPrint
                                )
                        )
                        elementPrints
                        (commentsBeforeElements.reverse |> List.reverse)
                        |> Print.listIntersperseAndFlatten
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterElements of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            Print.withIndentIncreasedBy 2
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                )
                    )
                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
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

        tailPatternPrints : List Print
        tailPatternPrints =
            tailPatterns
                |> List.map
                    (\tailPattern ->
                        patternParenthesizedIfSpaceSeparated syntaxComments
                            tailPattern
                    )

        tailPatternsCommentsBefore : List (Maybe { print : Print, lineSpread : Print.LineSpread })
        tailPatternsCommentsBefore =
            tailPatterns
                |> List.foldl
                    (\(Elm.Syntax.Node.Node tailPatternRange _) soFar ->
                        let
                            commentsBeforeTailPattern : List String
                            commentsBeforeTailPattern =
                                commentsInRange { start = soFar.end, end = tailPatternRange.start }
                                    syntaxComments
                        in
                        { reverse =
                            (case commentsBeforeTailPattern of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.reverse
                        , end = tailPatternRange.end
                        }
                    )
                    { reverse = []
                    , end = syntaxCons.head |> Elm.Syntax.Node.range |> .end
                    }
                |> .reverse
                |> List.reverse

        lineSpread : Print.LineSpread
        lineSpread =
            headPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> tailPatternPrints |> Print.lineSpreadListMapAndCombine Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        tailPatternsCommentsBefore
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )
    in
    headPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy
                        (List.map2
                            (\tailPatternPrint maybeCommentsBefore ->
                                printExactlyColonColonSpace
                                    |> Print.followedBy
                                        (Print.withIndentIncreasedBy 3
                                            ((case maybeCommentsBefore of
                                                Nothing ->
                                                    Print.empty

                                                Just commentsBefore ->
                                                    commentsBefore.print
                                                        |> Print.followedBy
                                                            (Print.spaceOrLinebreakIndented
                                                                (commentsBefore.lineSpread
                                                                    |> Print.lineSpreadMergeWith
                                                                        (\() -> tailPatternPrint |> Print.lineSpread)
                                                                )
                                                            )
                                             )
                                                |> Print.followedBy tailPatternPrint
                                            )
                                        )
                            )
                            tailPatternPrints
                            tailPatternsCommentsBefore
                            |> Print.listIntersperseAndFlatten
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
                                Print.empty

                            _ :: _ ->
                                commentsCollapsibleBeforeAliasName.print
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            commentsCollapsibleBeforeAliasName.lineSpread
                                        )
                        )
                    |> Print.followedBy
                        (Print.exactly (syntaxAs.aliasNameNode |> Elm.Syntax.Node.value))
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
                            Print.empty

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
                    )
                |> Print.followedBy printExactlyCurlyClosing

        field0 :: field1Up ->
            let
                commentsBeforeFields : { end : Elm.Syntax.Range.Location, reverse : List (List String) }
                commentsBeforeFields =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node elementRange _) soFar ->
                                { end = elementRange.end
                                , reverse =
                                    commentsInRange { start = soFar.end, end = elementRange.start } syntaxComments
                                        :: soFar.reverse
                                }
                            )
                            { end = syntaxRecord.fullRange.start
                            , reverse = []
                            }

                commentsAfterFields : List String
                commentsAfterFields =
                    commentsInRange { start = commentsBeforeFields.end, end = syntaxRecord.fullRange.end } syntaxComments

                lineSpread : Print.LineSpread
                lineSpread =
                    if
                        (commentsAfterFields |> List.isEmpty)
                            && (commentsBeforeFields.reverse |> List.all List.isEmpty)
                    then
                        Print.SingleLine

                    else
                        Print.MultipleLines
            in
            printExactlyCurlyOpeningSpace
                |> Print.followedBy
                    (List.map2
                        (\(Elm.Syntax.Node.Node _ fieldName) commentsBeforeField ->
                            Print.withIndentIncreasedBy 2
                                ((case commentsBeforeField of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                 )
                                    |> Print.followedBy (Print.exactly fieldName)
                                )
                        )
                        (field0 :: field1Up)
                        (commentsBeforeFields.reverse |> List.reverse)
                        |> Print.listIntersperseAndFlatten
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterFields of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            -- yes, in record patterns trailing comments
                            -- are indented and not separated with an extra linebreak
                            Print.withIndentIncreasedBy 2
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy
                                        (comments (comment0 :: comment1Up))
                                )
                    )
                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
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

        commentsBeforeFields :
            { end : Elm.Syntax.Range.Location
            , reverse :
                List
                    { beforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , betweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            }
        commentsBeforeFields =
            syntaxRecordExtension.fields
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange _, Elm.Syntax.Node.Node fieldValueRange _ )) soFar ->
                        let
                            commentsBeforeName : List String
                            commentsBeforeName =
                                commentsInRange
                                    { start = soFar.end, end = fieldNameRange.start }
                                    syntaxComments

                            commentsBetweenNameAndValue : List String
                            commentsBetweenNameAndValue =
                                commentsInRange
                                    { start = fieldNameRange.start, end = fieldValueRange.start }
                                    syntaxComments
                        in
                        { end = fieldValueRange.end
                        , reverse =
                            { beforeName =
                                case commentsBeforeName of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            , betweenNameAndValue =
                                case commentsBetweenNameAndValue of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            }
                                :: soFar.reverse
                        }
                    )
                    { end =
                        syntaxRecordExtension.recordVariable
                            |> Elm.Syntax.Node.range
                            |> .end
                    , reverse = []
                    }

        fieldValuePrints : List Print
        fieldValuePrints =
            syntaxRecordExtension.fields
                |> List.map
                    (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) ->
                        typeNotParenthesized syntaxComments fieldValue
                    )

        commentsAfterFields : List String
        commentsAfterFields =
            commentsInRange
                { start = commentsBeforeFields.end
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
                        fieldValuePrints
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
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
                        commentsBeforeFields.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\fieldComments ->
                                    (case fieldComments.beforeName of
                                        Nothing ->
                                            Print.SingleLine

                                        Just commentsBeforeName ->
                                            commentsBeforeName.lineSpread
                                    )
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                case fieldComments.betweenNameAndValue of
                                                    Nothing ->
                                                        Print.SingleLine

                                                    Just commentsBetweenNameAndValue ->
                                                        commentsBetweenNameAndValue.lineSpread
                                            )
                                )
                    )
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case commentsBeforeRecordVariable of
                    [] ->
                        Print.empty

                    _ :: _ ->
                        commentsCollapsibleBeforeRecordVariable.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    commentsCollapsibleBeforeRecordVariable.lineSpread
                                )
                )
                |> Print.followedBy
                    (Print.exactly
                        (syntaxRecordExtension.recordVariable
                            |> Elm.Syntax.Node.value
                        )
                    )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy printExactlyVerticalBarSpace
                    |> Print.followedBy
                        (List.map3
                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValue )) valuePrint fieldComments ->
                                let
                                    lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                    lineSpreadBetweenNameAndValueNotConsideringComments () =
                                        lineSpreadBetweenRanges
                                            fieldNameRange
                                            (fieldValue |> Elm.Syntax.Node.range)
                                            |> Print.lineSpreadMergeWith
                                                (\() -> valuePrint |> Print.lineSpread)
                                in
                                Print.withIndentIncreasedBy 2
                                    (case fieldComments.beforeName of
                                        Nothing ->
                                            Print.empty

                                        Just commentsBeforeName ->
                                            commentsBeforeName.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        commentsBeforeName.lineSpread
                                                    )
                                    )
                                    |> Print.followedBy (Print.exactly (fieldName ++ " :"))
                                    |> Print.followedBy
                                        (Print.withIndentIncreasedBy 2
                                            (Print.withIndentAtNextMultipleOf4
                                                ((case fieldComments.betweenNameAndValue of
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
                                                                            (\() -> valuePrint |> Print.lineSpread)
                                                                    )
                                                                )
                                                 )
                                                    |> Print.followedBy valuePrint
                                                )
                                            )
                                        )
                            )
                            syntaxRecordExtension.fields
                            fieldValuePrints
                            (commentsBeforeFields.reverse |> List.reverse)
                            |> Print.listIntersperseAndFlatten
                                (Print.emptyOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyCommaSpace
                                )
                        )
                    -- yes, elm-format indents trailing comments
                    |> Print.followedBy
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
        commentsBeforeArguments : List (Maybe { print : Print, lineSpread : Print.LineSpread })
        commentsBeforeArguments =
            syntaxConstruct.arguments
                |> List.foldl
                    (\argument soFar ->
                        let
                            commentsBeforeArgument : List String
                            commentsBeforeArgument =
                                commentsInRange
                                    { start = soFar.previousEnd
                                    , end = argument |> Elm.Syntax.Node.range |> .start
                                    }
                                    syntaxComments
                        in
                        { resultReverse =
                            (case commentsBeforeArgument of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.resultReverse
                        , previousEnd = argument |> Elm.Syntax.Node.range |> .end
                        }
                    )
                    { resultReverse = []
                    , previousEnd = syntaxConstruct.fullRange.start
                    }
                |> .resultReverse
                |> List.reverse

        argumentPrints : List Print
        argumentPrints =
            syntaxConstruct.arguments
                |> List.map
                    (\argument ->
                        specific.printArgumentParenthesizedIfSpaceSeparated syntaxComments
                            argument
                    )

        lineSpread : Print.LineSpread
        lineSpread =
            specific.lineSpreadMinimum
                |> Print.lineSpreadMergeWith
                    (\() ->
                        argumentPrints
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
                |> Print.lineSpreadMergeWith
                    (\() ->
                        commentsBeforeArguments
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )
    in
    Print.exactly syntaxConstruct.start
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (List.map2
                    (\argumentPrint maybeCommentsBeforeArgument ->
                        Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy
                                (case maybeCommentsBeforeArgument of
                                    Nothing ->
                                        Print.empty

                                    Just commentsBeforeArgument ->
                                        commentsBeforeArgument.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsBeforeArgument.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> argumentPrint |> Print.lineSpread)
                                                    )
                                                )
                                )
                            |> Print.followedBy argumentPrint
                    )
                    argumentPrints
                    commentsBeforeArguments
                    |> Print.listFlatten
                )
            )


tuple :
    (List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node part -> Print)
    -> List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : Elm.Syntax.Node.Node part
        , part1 : Elm.Syntax.Node.Node part
        }
    -> Print
tuple printPartNotParenthesized syntaxComments syntaxTuple =
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

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxTuple.fullRange
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

        part0Print : Print
        part0Print =
            printPartNotParenthesized syntaxComments syntaxTuple.part0

        part1Print : Print
        part1Print =
            printPartNotParenthesized syntaxComments syntaxTuple.part1
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((case beforePart0Comments of
                    [] ->
                        Print.empty

                    _ :: _ ->
                        beforePart0CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart0CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part0Print |> Print.lineSpread)
                                    )
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
                        Print.empty

                    _ :: _ ->
                        beforePart1CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart1CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part1Print |> Print.lineSpread)
                                    )
                                )
                 )
                    |> Print.followedBy part1Print
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
    (List (Elm.Syntax.Node.Node String) -> Elm.Syntax.Node.Node part -> Print)
    -> List (Elm.Syntax.Node.Node String)
    ->
        { fullRange : Elm.Syntax.Range.Range
        , part0 : Elm.Syntax.Node.Node part
        , part1 : Elm.Syntax.Node.Node part
        , part2 : Elm.Syntax.Node.Node part
        }
    -> Print
triple printPartNotParenthesized syntaxComments syntaxTriple =
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

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxTriple.fullRange
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

        part0Print : Print
        part0Print =
            printPartNotParenthesized syntaxComments syntaxTriple.part0

        part1Print : Print
        part1Print =
            printPartNotParenthesized syntaxComments syntaxTriple.part1

        part2Print : Print
        part2Print =
            printPartNotParenthesized syntaxComments syntaxTriple.part2
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((case beforePart0Comments of
                    [] ->
                        Print.empty

                    _ :: _ ->
                        beforePart0CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart0CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part0Print |> Print.lineSpread)
                                    )
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
                        Print.empty

                    _ :: _ ->
                        beforePart1CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart1CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part1Print |> Print.lineSpread)
                                    )
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
                        Print.empty

                    _ :: _ ->
                        beforePart2CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart2CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part2Print |> Print.lineSpread)
                                    )
                                )
                 )
                    |> Print.followedBy part2Print
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
                            Print.empty

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
                    )
                |> Print.followedBy printExactlyCurlyClosing

        field0 :: field1Up ->
            let
                commentsBeforeFields :
                    { end : Elm.Syntax.Range.Location
                    , reverse :
                        List
                            { beforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                            , betweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                            }
                    }
                commentsBeforeFields =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange _, Elm.Syntax.Node.Node fieldValueRange _ )) soFar ->
                                let
                                    commentsBeforeName : List String
                                    commentsBeforeName =
                                        commentsInRange
                                            { start = soFar.end, end = fieldNameRange.start }
                                            syntaxComments

                                    commentsBetweenNameAndValue : List String
                                    commentsBetweenNameAndValue =
                                        commentsInRange
                                            { start = fieldNameRange.start, end = fieldValueRange.start }
                                            syntaxComments
                                in
                                { end = fieldValueRange.end
                                , reverse =
                                    { beforeName =
                                        case commentsBeforeName of
                                            [] ->
                                                Nothing

                                            comment0 :: comment1Up ->
                                                Just (collapsibleComments (comment0 :: comment1Up))
                                    , betweenNameAndValue =
                                        case commentsBetweenNameAndValue of
                                            [] ->
                                                Nothing

                                            comment0 :: comment1Up ->
                                                Just (collapsibleComments (comment0 :: comment1Up))
                                    }
                                        :: soFar.reverse
                                }
                            )
                            { end = syntaxRecord.fullRange.start
                            , reverse = []
                            }

                fieldValuePrints : List Print
                fieldValuePrints =
                    (field0 :: field1Up)
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) ->
                                fieldSpecific.printValueNotParenthesized syntaxComments fieldValue
                            )

                commentsAfterFields : List String
                commentsAfterFields =
                    commentsInRange
                        { start = commentsBeforeFields.end
                        , end = syntaxRecord.fullRange.end
                        }
                        syntaxComments

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxRecord.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                fieldValuePrints
                                    |> Print.lineSpreadListMapAndCombine Print.lineSpread
                            )
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
                                commentsBeforeFields.reverse
                                    |> Print.lineSpreadListMapAndCombine
                                        (\fieldComments ->
                                            (case fieldComments.beforeName of
                                                Nothing ->
                                                    Print.SingleLine

                                                Just commentsBeforeName ->
                                                    commentsBeforeName.lineSpread
                                            )
                                                |> Print.lineSpreadMergeWith
                                                    (\() ->
                                                        case fieldComments.betweenNameAndValue of
                                                            Nothing ->
                                                                Print.SingleLine

                                                            Just commentsBetweenNameAndValue ->
                                                                commentsBetweenNameAndValue.lineSpread
                                                    )
                                        )
                            )
            in
            printExactlyCurlyOpeningSpace
                |> Print.followedBy
                    (List.map3
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValue )) valuePrint fieldComments ->
                            let
                                lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                lineSpreadBetweenNameAndValueNotConsideringComments () =
                                    lineSpreadBetweenRanges
                                        fieldNameRange
                                        (fieldValue |> Elm.Syntax.Node.range)
                                        |> Print.lineSpreadMergeWith
                                            (\() -> valuePrint |> Print.lineSpread)
                            in
                            Print.withIndentIncreasedBy 2
                                (case fieldComments.beforeName of
                                    Nothing ->
                                        Print.empty

                                    Just commentsBeforeName ->
                                        commentsBeforeName.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    commentsBeforeName.lineSpread
                                                )
                                )
                                |> Print.followedBy (Print.exactly (fieldName ++ " " ++ fieldSpecific.nameValueSeparator))
                                |> Print.followedBy
                                    (Print.withIndentIncreasedBy 2
                                        (Print.withIndentAtNextMultipleOf4
                                            ((case fieldComments.betweenNameAndValue of
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
                                                                        (\() -> valuePrint |> Print.lineSpread)
                                                                )
                                                            )
                                             )
                                                |> Print.followedBy valuePrint
                                            )
                                        )
                                    )
                        )
                        (field0 :: field1Up)
                        fieldValuePrints
                        (commentsBeforeFields.reverse |> List.reverse)
                        |> Print.listIntersperseAndFlatten
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterFields of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                |> Print.followedBy (comments (comment0 :: comment1Up))
                    )
                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
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

        afterArrowTypesBeforeRightestCommentsBefore :
            { end : Elm.Syntax.Range.Location
            , reverse : List (Maybe { print : Print, lineSpread : Print.LineSpread })
            }
        afterArrowTypesBeforeRightestCommentsBefore =
            afterArrowTypes.beforeRightest
                |> List.foldl
                    (\(Elm.Syntax.Node.Node afterArrowTypeRange _) soFar ->
                        let
                            commentsBeforeAfterArrowType : List String
                            commentsBeforeAfterArrowType =
                                commentsInRange
                                    { start = soFar.end, end = afterArrowTypeRange.start }
                                    syntaxComments
                        in
                        { end = afterArrowTypeRange.end
                        , reverse =
                            (case commentsBeforeAfterArrowType of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.reverse
                        }
                    )
                    { end = function.inType |> Elm.Syntax.Node.range |> .end
                    , reverse = []
                    }

        rightestAfterArrowTypePrint : Print
        rightestAfterArrowTypePrint =
            typeParenthesizedIfParenthesizedFunction syntaxComments
                afterArrowTypes.rightest

        commentsBeforeRightestAfterArrowType : List String
        commentsBeforeRightestAfterArrowType =
            commentsInRange
                { start = afterArrowTypesBeforeRightestCommentsBefore.end
                , end = afterArrowTypes.rightest |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRightestAfterArrowType : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRightestAfterArrowType =
            collapsibleComments commentsBeforeRightestAfterArrowType

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            lineSpreadInRange function.fullRange
                |> Print.lineSpreadMergeWith
                    (\() -> function.inType |> lineSpreadInNode)
                |> Print.lineSpreadMergeWith
                    (\() -> afterArrowTypes.rightest |> lineSpreadInNode)
                |> Print.lineSpreadMergeWith
                    (\() -> commentsCollapsibleBeforeRightestAfterArrowType.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> afterArrowTypes.beforeRightest |> Print.lineSpreadListMapAndCombine lineSpreadInNode)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        afterArrowTypesBeforeRightestCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )
    in
    typeParenthesizedIfFunction syntaxComments function.inType
        |> Print.followedBy
            (List.map2
                (\afterArrowTypeNode maybeCommentsBeforeAfterArrowType ->
                    let
                        afterArrowTypePrint : Print
                        afterArrowTypePrint =
                            typeParenthesizedIfFunction syntaxComments
                                afterArrowTypeNode
                    in
                    Print.spaceOrLinebreakIndented fullLineSpread
                        |> Print.followedBy printExactlyMinusGreaterThan
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                ((case maybeCommentsBeforeAfterArrowType of
                                    Nothing ->
                                        Print.spaceOrLinebreakIndented (lineSpreadInNode afterArrowTypeNode)

                                    Just commentsBeforeAfterArrowType ->
                                        let
                                            lineSpread : Print.LineSpread
                                            lineSpread =
                                                commentsBeforeAfterArrowType.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> afterArrowTypePrint |> Print.lineSpread)
                                        in
                                        Print.spaceOrLinebreakIndented lineSpread
                                            |> Print.followedBy commentsBeforeAfterArrowType.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    lineSpread
                                                )
                                 )
                                    |> Print.followedBy afterArrowTypePrint
                                )
                            )
                )
                afterArrowTypes.beforeRightest
                (afterArrowTypesBeforeRightestCommentsBefore.reverse
                    |> List.reverse
                )
                |> Print.listFlatten
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread
                |> Print.followedBy printExactlyMinusGreaterThan
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        ((case commentsBeforeRightestAfterArrowType of
                            [] ->
                                Print.spaceOrLinebreakIndented (lineSpreadInNode afterArrowTypes.rightest)

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
                        Print.empty

                    _ :: _ ->
                        commentsBeforeInnerCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented lineSpread)
                 )
                    |> Print.followedBy notParenthesizedPrint
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
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented lineSpread)
            )
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
                    tuple typeNotParenthesized
                        syntaxComments
                        { part0 = part0
                        , part1 = part1
                        , fullRange = fullRange
                        }

                [ part0, part1, part2 ] ->
                    triple typeNotParenthesized
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
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
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
        |> Print.followedBy printExactlyPortSpace
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
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterName |> Elm.Syntax.Node.range
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
                             )
                                |> Print.followedBy
                                    (Print.exactly (parameterName |> Elm.Syntax.Node.value))
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

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints |> Print.lineSpreadListMapAndCombine Print.lineSpread

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
    in
    (case syntaxTypeAliasDeclaration.documentation of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
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
        |> Print.followedBy printExactlyTypeSpaceAlias
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (Print.exactly (syntaxTypeAliasDeclaration.name |> Elm.Syntax.Node.value))
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (parameterPrints
                                |> Print.listMapAndFlatten
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
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
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
        parameterPrints :
            { end : Elm.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrints =
            syntaxChoiceTypeDeclaration.generics
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterPrintedRange : Elm.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterPattern |> Elm.Syntax.Node.range
                        in
                        { reverse =
                            ((case
                                commentsInRange
                                    { start = soFar.end, end = parameterPrintedRange.start }
                                    syntaxComments
                              of
                                [] ->
                                    Print.empty

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
                             )
                                |> Print.followedBy
                                    (Print.exactly (parameterPattern |> Elm.Syntax.Node.value))
                            )
                                :: soFar.reverse
                        , end = parameterPrintedRange.end
                        }
                    )
                    { reverse = []
                    , end =
                        syntaxChoiceTypeDeclaration.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints.reverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        variantPrints : List Print
        variantPrints =
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
                                (case commentsInRange { start = soFar.end, end = variant.name |> Elm.Syntax.Node.range |> .start } syntaxComments of
                                    [] ->
                                        Print.empty

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
                                )
                                    |> Print.followedBy
                                        variantPrint
                        in
                        { prints = commentsVariantPrint :: soFar.prints
                        , end = variantRange.end
                        }
                    )
                    { prints = []
                    , end = parameterPrints.end
                    }
                |> .prints
                |> List.reverse
    in
    (case syntaxChoiceTypeDeclaration.documentation of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
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
        |> Print.followedBy printExactlyType
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
                        (variantPrints
                            |> Print.listMapAndIntersperseAndFlatten
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
        parameterCommentsBefore :
            { end : Elm.Syntax.Range.Location
            , reverse : List (Maybe { lineSpread : Print.LineSpread, print : Print })
            }
        parameterCommentsBefore =
            implementation.arguments
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : Elm.Syntax.Range.Range
                            parameterRange =
                                parameterPattern |> Elm.Syntax.Node.range

                            commentsBeforeParameter : List String
                            commentsBeforeParameter =
                                commentsInRange
                                    { start = soFar.end, end = parameterRange.start }
                                    syntaxComments
                        in
                        { reverse =
                            (case commentsBeforeParameter of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.reverse
                        , end = parameterRange.end
                        }
                    )
                    { reverse = []
                    , end =
                        implementation.name
                            |> Elm.Syntax.Node.range
                            |> .end
                    }

        parameterPrints : List Print
        parameterPrints =
            implementation.arguments
                |> List.map
                    (\parameterPattern ->
                        patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
                    )

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        parameterCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )

        commentsBetweenParametersAndResult : List String
        commentsBetweenParametersAndResult =
            commentsInRange
                { start = parameterCommentsBefore.end
                , end =
                    implementation.expression
                        |> Elm.Syntax.Node.range
                        |> .start
                }
                syntaxComments
    in
    Print.exactly (implementation.name |> Elm.Syntax.Node.value)
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((List.map2
                    (\parameterPrint maybeCommentsBefore ->
                        Print.spaceOrLinebreakIndented parametersLineSpread
                            |> Print.followedBy
                                (case maybeCommentsBefore of
                                    Nothing ->
                                        Print.empty

                                    Just commentsBefore ->
                                        commentsBefore.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsBefore.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> parameterPrint |> Print.lineSpread)
                                                    )
                                                )
                                )
                            |> Print.followedBy parameterPrint
                    )
                    parameterPrints
                    (parameterCommentsBefore.reverse |> List.reverse)
                    |> Print.listFlatten
                 )
                    |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                    |> Print.followedBy printExactlyEquals
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsBetweenParametersAndResult of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
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
    (case syntaxExpressionDeclaration.documentation of
        Nothing ->
            Print.empty

        Just (Elm.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
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
        |> Print.followedBy
            (case syntaxExpressionDeclaration.signature of
                Nothing ->
                    Print.empty

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
                                    Print.empty

                                comment0 :: comment1Up ->
                                    printLinebreakLinebreakLinebreak
                                        |> Print.followedBy
                                            (moduleLevelComments (comment0 :: comment1Up))
                                        |> Print.followedBy printLinebreakLinebreak
                            )
            )
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
            printExactlyMinus
                |> Print.followedBy
                    (expressionParenthesizedIfSpaceSeparated syntaxComments
                        negated
                    )

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
                    tuple expressionNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1 }

                [ part0, part1, part2 ] ->
                    triple expressionNotParenthesized
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


expressionGlsl : String -> Print
expressionGlsl glslContent =
    Print.exactly "[glsl|"
        |> Print.followedBy
            (glslContent
                |> String.lines
                |> Print.listMapToExactAndIntersperseAndFlatten
                    identity
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

        argument1UpCommentsBefore : List (Maybe { print : Print, lineSpread : Print.LineSpread })
        argument1UpCommentsBefore =
            syntaxCall.argument1Up
                |> List.foldl
                    (\argument soFar ->
                        let
                            commentsBeforeArgument : List String
                            commentsBeforeArgument =
                                commentsInRange
                                    { start = soFar.previousEnd
                                    , end = argument |> Elm.Syntax.Node.range |> .start
                                    }
                                    syntaxComments
                        in
                        { resultReverse =
                            (case commentsBeforeArgument of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.resultReverse
                        , previousEnd = argument |> Elm.Syntax.Node.range |> .end
                        }
                    )
                    { resultReverse = []
                    , previousEnd =
                        syntaxCall.argument0
                            |> Elm.Syntax.Node.range
                            |> .end
                    }
                |> .resultReverse
                |> List.reverse

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
                        argument1UpPrints
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
                |> Print.lineSpreadMergeWith
                    (\() ->
                        argument1UpCommentsBefore
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )

        argument1UpPrints : List Print
        argument1UpPrints =
            syntaxCall.argument1Up
                |> List.map
                    (\argument -> expressionParenthesizedIfSpaceSeparated syntaxComments argument)
    in
    appliedPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented argument0LineSpread
                    |> Print.followedBy
                        (case commentsBeforeArgument0 of
                            [] ->
                                Print.empty

                            _ :: _ ->
                                collapsibleCommentsBeforeArgument0.print
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            (collapsibleCommentsBeforeArgument0.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> argument0Print |> Print.lineSpread)
                                            )
                                        )
                        )
                    |> Print.followedBy argument0Print
                    |> Print.followedBy
                        (List.map2
                            (\argumentPrint maybeCommentsBeforeArgument ->
                                Print.spaceOrLinebreakIndented argument1UpLineSpread
                                    |> Print.followedBy
                                        (case maybeCommentsBeforeArgument of
                                            Nothing ->
                                                Print.empty

                                            Just commentsBeforeArgument ->
                                                commentsBeforeArgument.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            (commentsBeforeArgument.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() -> argumentPrint |> Print.lineSpread)
                                                            )
                                                        )
                                        )
                                    |> Print.followedBy argumentPrint
                            )
                            argument1UpPrints
                            argument1UpCommentsBefore
                            |> Print.listFlatten
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

        beforeRightestComments :
            { commentsReverse : List (Maybe { print : Print, lineSpread : Print.LineSpread })
            , end : Elm.Syntax.Range.Location
            }
        beforeRightestComments =
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
                                    { start = soFar.end, end = expressionRange.start }
                                    syntaxComments
                        in
                        { end = expressionRange.end
                        , commentsReverse =
                            (case commentsBefore of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.commentsReverse
                        }
                    )
                    { end = operationExpanded.leftest |> Elm.Syntax.Node.range |> .end
                    , commentsReverse = []
                    }

        commentsBeforeRightestExpression : List String
        commentsBeforeRightestExpression =
            commentsInRange
                { start = beforeRightestComments.end
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

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxOperation.fullRange
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRightestExpression.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        beforeRightestComments.commentsReverse
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )

        beforeRightestOperatorExpressionChainWithPreviousLineSpread :
            { previousLineSpread : Print.LineSpread
            , rightToLeft :
                List
                    { operator : String
                    , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    , expressionPrint : Print
                    , commentsBeforeExpression : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , previousLineSpread : Print.LineSpread
                    }
            }
        beforeRightestOperatorExpressionChainWithPreviousLineSpread =
            List.map2
                (\operatorExpression commentsBeforeExpression ->
                    { operator = operatorExpression.operator
                    , expression = operatorExpression.expression
                    , commentsBeforeExpression = commentsBeforeExpression
                    }
                )
                operationExpanded.beforeRightestOperatorExpressionChain
                (beforeRightestComments.commentsReverse |> List.reverse)
                |> List.foldl
                    (\operatorExpression soFar ->
                        let
                            expressionPrint : Print
                            expressionPrint =
                                expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                    operatorExpression.expression
                        in
                        { previousLineSpread = Print.lineSpread expressionPrint
                        , rightToLeft =
                            { operator = operatorExpression.operator
                            , expression = operatorExpression.expression
                            , expressionPrint = expressionPrint
                            , commentsBeforeExpression = operatorExpression.commentsBeforeExpression
                            , previousLineSpread = soFar.previousLineSpread
                            }
                                :: soFar.rightToLeft
                        }
                    )
                    { previousLineSpread = Print.lineSpread leftestPrint
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
                    Print.spaceOrLinebreakIndented beforeRightestOperatorExpressionChainWithPreviousLineSpread.previousLineSpread
                        |> Print.followedBy printExactlyLessThanVerticalBar
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy
                                        (case commentsBeforeRightestExpression of
                                            [] ->
                                                Print.empty

                                            _ :: _ ->
                                                commentsCollapsibleBeforeRightestExpression.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            (commentsCollapsibleBeforeRightestExpression.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() -> expressionPrint |> Print.lineSpread)
                                                            )
                                                        )
                                        )
                                    |> Print.followedBy expressionPrint
                                )
                            )

                nonApLOperator ->
                    let
                        expressionPrint : Print
                        expressionPrint =
                            if operationExpanded.rightestExpression |> expressionIsSpaceSeparatedExceptApplication then
                                Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                    (expressionParenthesized syntaxComments operationExpanded.rightestExpression)

                            else
                                expressionNotParenthesized syntaxComments operationExpanded.rightestExpression
                    in
                    Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy (Print.exactly (nonApLOperator ++ " "))
                            |> Print.followedBy
                                (case commentsBeforeRightestExpression of
                                    [] ->
                                        Print.empty

                                    _ :: _ ->
                                        Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                            (commentsCollapsibleBeforeRightestExpression.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsCollapsibleBeforeRightestExpression.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> expressionPrint |> Print.lineSpread)
                                                        )
                                                    )
                                            )
                                )
                            |> Print.followedBy expressionPrint
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
                                                    (case operatorExpression.commentsBeforeExpression of
                                                        Nothing ->
                                                            Print.empty

                                                        Just commentsBeforeExpression ->
                                                            commentsBeforeExpression.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBeforeExpression.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> operatorExpression.expressionPrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                    )
                                                |> Print.followedBy operatorExpression.expressionPrint
                                                |> Print.followedBy chainRightPrint
                                            )
                                        )

                            nonApLOperator ->
                                Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented lineSpread
                                        |> Print.followedBy (Print.exactly (nonApLOperator ++ " "))
                                        |> Print.followedBy
                                            (case operatorExpression.commentsBeforeExpression of
                                                Nothing ->
                                                    Print.empty

                                                Just commentsBeforeExpression ->
                                                    Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                                        (commentsBeforeExpression.print
                                                            |> Print.followedBy
                                                                (Print.spaceOrLinebreakIndented
                                                                    (commentsBeforeExpression.lineSpread
                                                                        |> Print.lineSpreadMergeWith
                                                                            (\() -> operatorExpression.expressionPrint |> Print.lineSpread)
                                                                    )
                                                                )
                                                        )
                                            )
                                        |> Print.followedBy
                                            (if operatorExpression.expression |> expressionIsSpaceSeparatedExceptApplication then
                                                Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                                    operatorExpression.expressionPrint

                                             else
                                                operatorExpression.expressionPrint
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
                            Print.empty

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
                    )
                |> Print.followedBy printExactlySquareClosing

        element0 :: element1Up ->
            let
                commentsBeforeElements :
                    { end : Elm.Syntax.Range.Location
                    , reverse : List (Maybe { print : Print, lineSpread : Print.LineSpread })
                    }
                commentsBeforeElements =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node elementRange _) soFar ->
                                let
                                    commentsBeforeElement : List String
                                    commentsBeforeElement =
                                        commentsInRange { start = soFar.end, end = elementRange.start }
                                            syntaxComments
                                in
                                { end = elementRange.end
                                , reverse =
                                    (case commentsBeforeElement of
                                        [] ->
                                            Nothing

                                        comment0 :: comment1Up ->
                                            Just (collapsibleComments (comment0 :: comment1Up))
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { end = syntaxList.fullRange.start
                            , reverse = []
                            }

                commentsAfterElements : List String
                commentsAfterElements =
                    commentsInRange { start = commentsBeforeElements.end, end = syntaxList.fullRange.end } syntaxComments

                elementPrints : List Print
                elementPrints =
                    (element0 :: element1Up)
                        |> List.map
                            (\element ->
                                expressionNotParenthesized syntaxComments
                                    element
                            )

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxList.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                elementPrints
                                    |> Print.lineSpreadListMapAndCombine Print.lineSpread
                            )
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                commentsBeforeElements.reverse
                                    |> Print.lineSpreadListMapAndCombine
                                        (\c -> c |> maybeLineSpread .lineSpread)
                            )
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    (List.map2
                        (\elementPrint maybeCommentsBeforeElement ->
                            Print.withIndentIncreasedBy 2
                                ((case maybeCommentsBeforeElement of
                                    Nothing ->
                                        Print.empty

                                    Just commentsBeforeElement ->
                                        commentsBeforeElement.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsBeforeElement.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> elementPrint |> Print.lineSpread)
                                                    )
                                                )
                                 )
                                    |> Print.followedBy elementPrint
                                )
                        )
                        elementPrints
                        (commentsBeforeElements.reverse |> List.reverse)
                        |> Print.listIntersperseAndFlatten
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterElements of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                |> Print.followedBy (comments (comment0 :: comment1Up))
                    )
                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
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
        commentsBeforeRecordVariable : List String
        commentsBeforeRecordVariable =
            commentsInRange
                { start = syntaxRecordUpdate.fullRange.start
                , end = syntaxRecordUpdate.recordVariable |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsBeforeFields :
            { end : Elm.Syntax.Range.Location
            , reverse :
                List
                    { beforeName : List String
                    , betweenNameAndValue : List String
                    }
            }
        commentsBeforeFields =
            syntaxRecordUpdate.fields
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange _, Elm.Syntax.Node.Node fieldValueRange _ )) soFar ->
                        { end = fieldValueRange.end
                        , reverse =
                            { beforeName =
                                commentsInRange
                                    { start = soFar.end, end = fieldNameRange.start }
                                    syntaxComments
                            , betweenNameAndValue =
                                commentsInRange
                                    { start = fieldNameRange.start, end = fieldValueRange.start }
                                    syntaxComments
                            }
                                :: soFar.reverse
                        }
                    )
                    { end = syntaxRecordUpdate.recordVariable |> Elm.Syntax.Node.range |> .end
                    , reverse = []
                    }

        commentsAfterFields : List String
        commentsAfterFields =
            commentsInRange
                { start = commentsBeforeFields.end
                , end = syntaxRecordUpdate.fullRange.end
                }
                syntaxComments

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxRecordUpdate.fullRange
                |> Print.lineSpreadMergeWith
                    (\() ->
                        if
                            (commentsBeforeRecordVariable |> List.isEmpty)
                                && (commentsAfterFields |> List.isEmpty)
                                && (commentsBeforeFields.reverse
                                        |> List.all
                                            (\fieldComments ->
                                                (fieldComments.beforeName |> List.isEmpty)
                                                    && (fieldComments.betweenNameAndValue |> List.isEmpty)
                                            )
                                   )
                        then
                            Print.SingleLine

                        else
                            Print.MultipleLines
                    )
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case commentsBeforeRecordVariable of
                    [] ->
                        Print.empty

                    comment0 :: comment1Up ->
                        comments (comment0 :: comment1Up)
                            |> Print.followedBy Print.linebreakIndented
                )
                |> Print.followedBy
                    (Print.exactly
                        (syntaxRecordUpdate.recordVariable
                            |> Elm.Syntax.Node.value
                        )
                    )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy printExactlyVerticalBarSpace
                    |> Print.followedBy
                        (List.map2
                            (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node fieldNameRange fieldName, fieldValue )) fieldComments ->
                                Print.withIndentIncreasedBy 2
                                    (case fieldComments.beforeName of
                                        [] ->
                                            Print.empty

                                        comment0 :: comment1Up ->
                                            comments (comment0 :: comment1Up)
                                                |> Print.followedBy Print.linebreakIndented
                                    )
                                    |> Print.followedBy (Print.exactly (fieldName ++ " ="))
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            ((case fieldComments.betweenNameAndValue of
                                                [] ->
                                                    Print.spaceOrLinebreakIndented
                                                        (lineSpreadBetweenRanges
                                                            fieldNameRange
                                                            (fieldValue |> Elm.Syntax.Node.range)
                                                        )

                                                comment0 :: comment1Up ->
                                                    Print.linebreakIndented
                                                        |> Print.followedBy (comments (comment0 :: comment1Up))
                                                        |> Print.followedBy Print.linebreakIndented
                                             )
                                                |> Print.followedBy
                                                    (expressionNotParenthesized syntaxComments fieldValue)
                                            )
                                        )
                            )
                            syntaxRecordUpdate.fields
                            (commentsBeforeFields.reverse |> List.reverse)
                            |> Print.listIntersperseAndFlatten
                                (Print.emptyOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyCommaSpace
                                )
                        )
                    -- yes, elm-format indents trailing comments
                    |> Print.followedBy
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
        parameterCommentsBefore :
            { end : Elm.Syntax.Range.Location
            , reverse : List (Maybe { lineSpread : Print.LineSpread, print : Print })
            }
        parameterCommentsBefore =
            syntaxLambda.args
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : Elm.Syntax.Range.Range
                            parameterRange =
                                parameterPattern |> Elm.Syntax.Node.range

                            commentsBeforeParameter : List String
                            commentsBeforeParameter =
                                commentsInRange
                                    { start = soFar.end, end = parameterRange.start }
                                    syntaxComments
                        in
                        { reverse =
                            (case commentsBeforeParameter of
                                [] ->
                                    Nothing

                                comment0 :: comment1Up ->
                                    Just (collapsibleComments (comment0 :: comment1Up))
                            )
                                :: soFar.reverse
                        , end = parameterRange.end
                        }
                    )
                    { reverse = []
                    , end = fullRange.start
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = parameterCommentsBefore.end
                , end = syntaxLambda.expression |> Elm.Syntax.Node.range |> .start
                }
                syntaxComments

        parameterPrints : List Print
        parameterPrints =
            List.map
                (\parameterPattern ->
                    patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
                )
                syntaxLambda.args

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        parameterCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\c -> c |> maybeLineSpread .lineSpread)
                    )
    in
    printExactlyBackSlash
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                (Print.emptyOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (List.map2
                            (\parameterPrint maybeCommentsBefore ->
                                (case maybeCommentsBefore of
                                    Nothing ->
                                        Print.empty

                                    Just commentsBefore ->
                                        commentsBefore.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsBefore.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> parameterPrint |> Print.lineSpread)
                                                    )
                                                )
                                )
                                    |> Print.followedBy parameterPrint
                            )
                            parameterPrints
                            (parameterCommentsBefore.reverse |> List.reverse)
                            |> Print.listIntersperseAndFlatten
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
                                            (\() -> lineSpreadInRange fullRange)
                                    )

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy Print.linebreakIndented
                         )
                            |> Print.followedBy
                                (expressionNotParenthesized syntaxComments
                                    syntaxLambda.expression
                                )
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
                                Print.empty

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                        )
                    |> Print.followedBy conditionPrint
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
                                Print.empty

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                        )
                    |> Print.followedBy onTruePrint
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
                    Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsBeforeOnFalse of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                )
                            |> Print.followedBy
                                (expressionNotParenthesized syntaxComments syntaxIfThenElse.onFalse)
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
    in
    printExactlyCase
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented casedExpressionLineSpread
                    |> Print.followedBy
                        (case commentsBeforeCasedExpression of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                        )
                    |> Print.followedBy
                        (expressionNotParenthesized syntaxComments
                            syntaxCaseOf.expression
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
                                                { start = soFar.end
                                                , end = casePattern |> Elm.Syntax.Node.range |> .start
                                                }
                                                syntaxComments

                                        commentsAndCasePrint : Print
                                        commentsAndCasePrint =
                                            (case commentsBeforeCasePattern of
                                                [] ->
                                                    Print.empty

                                                comment0 :: comment1Up ->
                                                    comments (comment0 :: comment1Up)
                                                        |> Print.followedBy
                                                            Print.linebreakIndented
                                            )
                                                |> Print.followedBy
                                                    (case_ syntaxComments ( casePattern, caseResult ))
                                    in
                                    { end = caseResult |> Elm.Syntax.Node.range |> .end
                                    , reverse = commentsAndCasePrint :: soFar.reverse
                                    }
                                )
                                { end = syntaxCaseOf.expression |> Elm.Syntax.Node.range |> .end
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
        letDeclarationPrints : { end : Elm.Syntax.Range.Location, printsReverse : List Print }
        letDeclarationPrints =
            (syntaxLetIn.letDeclaration0 :: syntaxLetIn.letDeclaration1Up)
                |> List.foldl
                    (\(Elm.Syntax.Node.Node letDeclarationRange letDeclaration) soFar ->
                        let
                            commentsBefore : List String
                            commentsBefore =
                                commentsInRange
                                    { start = soFar.end
                                    , end = letDeclarationRange.start
                                    }
                                    syntaxComments

                            letDeclarationPrint : Print
                            letDeclarationPrint =
                                (case commentsBefore of
                                    [] ->
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                )
                                    |> Print.followedBy
                                        (expressionLetDeclaration syntaxComments letDeclaration)
                        in
                        { end = letDeclarationRange.end
                        , printsReverse =
                            letDeclarationPrint :: soFar.printsReverse
                        }
                    )
                    { end = syntaxLetIn.fullRange.start
                    , printsReverse = []
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = letDeclarationPrints.end
                , end =
                    syntaxLetIn.result
                        |> Elm.Syntax.Node.range
                        |> .start
                }
                syntaxComments
    in
    printExactlyLet
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (letDeclarationPrints.printsReverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                printLinebreakLinebreakIndented
                        )
                )
            )
        |> Print.followedBy printLinebreakIndentedInLinebreakIndented
        |> Print.followedBy
            (case commentsBeforeResult of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy Print.linebreakIndented
            )
        |> Print.followedBy
            (expressionNotParenthesized syntaxComments syntaxLetIn.result)


expressionLetDeclaration :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.LetDeclaration
    -> Print
expressionLetDeclaration syntaxComments letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetFunction letDeclarationExpression ->
            (case letDeclarationExpression.signature of
                Nothing ->
                    Print.empty

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
                                    Print.empty

                                comment0 :: comment1Up ->
                                    Print.linebreakIndented
                                        |> Print.followedBy
                                            (comments (comment0 :: comment1Up))
                            )
                        |> Print.followedBy Print.linebreakIndented
            )
                |> Print.followedBy
                    (declarationExpressionImplementation syntaxComments
                        (letDeclarationExpression.declaration |> Elm.Syntax.Node.value)
                    )

        Elm.Syntax.Expression.LetDestructuring destructuringPattern destructuredExpression ->
            let
                commentsBeforeDestructuredExpression : List String
                commentsBeforeDestructuredExpression =
                    commentsInRange
                        { start = destructuringPattern |> Elm.Syntax.Node.range |> .end
                        , end = destructuredExpression |> Elm.Syntax.Node.range |> .start
                        }
                        syntaxComments

                destructuringPatternPrint =
                    patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern
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
                                        Print.empty

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy
                                                Print.linebreakIndented
                                )
                            |> Print.followedBy
                                (expressionNotParenthesized syntaxComments
                                    destructuredExpression
                                )
                        )
                    )


expressionToNotParenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
expressionToNotParenthesized (Elm.Syntax.Node.Node fullRange syntaxExpression) =
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
                                Print.empty

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                        )
                    |> Print.followedBy
                        (expressionNotParenthesized syntaxComments caseResult)
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
    Print.exactly " { "


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


printExactlySingleQuote : Print
printExactlySingleQuote =
    Print.exactly "'"


printExactlyDoubleQuote : Print
printExactlyDoubleQuote =
    Print.exactly "\""


printExactlyDoubleQuoteDoubleQuoteDoubleQuote : Print
printExactlyDoubleQuoteDoubleQuoteDoubleQuote =
    Print.exactly "\"\"\""


printExactlyComma : Print
printExactlyComma =
    Print.exactly ","


printExactlyMinusGreaterThan : Print
printExactlyMinusGreaterThan =
    Print.exactly "->"


printExactlyEquals : Print
printExactlyEquals =
    Print.exactly "="


printExactlyDotDot : Print
printExactlyDotDot =
    Print.exactly ".."


printExactlyParensOpeningDotDotParensClosing : Print
printExactlyParensOpeningDotDotParensClosing =
    Print.exactly "(..)"


printExactlyBackSlash : Print
printExactlyBackSlash =
    Print.exactly "\\"


printExactlyVerticalBar : Print
printExactlyVerticalBar =
    Print.exactly "|"


printExactlyLessThanVerticalBar : Print
printExactlyLessThanVerticalBar =
    Print.exactly "<|"


printExactlyColon : Print
printExactlyColon =
    Print.exactly ":"


printExactlyMinus : Print
printExactlyMinus =
    Print.exactly "-"


printExactlyCurlyOpeningMinus : Print
printExactlyCurlyOpeningMinus =
    Print.exactly "{-"


printExactlyMinusCurlyClosing : Print
printExactlyMinusCurlyClosing =
    Print.exactly "-}"


printExactlyCurlyOpeningMinusMinusCurlyClosing : Print
printExactlyCurlyOpeningMinusMinusCurlyClosing =
    Print.exactly "{--}"


printExactlyParensOpeningClosing : Print
printExactlyParensOpeningClosing =
    Print.exactly "()"


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


printExactlyColonColon : Print
printExactlyColonColon =
    Print.exactly "::"


printExactlyDot : Print
printExactlyDot =
    Print.exactly "."


printExactly0x : Print
printExactly0x =
    Print.exactly "0x"


printExactlyType : Print
printExactlyType =
    Print.exactly "type"


printExactlyAlias : Print
printExactlyAlias =
    Print.exactly "alias"


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


printExactlyPort : Print
printExactlyPort =
    Print.exactly "port"


printExactlyModule : Print
printExactlyModule =
    Print.exactly "module"


printExactEffect : Print
printExactEffect =
    Print.exactly "effect"


printExactWhere : Print
printExactWhere =
    Print.exactly "where"


printExactCommand : Print
printExactCommand =
    Print.exactly "command"


printExactSubscription : Print
printExactSubscription =
    Print.exactly "subscription"


printExactImport : Print
printExactImport =
    Print.exactly "import"
