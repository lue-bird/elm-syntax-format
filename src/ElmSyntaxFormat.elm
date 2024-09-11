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
import Print exposing (Print)


module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    -- TODO header
    -- TODO declarations
    -- TODO comments
    Print.inSequence
        (syntaxModule.imports
            |> List.map (\(Elm.Syntax.Node.Node _ syntaxImport) -> import_ syntaxImport)
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
                                    |> Print.followedBy (exposing_ syntaxExposing)
                                )
                            )
            )


lineOffsetBetweenNodes : Elm.Syntax.Node.Node a -> Elm.Syntax.Node.Node b -> Print.LineOffset
lineOffsetBetweenNodes (Elm.Syntax.Node.Node earlierRange _) (Elm.Syntax.Node.Node laterRange _) =
    if earlierRange.end.row == laterRange.start.row then
        Print.NextLine

    else
        Print.SameLine


lineOffsetInNode : Elm.Syntax.Node.Node a -> Print.LineOffset
lineOffsetInNode (Elm.Syntax.Node.Node range _) =
    lineOffsetInRange range


lineOffsetInRange : Elm.Syntax.Range.Range -> Print.LineOffset
lineOffsetInRange range =
    if range.start.row == range.end.row then
        Print.NextLine

    else
        Print.SameLine


lineOffsetBetweenNodeList : Elm.Syntax.Node.Node a -> Elm.Syntax.Node.Node b -> Print.LineOffset
lineOffsetBetweenNodeList (Elm.Syntax.Node.Node earlierRange _) (Elm.Syntax.Node.Node laterRange _) =
    if earlierRange.end.row == laterRange.start.row then
        Print.NextLine

    else
        Print.SameLine


exposing_ : Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing -> Print
exposing_ (Elm.Syntax.Node.Node exposingRange syntaxExposing) =
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
                                (lineOffsetInRange exposingRange)
                                (exposingSet
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ syntaxExpose) -> expose syntaxExpose)
                                )
            )
        |> Print.followedBy (Print.symbol ")")


commaSeparated : Print.LineOffset -> List Print -> Print
commaSeparated lineOffset elements =
    Print.emptiableLayoutPositiveIndent lineOffset
        |> Print.followedBy
            (Print.inSequence
                (elements
                    |> List.map
                        (\elementPrint ->
                            elementPrint
                                |> Print.followedBy
                                    (Print.layoutPositiveIndent lineOffset)
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
                    Print.symbol "("
                        |> Print.followedBy
                            (Print.symbol syntaxExposeType.name)
                        |> Print.followedBy (Print.symbol ")")
