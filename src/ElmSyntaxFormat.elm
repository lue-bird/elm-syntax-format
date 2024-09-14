module ElmSyntaxFormat exposing
    ( module_, moduleName, exposing_, expose, imports, import_
    , declarations, declaration, declarationChoiceType, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , patternNotParenthesized, patternParenthesizedIfSpaceSeparated, typeNotParenthesized
    , case_
    )

{-| Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

@docs module_, moduleName, exposing_, expose, imports, import_
@docs declarations, declaration, declarationChoiceType, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expression, patternNotParenthesized, patternParenthesizedIfSpaceSeparated, typeNotParenthesized

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
import Print exposing (LineOffset, Print)
import Unicode


module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule =
    -- TODO module documentation
    -- TODO comments
    let
        maybeModuleDocumentation : Maybe (Elm.Syntax.Node.Node String)
        maybeModuleDocumentation =
            moduleDocumentation syntaxModule

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
        |> moduleHeader
        |> Print.followedBy maybeModuleDocumentationPrint
        |> Print.followedBy importsPrint
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (syntaxModule.declarations
                |> List.map Elm.Syntax.Node.value
                |> declarations
            )


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
moduleDocumentationBeforeCutOffLine cutOffLine comments =
    case comments of
        [] ->
            Nothing

        comment :: restOfComments ->
            let
                (Elm.Syntax.Node.Node range content) =
                    comment
            in
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just comment

            else
                moduleDocumentationBeforeCutOffLine cutOffLine restOfComments


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
    -- TODO inline (and check indentation!)
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
    -- TODO cap out
    Print.symbol (String.fromInt int)


cappedHex : Int -> Print
cappedHex int =
    -- TODO cap out
    Print.symbol (intToHexString int)


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
            typePrint : Print
            typePrint =
                typeNotParenthesized typeNode
        in
        Print.symbol "("
            |> Print.followedBy (Print.bumpIndentBy1 typePrint)
            |> Print.followedBy
                (Print.emptiableLayoutPositiveIndent (Print.lineOffset typePrint))
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
typeNotParenthesized (Elm.Syntax.Node.Node fullRange syntaxType) =
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
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (typeNotParenthesized part0))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (typeNotParenthesized part1))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                [ part0, part1, part2 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (typeNotParenthesized part0))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (typeNotParenthesized part1))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (typeNotParenthesized part2))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
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
                                    |> List.map (\part -> Print.bumpIndentBy2 (typeNotParenthesized part))
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
                                                    (Print.bumpIndentBy4
                                                        (Print.layoutPositiveIndent (lineOffsetInNode fieldValue)
                                                            |> Print.followedBy
                                                                (typeNotParenthesized fieldValue)
                                                        )
                                                    )
                                        )
                                )
                            )
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
                |> Print.followedBy (Print.layoutPositiveIndent (lineOffsetInRange fullRange))
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
                                                    |> Print.followedBy
                                                        (Print.bumpIndentBy4
                                                            (Print.layoutPositiveIndent (lineOffsetInNode fieldValue)
                                                                |> Print.followedBy (typeNotParenthesized fieldValue)
                                                            )
                                                        )
                                            )
                                    )
                                )
                        )
                    )
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            typeFunctionNotParenthesized inType outType


declarations : List Elm.Syntax.Declaration.Declaration -> Print
declarations syntaxDeclarations =
    Print.inSequence
        (syntaxDeclarations
            |> List.map declaration
            |> List.intersperse
                (Print.linebreak
                    |> Print.followedBy Print.linebreak
                    |> Print.followedBy Print.linebreak
                )
        )


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
            patternParenthesizedIfSpaceSeparated destructuringPattern
                |> Print.followedBy Print.space
                |> Print.followedBy (Print.symbol "=")
                |> Print.followedBy
                    (Print.bumpIndentBy4
                        (Print.layoutPositiveIndent Print.NextLine
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
        |> Print.followedBy (Print.layoutPositiveIndent Print.NextLine)
        |> Print.followedBy (typeNotParenthesized syntaxTypeAliasDeclaration.typeAnnotation)


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
        |> Print.followedBy Print.space
        |> Print.followedBy
            (Print.inSequence
                (syntaxChoiceTypeDeclaration.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            Print.symbol parameter
                                |> Print.followedBy Print.space
                        )
                )
            )
        |> Print.followedBy (Print.layoutPositiveIndent Print.NextLine)
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
                                    (Print.bumpIndentBy4
                                        (Print.inSequence
                                            (parameterPrints
                                                |> List.map
                                                    (\parameterPrint ->
                                                        Print.layoutPositiveIndent parametersLineOffset
                                                            |> Print.followedBy parameterPrint
                                                    )
                                            )
                                        )
                                    )
                        )
                    |> List.intersperse
                        (Print.layoutPositiveIndent Print.NextLine
                            |> Print.followedBy (Print.symbol "|")
                            |> Print.followedBy Print.space
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
            Print.symbol "left"

        Elm.Syntax.Infix.Right ->
            Print.symbol "right"

        Elm.Syntax.Infix.Non ->
            Print.symbol "non"


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
            (Print.layoutPositiveIndent (Print.lineOffset typePrint))
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
        |> Print.followedBy (Print.layoutPositiveIndent Print.NextLine)
        |> Print.followedBy (expressionNotParenthesized implementation.expression)


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
                        argument1UpLineOffset : LineOffset
                        argument1UpLineOffset =
                            lineOffsetInRange fullRange
                    in
                    expressionParenthesizedIfSpaceSeparated applied
                        |> Print.followedBy
                            (Print.bumpIndentBy4
                                (Print.layoutPositiveIndent (lineOffsetInNode argument0)
                                    |> Print.followedBy (expressionParenthesizedIfSpaceSeparated argument0)
                                    |> Print.followedBy
                                        (Print.inSequence
                                            (argument1Up
                                                |> List.map
                                                    (\argument ->
                                                        Print.layoutPositiveIndent argument1UpLineOffset
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
            in
            expressionParenthesizedIfSpaceSeparatedExceptNonParenthesizedOperation left
                |> Print.followedBy
                    (Print.bumpIndentBy4
                        (Print.layoutPositiveIndent lineOffset
                            |> Print.followedBy (Print.symbol operator)
                            |> Print.followedBy
                                (Print.bumpIndentBy4
                                    (Print.layoutPositiveIndent lineOffset
                                        |> Print.followedBy
                                            (expressionParenthesizedIfSpaceSeparatedExceptNonParenthesizedOperation right)
                                    )
                                )
                        )
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
            -- TODO cap out?
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
                        |> Print.followedBy (Print.bumpIndentBy2 (expressionNotParenthesized part0))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (expressionNotParenthesized part1))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ")")

                [ part0, part1, part2 ] ->
                    let
                        lineOffset : Print.LineOffset
                        lineOffset =
                            lineOffsetInRange fullRange
                    in
                    Print.symbol "("
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (expressionNotParenthesized part0))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (expressionNotParenthesized part1))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
                        |> Print.followedBy (Print.symbol ",")
                        |> Print.followedBy Print.space
                        |> Print.followedBy (Print.bumpIndentBy2 (expressionNotParenthesized part2))
                        |> Print.followedBy (Print.layoutPositiveIndent lineOffset)
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
                                    |> List.map (\part -> Print.bumpIndentBy2 (expressionNotParenthesized part))
                                )
                            )
                        |> Print.followedBy (Print.symbol ")")

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            expressionNotParenthesized inParens

        Elm.Syntax.Expression.LetExpression syntaxLetIn ->
            expressionLetIn syntaxLetIn

        Elm.Syntax.Expression.CaseExpression syntaxCaseOf ->
            expressionCaseOf syntaxCaseOf

        Elm.Syntax.Expression.LambdaExpression syntaxLambda ->
            expressionLambda syntaxLambda

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
                                                    (Print.bumpIndentBy4
                                                        (Print.layoutPositiveIndent (lineOffsetInNode fieldValue)
                                                            |> Print.followedBy
                                                                (expressionNotParenthesized fieldValue)
                                                        )
                                                    )
                                        )
                                )
                            )
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
                                        (\element -> Print.bumpIndentBy2 (expressionNotParenthesized element))
                                )
                            )
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
                |> Print.followedBy (Print.layoutPositiveIndent (lineOffsetInRange fullRange))
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
                                                    |> Print.followedBy (Print.symbol "=")
                                                    |> Print.followedBy
                                                        (Print.bumpIndentBy4
                                                            (Print.layoutPositiveIndent (lineOffsetInNode fieldValue)
                                                                |> Print.followedBy (expressionNotParenthesized fieldValue)
                                                            )
                                                        )
                                            )
                                    )
                                )
                        )
                    )
                |> Print.followedBy (Print.symbol "}")

        Elm.Syntax.Expression.GLSLExpression glsl ->
            Print.symbol glsl


expressionLambda : Elm.Syntax.Expression.Lambda -> Print
expressionLambda syntaxLambda =
    let
        resultPrint : Print
        resultPrint =
            expressionNotParenthesized syntaxLambda.expression
    in
    Print.symbol "\\"
        |> Print.followedBy Print.space
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
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent (Print.lineOffset resultPrint))
            )
        |> Print.followedBy resultPrint


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

        onTrueLineOffset : Print.LineOffset
        onTrueLineOffset =
            Print.lineOffset onTruePrint

        onFalsePrint : Print
        onFalsePrint =
            expressionNotParenthesized onFalse

        onFalseLineOffset : Print.LineOffset
        onFalseLineOffset =
            Print.lineOffset onFalsePrint
    in
    Print.symbol "if"
        |> Print.followedBy
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent conditionLineOffset
                    |> Print.followedBy conditionPrint
                    |> Print.followedBy (Print.layoutPositiveIndent conditionLineOffset)
                )
            )
        |> Print.followedBy (Print.symbol "then")
        |> Print.followedBy
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent onTrueLineOffset
                    |> Print.followedBy onTruePrint
                    |> Print.followedBy (Print.layoutPositiveIndent onTrueLineOffset)
                )
            )
        |> Print.followedBy (Print.symbol "else")
        |> Print.followedBy
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent onFalseLineOffset
                    |> Print.followedBy onFalsePrint
                    |> Print.followedBy (Print.layoutPositiveIndent onFalseLineOffset)
                )
            )


expressionCaseOf : Elm.Syntax.Expression.CaseBlock -> Print
expressionCaseOf syntaxCaseOf =
    let
        casedExpressionLineOffset : Print.LineOffset
        casedExpressionLineOffset =
            lineOffsetInNode syntaxCaseOf.expression
    in
    Print.symbol "case"
        |> Print.followedBy
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent casedExpressionLineOffset
                    |> Print.followedBy (expressionNotParenthesized syntaxCaseOf.expression)
                )
            )
        |> Print.followedBy
            (Print.layoutPositiveIndent casedExpressionLineOffset)
        |> Print.followedBy (Print.symbol "of")
        |> Print.followedBy
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent Print.NextLine
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxCaseOf.cases
                                |> List.map case_
                                |> List.intersperse Print.linebreak
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
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent Print.NextLine
                    |> Print.followedBy (expressionNotParenthesized caseResult)
                )
            )


expressionLetIn : Elm.Syntax.Expression.LetBlock -> Print
expressionLetIn syntaxLetIn =
    Print.symbol "let"
        |> Print.followedBy
            (Print.bumpIndentBy4
                (Print.layoutPositiveIndent Print.NextLine
                    |> Print.followedBy
                        (Print.inSequence
                            (syntaxLetIn.declarations
                                |> List.map
                                    (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                        expressionLetDeclaration letDeclaration
                                    )
                                |> List.intersperse
                                    (Print.linebreak
                                        |> Print.followedBy (Print.layoutPositiveIndent Print.NextLine)
                                    )
                            )
                        )
                )
            )
        |> Print.followedBy (Print.layoutPositiveIndent Print.NextLine)
        |> Print.followedBy (Print.symbol "in")
        |> Print.followedBy (Print.layoutPositiveIndent Print.NextLine)
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
                                |> Print.followedBy Print.linebreak
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
                    (Print.bumpIndentBy4
                        (Print.layoutPositiveIndent Print.NextLine
                            |> Print.followedBy (expressionNotParenthesized destructuredExpression)
                        )
                    )


expressionParenthesizedIfSpaceSeparated : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionParenthesizedIfSpaceSeparated expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> Elm.Syntax.Node.value) then
        let
            expressionPrint : Print
            expressionPrint =
                expressionNotParenthesized expressionNode
        in
        Print.symbol "("
            |> Print.followedBy (Print.bumpIndentBy1 expressionPrint)
            |> Print.followedBy
                (Print.emptiableLayoutPositiveIndent (Print.lineOffset expressionPrint))
            |> Print.followedBy (Print.symbol ")")

    else
        expressionNotParenthesized expressionNode


expressionParenthesizedIfSpaceSeparatedExceptNonParenthesizedOperation : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Print
expressionParenthesizedIfSpaceSeparatedExceptNonParenthesizedOperation expressionNode =
    let
        (Elm.Syntax.Node.Node _ syntaxExpression) =
            expressionNode
    in
    if expressionIsSpaceSeparated syntaxExpression && not (expressionIsParenthesizedOperation syntaxExpression) then
        let
            expressionPrint : Print
            expressionPrint =
                expressionNotParenthesized expressionNode
        in
        Print.symbol "("
            |> Print.followedBy (Print.bumpIndentBy1 expressionPrint)
            |> Print.followedBy
                (Print.emptiableLayoutPositiveIndent (Print.lineOffset expressionPrint))
            |> Print.followedBy (Print.symbol ")")

    else
        expressionNotParenthesized expressionNode


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
