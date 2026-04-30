module ElmFormatParser exposing
    ( Parser, module_, run
    , moduleHeader, import_, declaration
    , type_, pattern, expression
    )

{-| Like [`Elm.Parser`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Parser)
but able to parse badly indented code and similar somewhat incorrect syntax,
similar to elm-format.
Just like [`ElmSyntaxParserLenient`](ElmSyntaxParserLenient) but producing
[`ElmFormatSyntax`](ElmFormatSyntax) instead of `elm-syntax`,
which allows more authentically recreating formatted code.

@docs Parser, module_, run

Sometimes it's useful to parse only some part of the syntax,
to, say, display only an expression in an article
or reparse only the touched declarations on save.

@docs moduleHeader, import_, declaration
@docs type_, pattern, expression

-}

import Char.Extra
import Elm.Syntax.Range
import ElmFormatSyntax
import ParserLenient


{-| Can turn a String into syntax or Nothing.
See [`ElmSyntaxParserLenient.run`](#run)

(This is not related to [`elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/).
[Open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)
if you need a way to covert to that)

-}
type alias Parser a =
    ParserLenient.Parser a


{-| Turn a given source String into `Just` the parsed syntax
or `Nothing` if any unrecognizable part is found.
-}
run : Parser a -> String -> Maybe a
run syntaxParser source =
    ParserLenient.run syntaxParser source


{-| [`Parser`](#Parser) for the space between syntax tokens
which can contain spaces, linebreaks, block and line comments
-}
whitespaceAndComments : Parser (List ElmFormatSyntax.Comment)
whitespaceAndComments =
    ParserLenient.skipWhileWhitespaceBacktrackableFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserLenient.offsetSourceAndThenOrSucceed
            (\offset source ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to empty
                        justFromSingleLineCommentNode

                    "{-" ->
                        justFromMultilineCommentNodeOrEmptyOnProblem

                    _ ->
                        Nothing
            )
            []
        )


{-| [`Parser`](#Parser) for the space between syntax tokens
which can contain spaces, linebreaks, block and line comments
-}
whitespaceAndCommasAndComments : Parser (List ElmFormatSyntax.Comment)
whitespaceAndCommasAndComments =
    ParserLenient.skipWhileWhitespaceOrCommaBacktrackableFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserLenient.offsetSourceAndThenOrSucceed
            (\offset source ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to empty
                        justFromSingleLineCommentNode

                    "{-" ->
                        justFromMultilineCommentNodeOrEmptyOnProblem

                    _ ->
                        Nothing
            )
            []
        )


symbolStartFollowedByWhitespaceAndComments : String -> ParserLenient.Parser ElmFormatSyntax.StartAndCommentsAfter
symbolStartFollowedByWhitespaceAndComments symbol =
    ParserLenient.symbolStartFollowedBy
        (\start commentsAfter -> { start = start, commentsAfter = commentsAfter })
        symbol
        whitespaceAndComments


symbolMapStartFollowedByWhitespaceAndComments :
    (ElmFormatSyntax.StartAndCommentsAfter -> result)
    -> String
    -> ParserLenient.Parser result
symbolMapStartFollowedByWhitespaceAndComments toResult symbol =
    ParserLenient.symbolStartFollowedBy
        (\start commentsAfter ->
            toResult { start = start, commentsAfter = commentsAfter }
        )
        symbol
        whitespaceAndComments


symbolStartFollowedByWhitespaceAndCommasAndComments : String -> ParserLenient.Parser ElmFormatSyntax.StartAndCommentsAfter
symbolStartFollowedByWhitespaceAndCommasAndComments symbol =
    ParserLenient.symbolStartFollowedBy
        (\start commentsAfter -> { start = start, commentsAfter = commentsAfter })
        symbol
        whitespaceAndCommasAndComments


keywordStartFollowedByWhitespaceAndComments : String -> ParserLenient.Parser { start : Elm.Syntax.Range.Location, commentsAfter : List ElmFormatSyntax.Comment }
keywordStartFollowedByWhitespaceAndComments keyword =
    ParserLenient.keywordStartFollowedBy
        (\start commentsAfter -> { start = start, commentsAfter = commentsAfter })
        keyword
        whitespaceAndComments


parseListWhileSucceeds : Parser a -> ParserLenient.Parser (List a)
parseListWhileSucceeds element =
    ParserLenient.loopWhileSucceeds element [] (::) List.reverse


moduleName : Parser String
moduleName =
    dotSeparatedNameUppercase


{-| [`Parser`](#Parser) for an dot-separated uppercase name.
Useful for qualification and actual module names.
Be aware that this is not as strict as the compiler as it e.g. allows A..A and A.b.
Do not use for parsing qualification-like names
-}
dotSeparatedNameUppercase : Parser String
dotSeparatedNameUppercase =
    ParserLenient.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        (\c ->
            case c of
                '.' ->
                    True

                _ ->
                    c |> Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        )


{-| [`Parser`](#Parser) for an dot-separated uppercase name.
Useful for qualification and actual module names.
Be aware that this is not as strict as the compiler as it e.g. allows A..A and A.b.
Do not use for parsing qualification-like names
-}
dotSeparatedName : Parser String
dotSeparatedName =
    ParserLenient.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLatinLetter
        (\c ->
            case c of
                '.' ->
                    True

                _ ->
                    c |> Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        )
        isNotReserved


justFromSingleLineCommentNode : Maybe (Parser (List ElmFormatSyntax.Comment))
justFromSingleLineCommentNode =
    Just fromSingleLineComment


justFromMultilineCommentNodeOrEmptyOnProblem : Maybe (Parser (List ElmFormatSyntax.Comment))
justFromMultilineCommentNodeOrEmptyOnProblem =
    Just fromMultilineCommentNodeOrEmptyOnProblem


fromMultilineCommentNodeOrEmptyOnProblem : Parser (List ElmFormatSyntax.Comment)
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserLenient.map2OrSucceed
        (\comment commentsAfter ->
            comment :: commentsAfter
        )
        (multiLineComment
            |> ParserLenient.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        []


fromSingleLineComment : Parser (List ElmFormatSyntax.Comment)
fromSingleLineComment =
    ParserLenient.map2 (::)
        (singleLineComment
            |> ParserLenient.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


{-| [`Parser`](#Parser) for a `--...` comment
-}
singleLineComment : Parser ElmFormatSyntax.Comment
singleLineComment =
    ParserLenient.symbolFollowedBy "--"
        (ParserLenient.whileMapWithRange
            (\c ->
                case c of
                    '\u{000D}' ->
                        False

                    '\n' ->
                        False

                    _ ->
                        not (Char.Extra.isUtf16Surrogate c)
            )
            (\range content ->
                { start = { row = range.start.row, column = range.start.column - 2 }
                , end =
                    { row = range.start.row
                    , column = range.end.column
                    }
                , content = content
                , style = ElmFormatSyntax.CommentLine
                }
            )
        )


{-| [`Parser`](#Parser) for a `{-...-}` comment,
also verifying that it itself isn't a documentation comment
-}
multiLineComment : Parser ElmFormatSyntax.Comment
multiLineComment =
    ParserLenient.offsetSourceValidateFollowedBy
        (\offset source ->
            case String.slice (offset + 2) (offset + 3) source of
                "|" ->
                    False

                _ ->
                    True
        )
        (ParserLenient.nestableMultiCommentMapWithRange
            (\range content ->
                { start = range.start
                , style = ElmFormatSyntax.CommentBlock
                , content = content
                , end = range.end
                }
            )
            ( '{', "-" )
            ( '-', "}" )
        )


whitespaceAndCommentsOrEmptyLoop : Parser (List ElmFormatSyntax.Comment)
whitespaceAndCommentsOrEmptyLoop =
    ParserLenient.loopWhileSucceeds
        (ParserLenient.oneOf2
            singleLineComment
            multiLineComment
            |> ParserLenient.followedBySkipWhileWhitespace
        )
        []
        -- inefficient for a large number of comments
        (\right soFar -> soFar ++ [ right ])
        identity


{-| [`Parser`](#Parser) for a name used for
record field names and unqualified function/value references
-}
nameLowercase : Parser String
nameLowercase =
    ParserLenient.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


isNotReserved : String -> Bool
isNotReserved name =
    case name of
        "module" ->
            False

        "exposing" ->
            False

        "import" ->
            False

        "as" ->
            False

        "if" ->
            False

        "then" ->
            False

        "else" ->
            False

        "let" ->
            False

        "in" ->
            False

        "case" ->
            False

        "of" ->
            False

        "port" ->
            False

        --"infixr"
        --"infixl"
        "type" ->
            False

        -- "infix" Apparently this is not a reserved keyword
        -- "alias" Apparently this is not a reserved keyword
        "where" ->
            False

        _ ->
            True


{-| [`Parser`](#Parser) for a name used for
type names, variant names, record type alias constructor function names and module names
-}
nameUppercase : Parser String
nameUppercase =
    ParserLenient.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    ParserLenient.columnIndentAndThen
        (\column indent ->
            if
                (column > 1)
                    && (indent |> List.all (\nestedIndent -> column - nestedIndent /= 0))
            then
                nextParser

            else
                ParserLenient.problem
        )


{-| [`Parser`](#Parser) for a [`ModuleHeader`](ElmFormatSyntax#ModuleHeader)
-}
moduleHeader : Parser ElmFormatSyntax.ModuleHeader
moduleHeader =
    ParserLenient.oneOf3
        moduleHeaderPure
        moduleHeaderPort
        moduleHeaderEffect


moduleHeaderPure : Parser ElmFormatSyntax.ModuleHeader
moduleHeaderPure =
    ParserLenient.map2
        (\moduleKeyword name ->
            ElmFormatSyntax.ModuleHeaderPure
                { moduleKeyword = moduleKeyword
                , moduleName = name
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "module")
        (withStartFollowedByWhitespaceAndComments moduleName)


moduleHeaderPort : Parser ElmFormatSyntax.ModuleHeader
moduleHeaderPort =
    ParserLenient.map3
        (\portKeyword moduleKeyword name ->
            ElmFormatSyntax.ModuleHeaderPort
                { portKeyword = portKeyword
                , moduleKeyword = moduleKeyword
                , moduleName = name
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "port")
        (symbolStartFollowedByWhitespaceAndComments "module")
        (withStartFollowedByWhitespaceAndComments moduleName)


moduleHeaderEffect : Parser ElmFormatSyntax.ModuleHeader
moduleHeaderEffect =
    ParserLenient.map7
        (\effectKeywordStart moduleKeyword name whereKeyword openCurly effectEntries_ closedCurly ->
            ElmFormatSyntax.ModuleHeaderEffect
                { effectKeyword = effectKeywordStart
                , moduleKeyword = moduleKeyword
                , moduleName = name
                , whereKeyword = whereKeyword
                , openCurly = openCurly
                , effectEntries = effectEntries_
                , closedCurly = closedCurly
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "effect")
        (symbolStartFollowedByWhitespaceAndComments "module")
        (withStartFollowedByWhitespaceAndComments moduleName)
        (symbolStartFollowedByWhitespaceAndComments "where")
        (symbolStartFollowedByWhitespaceAndCommasAndComments "{")
        effectEntries
        (symbolStartFollowedByWhitespaceAndComments "}")


withStartFollowedByWhitespaceAndComments :
    ParserLenient.Parser value
    -> ParserLenient.Parser (ElmFormatSyntax.WithStartAndCommentsAfter value)
withStartFollowedByWhitespaceAndComments valueParser =
    ParserLenient.map2WithStartLocation
        (\start value commentsAfter ->
            { start = start, value = value, commentsAfter = commentsAfter }
        )
        valueParser
        whitespaceAndComments


mapWithStartFollowedByWhitespaceAndComments :
    (ElmFormatSyntax.WithStartAndCommentsAfter value -> result)
    -> ParserLenient.Parser value
    -> ParserLenient.Parser result
mapWithStartFollowedByWhitespaceAndComments toResult valueParser =
    ParserLenient.map2WithStartLocation
        (\start value commentsAfter ->
            toResult { start = start, value = value, commentsAfter = commentsAfter }
        )
        valueParser
        whitespaceAndComments


effectEntries : Parser ElmFormatSyntax.EffectsEntries
effectEntries =
    ParserLenient.oneOf2Map
        ElmFormatSyntax.OnlySubscription
        (effectEntry "subscription")
        Basics.identity
        (ParserLenient.map2
            (\command maybeSubscription ->
                ElmFormatSyntax.CommandAnd
                    { command = command
                    , subscription = maybeSubscription
                    }
            )
            (effectEntry "command")
            (ParserLenient.map2OrSucceed
                (\comma entry -> Just { comma = comma, entry = entry })
                (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
                (effectEntry "subscription")
                Nothing
            )
        )


effectEntry : String -> Parser ElmFormatSyntax.EffectEntry
effectEntry entryKeyword =
    ParserLenient.map3
        (\keyword equals name ->
            { keyword = keyword
            , equals = equals
            , name = name
            }
        )
        (symbolStartFollowedByWhitespaceAndComments entryKeyword)
        (symbolStartFollowedByWhitespaceAndComments "=")
        (withStartFollowedByWhitespaceAndComments nameUppercase)


{-| [`Parser`](#Parser) for an [`Import`](ElmFormatSyntax#Import)
-}
import_ : Parser ElmFormatSyntax.Import
import_ =
    ParserLenient.map4
        (\importKeyword name maybeAs maybeExposing ->
            { importKeyword = importKeyword
            , name = name
            , as_ = maybeAs
            , exposing_ = maybeExposing
            }
        )
        (keywordStartFollowedByWhitespaceAndComments "import")
        (withStartFollowedByWhitespaceAndComments moduleName)
        (ParserLenient.map2OrSucceed
            (\asKeyword name -> Just { asKeyword = asKeyword, name = name })
            (keywordStartFollowedByWhitespaceAndComments "as")
            (withStartFollowedByWhitespaceAndComments nameUppercase)
            Nothing
        )
        (ParserLenient.mapOrSucceed Just exposing_ Nothing)


exposing_ : Parser ElmFormatSyntax.Exposing
exposing_ =
    ParserLenient.map4
        (\exposingKeyword openParen exposed_ closedParen ->
            { exposingKeyword = exposingKeyword
            , openParen = openParen
            , exposed = exposed_
            , closedParen = closedParen
            }
        )
        (keywordStartFollowedByWhitespaceAndComments "exposing")
        (symbolStartFollowedByWhitespaceAndCommasAndComments "(")
        exposed
        (symbolStartFollowedByWhitespaceAndComments ")")


exposed : Parser ElmFormatSyntax.Exposed
exposed =
    ParserLenient.oneOf3Map
        ElmFormatSyntax.ExposedAll
        (symbolStartFollowedByWhitespaceAndComments "...")
        ElmFormatSyntax.ExposedAll
        (symbolStartFollowedByWhitespaceAndComments "..")
        ElmFormatSyntax.ExposedExplicit
        (commaSeparated expose)


{-| The element parser should include trailing whitespace and potential comments
-}
commaSeparated : Parser element -> Parser (ElmFormatSyntax.CommaSeparated element)
commaSeparated elementParser =
    ParserLenient.map2OrSucceed
        (\head tail -> Just { head = head, tail = tail })
        elementParser
        (tailingCommaSeparated elementParser)
        Nothing


tailingCommaSeparated : Parser element -> Parser (ElmFormatSyntax.TailingCommaSeparated element)
tailingCommaSeparated elementParser =
    parseListWhileSucceeds
        (ParserLenient.map2 (\comma element -> { comma = comma, element = element })
            (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
            elementParser
        )


{-| [`Parser`](#Parser) for a single [`Elm.Syntax.Exposing.TopLevelExpose`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#TopLevelExpose)
-}
expose : Parser ElmFormatSyntax.Expose
expose =
    ParserLenient.oneOf3Map
        ElmFormatSyntax.ExposeVariable
        (withStartFollowedByWhitespaceAndComments nameLowercase)
        ElmFormatSyntax.ExposeType
        exposeType
        ElmFormatSyntax.ExposeOperator
        exposeOperator


exposeOperator : Parser (ElmFormatSyntax.WithStartAndCommentsAfter String)
exposeOperator =
    withStartFollowedByWhitespaceAndComments
        (ParserLenient.symbolFollowedBy "("
            (ParserLenient.ifFollowedByWhileWithoutLinebreak
                (\c ->
                    case c of
                        ')' ->
                            False

                        '\n' ->
                            False

                        ' ' ->
                            False

                        _ ->
                            True
                )
                (\c ->
                    case c of
                        ')' ->
                            False

                        '\n' ->
                            False

                        ' ' ->
                            False

                        _ ->
                            True
                )
            )
            |> ParserLenient.followedBySymbol ")"
        )


exposeType :
    Parser
        { name : ElmFormatSyntax.WithStartAndCommentsAfter String
        , exposingAllVariants :
            Maybe
                { openParen : ElmFormatSyntax.StartAndCommentsAfter
                , dotDot : ElmFormatSyntax.StartAndCommentsAfter
                , closedParen : ElmFormatSyntax.StartAndCommentsAfter
                }
        }
exposeType =
    ParserLenient.map2
        (\typeName maybeExposingAllVariants ->
            { name = typeName
            , exposingAllVariants = maybeExposingAllVariants
            }
        )
        (withStartFollowedByWhitespaceAndComments nameUppercase)
        (ParserLenient.map3OrSucceed
            (\openParen dotDot closedParen ->
                Just
                    { openParen = openParen
                    , dotDot = dotDot
                    , closedParen = closedParen
                    }
            )
            (symbolStartFollowedByWhitespaceAndComments "(")
            (ParserLenient.oneOf2
                (symbolStartFollowedByWhitespaceAndComments "...")
                (symbolStartFollowedByWhitespaceAndComments "..")
            )
            (symbolStartFollowedByWhitespaceAndComments ")")
            Nothing
        )


{-| [`Parser`](#Parser) for a full [`Module`](ElmFormatSyntax#Module)
-}
module_ : Parser ElmFormatSyntax.Module
module_ =
    ParserLenient.map6
        (\commentsBeforeHeader parsedModuleHeader parsedExposing moduleDocumentation imports parsedDeclarations ->
            { commentsBeforeHeader = commentsBeforeHeader
            , header = parsedModuleHeader
            , exposing_ = parsedExposing
            , moduleDocumentation = moduleDocumentation
            , imports = imports
            , declarations = parsedDeclarations
            }
        )
        whitespaceAndComments
        (ParserLenient.mapOrSucceed Just moduleHeader Nothing)
        (ParserLenient.mapOrSucceed Just exposing_ Nothing)
        (ParserLenient.mapOrSucceed Just documentationComment Nothing)
        (parseListWhileSucceeds import_)
        (parseListWhileSucceeds declaration)


documentationComment : Parser (ElmFormatSyntax.WithStartAndCommentsAfter String)
documentationComment =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice these comments will have been parsed by a previous syntax parser
    ParserLenient.map2WithStartLocation
        (\start comment commentsAfter ->
            { start = start
            , value = comment |> String.dropLeft 1 -- |
            , commentsAfter = commentsAfter
            }
        )
        (ParserLenient.nestableMultiCommentMapWithRange (\_ value -> value)
            ( '{', "-" )
            ( '-', "}" )
        )
        whitespaceAndComments


{-| [`Parser`](#Parser) for a [`Declaration`](ElmFormatSyntax#Declaration)
(which includes potential documentation)
-}
declaration : Parser ElmFormatSyntax.Declaration
declaration =
    ParserLenient.oneOf2
        (ParserLenient.map
            (\parsedDeclaration ->
                { documentation = Nothing
                , declaration = parsedDeclaration
                }
            )
            undocumentedDeclaration
        )
        (ParserLenient.map2
            (\maybeDocumentation parsedDeclaration ->
                { documentation = Just maybeDocumentation
                , declaration = parsedDeclaration
                }
            )
            documentationComment
            undocumentedDeclaration
        )


undocumentedDeclaration : Parser ElmFormatSyntax.UndocumentedDeclaration
undocumentedDeclaration =
    ParserLenient.oneOf4
        variableDeclaration
        typeOrTypeAliasDeclaration
        portDeclaration
        infixDeclaration


portDeclaration : Parser ElmFormatSyntax.UndocumentedDeclaration
portDeclaration =
    ParserLenient.map4
        (\portKeyword startName colon parsedType ->
            ElmFormatSyntax.DeclarationPort
                { portKeyword = portKeyword
                , name = startName
                , colon = colon
                , type_ = parsedType
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "port")
        (withStartFollowedByWhitespaceAndComments nameLowercase)
        (symbolStartFollowedByWhitespaceAndComments ":")
        type_


variableDeclaration : Parser ElmFormatSyntax.UndocumentedDeclaration
variableDeclaration =
    ParserLenient.map5
        (\startName maybeSignature parameters equals parsedExpression ->
            ElmFormatSyntax.DeclarationVariable
                { name = startName
                , signature = maybeSignature
                , parameters = parameters
                , equals = equals
                , expression = parsedExpression
                }
        )
        (withStartFollowedByWhitespaceAndComments nameLowercaseNotInfixOrKeyword)
        (ParserLenient.map3OrSucceed
            (\colon parsedType implementationName ->
                Just
                    { colon = colon
                    , type_ = parsedType
                    , implementationName = implementationName
                    }
            )
            (symbolStartFollowedByWhitespaceAndComments ":")
            type_
            (withStartFollowedByWhitespaceAndComments nameLowercaseNotInfixOrKeyword)
            Nothing
        )
        (parseListWhileSucceeds patternNotSpaceSeparated)
        (ParserLenient.oneOf2
            (symbolStartFollowedByWhitespaceAndComments "=")
            (symbolStartFollowedByWhitespaceAndComments "->")
        )
        expression


nameLowercaseNotInfixOrKeyword : Parser String
nameLowercaseNotInfixOrKeyword =
    ParserLenient.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        (\name ->
            case name of
                "infix" ->
                    False

                nameNotInfix ->
                    nameNotInfix |> isNotReserved
        )


infixDeclaration : Parser ElmFormatSyntax.UndocumentedDeclaration
infixDeclaration =
    ParserLenient.map6
        (\infixKeyword direction precedence operator equals variable ->
            ElmFormatSyntax.DeclarationInfix
                { infixKeyword = infixKeyword
                , direction = direction
                , precedence = precedence
                , operator = operator
                , equals = equals
                , variable = variable
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "infix")
        (withStartFollowedByWhitespaceAndComments infixDirection)
        (withStartFollowedByWhitespaceAndComments
            (ParserLenient.whileAtLeast1WithoutLinebreak Char.isDigit)
        )
        (withStartFollowedByWhitespaceAndComments
            (ParserLenient.symbolFollowedBy "("
                (ParserLenient.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                    (\_ symbol -> symbol)
                    isOperatorSymbolCharAsString
                    isAllowedOperatorToken
                    ")"
                )
            )
        )
        (symbolStartFollowedByWhitespaceAndComments "=")
        (withStartFollowedByWhitespaceAndComments nameLowercase)


infixDirection : Parser ElmFormatSyntax.InfixDirection
infixDirection =
    ParserLenient.oneOf3
        (ParserLenient.symbol "left" ElmFormatSyntax.InfixDirectionLeft)
        (ParserLenient.symbol "non" ElmFormatSyntax.InfixDirectionNon)
        (ParserLenient.symbol "right" ElmFormatSyntax.InfixDirectionRight)


typeOrTypeAliasDeclaration : Parser ElmFormatSyntax.UndocumentedDeclaration
typeOrTypeAliasDeclaration =
    ParserLenient.map2
        (\typeKeyword aliasOrChoiceType ->
            case aliasOrChoiceType of
                AfterTypeAlias afterTypeAlias ->
                    ElmFormatSyntax.DeclarationTypeAlias
                        { typeKeyword = typeKeyword
                        , aliasKeyword = afterTypeAlias.aliasKeyword
                        , name = afterTypeAlias.name
                        , parameters = afterTypeAlias.parameters
                        , equals = afterTypeAlias.equals
                        , type_ = afterTypeAlias.type_
                        }

                AfterTypeChoice afterTypeChoice ->
                    ElmFormatSyntax.DeclarationChoiceType
                        { typeKeyword = typeKeyword
                        , name = afterTypeChoice.name
                        , parameters = afterTypeChoice.parameters
                        , equals = afterTypeChoice.equals
                        , headVariantName = afterTypeChoice.headVariantName
                        , headVariantValues = afterTypeChoice.headVariantValues
                        , tailVariants = afterTypeChoice.tailVariants
                        }
        )
        (symbolStartFollowedByWhitespaceAndComments "type")
        (ParserLenient.oneOf2
            (ParserLenient.map5
                (\aliasKeyword name parameters equals equivalentType ->
                    AfterTypeAlias
                        { aliasKeyword = aliasKeyword
                        , name = name
                        , parameters = parameters
                        , equals = equals
                        , type_ = equivalentType
                        }
                )
                (symbolStartFollowedByWhitespaceAndComments "alias")
                (withStartFollowedByWhitespaceAndComments nameUppercase)
                (parseListWhileSucceeds (withStartFollowedByWhitespaceAndComments nameLowercase))
                (symbolStartFollowedByWhitespaceAndComments "=")
                type_
            )
            (ParserLenient.map7
                (\name parameters equals commentsWithExtraBars headVariantName headVariantValues tailVariants ->
                    AfterTypeChoice
                        { name = name
                        , parameters = parameters
                        , equals =
                            { start = equals.start
                            , commentsAfter = equals.commentsAfter ++ commentsWithExtraBars
                            }
                        , headVariantName = headVariantName
                        , headVariantValues = headVariantValues
                        , tailVariants = tailVariants
                        }
                )
                (withStartFollowedByWhitespaceAndComments nameUppercase)
                (parseListWhileSucceeds (withStartFollowedByWhitespaceAndComments nameLowercase))
                (symbolStartFollowedByWhitespaceAndComments "=")
                (ParserLenient.loopWhileSucceeds
                    (ParserLenient.symbolFollowedBy "|" whitespaceAndComments)
                    []
                    (++)
                    Basics.identity
                )
                (withStartFollowedByWhitespaceAndComments nameUppercase)
                (parseListWhileSucceeds typeNotSpaceSeparated)
                (parseListWhileSucceeds
                    (ParserLenient.map4
                        (\bar commentsWithExtraBars name values ->
                            { bar =
                                { start = bar.start
                                , commentsAfter =
                                    bar.commentsAfter ++ commentsWithExtraBars
                                }
                            , name = name
                            , values = values
                            }
                        )
                        (symbolStartFollowedByWhitespaceAndCommasAndComments "|")
                        (ParserLenient.loopWhileSucceeds
                            (ParserLenient.symbolFollowedBy "|" whitespaceAndComments)
                            []
                            (++)
                            Basics.identity
                        )
                        (withStartFollowedByWhitespaceAndComments nameUppercase)
                        (parseListWhileSucceeds typeNotSpaceSeparated)
                    )
                )
            )
        )


type AfterTypeChoiceOrAliasDeclaration
    = AfterTypeAlias
        { aliasKeyword : ElmFormatSyntax.StartAndCommentsAfter
        , name : ElmFormatSyntax.WithStartAndCommentsAfter String
        , parameters : List (ElmFormatSyntax.WithStartAndCommentsAfter String)
        , equals : ElmFormatSyntax.StartAndCommentsAfter
        , type_ : ElmFormatSyntax.Type
        }
    | AfterTypeChoice
        { name : ElmFormatSyntax.WithStartAndCommentsAfter String
        , parameters : List (ElmFormatSyntax.WithStartAndCommentsAfter String)
        , equals : ElmFormatSyntax.StartAndCommentsAfter
        , headVariantName : ElmFormatSyntax.WithStartAndCommentsAfter String
        , headVariantValues : List ElmFormatSyntax.Type
        , tailVariants :
            List
                { bar : ElmFormatSyntax.StartAndCommentsAfter
                , name : ElmFormatSyntax.WithStartAndCommentsAfter String
                , values : List ElmFormatSyntax.Type
                }
        }


{-| [`Parser`](#Parser) for a potentially space-separated [`Type`](ElmFormatSyntax#Type).
Use for type signatures but not for types that need to be tightly grouped like in type arguments.
-}
type_ : Parser ElmFormatSyntax.Type
type_ =
    ParserLenient.map2
        (\startType arrowParts ->
            case arrowParts of
                [] ->
                    startType

                arrowPart0 :: arrowPart1Up ->
                    ElmFormatSyntax.TypeFunction
                        { parameter0 = startType
                        , arrowPart0 = arrowPart0
                        , arrowParts1Up = arrowPart1Up
                        }
        )
        (ParserLenient.lazy (\() -> typeNotFunction))
        (parseListWhileSucceeds
            (ParserLenient.map3
                (\arrow commentsWithExtraArrows part ->
                    { arrow =
                        { start = arrow.start
                        , commentsAfter = arrow.commentsAfter ++ commentsWithExtraArrows
                        }
                    , part = part
                    }
                )
                (ParserLenient.oneOf2
                    (symbolStartFollowedByWhitespaceAndComments "->")
                    (symbolStartFollowedByWhitespaceAndComments "=>")
                )
                (ParserLenient.loopWhileSucceeds
                    (ParserLenient.oneOf2
                        (ParserLenient.symbolFollowedBy "->" whitespaceAndCommasAndComments)
                        (ParserLenient.symbolFollowedBy "=>" whitespaceAndCommasAndComments)
                    )
                    []
                    (++)
                    Basics.identity
                )
                (ParserLenient.lazy (\() -> typeNotFunction))
            )
        )


typeNotFunction : Parser ElmFormatSyntax.Type
typeNotFunction =
    ParserLenient.oneOf5
        typeUnit
        typeParenthesizedOrTupleOrTriple
        typeConstruct
        typeVariable
        typeRecord


typeNotSpaceSeparated : Parser ElmFormatSyntax.Type
typeNotSpaceSeparated =
    ParserLenient.oneOf5
        typeUnit
        typeParenthesizedOrTupleOrTriple
        typeConstructWithoutArguments
        typeVariable
        typeRecord


typeVariable : Parser ElmFormatSyntax.Type
typeVariable =
    positivelyIndentedFollowedBy
        (mapWithStartFollowedByWhitespaceAndComments
            ElmFormatSyntax.TypeVariable
            nameLowercase
        )


typeConstructWithoutArguments : Parser ElmFormatSyntax.Type
typeConstructWithoutArguments =
    mapWithStartFollowedByWhitespaceAndComments
        (\name -> ElmFormatSyntax.TypeConstruct { name = name, arguments = [] })
        dotSeparatedNameUppercase


typeConstruct : Parser ElmFormatSyntax.Type
typeConstruct =
    ParserLenient.map2
        (\name arguments -> ElmFormatSyntax.TypeConstruct { name = name, arguments = arguments })
        (withStartFollowedByWhitespaceAndComments dotSeparatedNameUppercase)
        (parseListWhileSucceeds typeNotSpaceSeparated)


typeRecord : Parser ElmFormatSyntax.Type
typeRecord =
    ParserLenient.offsetSourceValidateFollowedBy
        (\offset source ->
            case source |> String.slice (offset + 1) (offset + 2) of
                "-" ->
                    -- documentation comment
                    False

                _ ->
                    True
        )
        (ParserLenient.map3
            (\openCurly maybeInner closedCurly ->
                ElmFormatSyntax.TypeRecord
                    { openCurly = openCurly
                    , inner = maybeInner
                    , closedCurly = closedCurly
                    }
            )
            (symbolStartFollowedByWhitespaceAndCommasAndComments "{")
            (ParserLenient.orSucceed
                (ParserLenient.map2
                    (\firstName inner ->
                        Just
                            { firstName = firstName
                            , inner = inner
                            }
                    )
                    (withStartFollowedByWhitespaceAndComments nameLowercase)
                    (ParserLenient.oneOf2Map
                        ElmFormatSyntax.TypeRecordStartExtension
                        (ParserLenient.map2
                            (\bar fields -> { bar = bar, fields = fields })
                            (symbolStartFollowedByWhitespaceAndCommasAndComments "|")
                            (commaSeparated typeField)
                        )
                        ElmFormatSyntax.TypeRecordStartField
                        (ParserLenient.map3
                            (\colon value tailingFields ->
                                { firstColon = colon
                                , firstValue = value
                                , tailingFields = tailingFields
                                }
                            )
                            (ParserLenient.oneOf2
                                (symbolStartFollowedByWhitespaceAndComments ":")
                                (symbolStartFollowedByWhitespaceAndComments "=")
                            )
                            type_
                            (tailingCommaSeparated typeField)
                        )
                    )
                )
                Nothing
            )
            (symbolStartFollowedByWhitespaceAndComments "}")
        )


typeField : ParserLenient.Parser { name : ElmFormatSyntax.WithStartAndCommentsAfter String, colon : { start : Elm.Syntax.Range.Location, commentsAfter : List ElmFormatSyntax.Comment }, value : ElmFormatSyntax.Type }
typeField =
    ParserLenient.map3
        (\name colon value -> { name = name, colon = colon, value = value })
        (withStartFollowedByWhitespaceAndComments nameLowercase)
        (ParserLenient.oneOf2
            (symbolStartFollowedByWhitespaceAndComments ":")
            (symbolStartFollowedByWhitespaceAndComments "=")
        )
        type_


typeUnit : Parser ElmFormatSyntax.Type
typeUnit =
    symbolMapStartFollowedByWhitespaceAndComments ElmFormatSyntax.TypeUnit "()"


typeParenthesizedOrTupleOrTriple : Parser ElmFormatSyntax.Type
typeParenthesizedOrTupleOrTriple =
    ParserLenient.map4
        (\openParen part0 maybePart1Up closedParen ->
            case maybePart1Up of
                Nothing ->
                    ElmFormatSyntax.TypeParenthesized
                        { openParen = openParen
                        , inner = part0
                        , closedParen = closedParen
                        }

                Just part1Up ->
                    case part1Up.part2 of
                        Nothing ->
                            ElmFormatSyntax.TypeTuple
                                { openParen = openParen
                                , part0 = part0
                                , comma0 = part1Up.comma0
                                , part1 = part1Up.part1
                                , closedParen = closedParen
                                }

                        Just part2 ->
                            ElmFormatSyntax.TypeTriple
                                { openParen = openParen
                                , part0 = part0
                                , comma0 = part1Up.comma0
                                , part1 = part1Up.part1
                                , comma1 = part2.comma1
                                , part2 = part2.part2
                                , closedParen = closedParen
                                }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "(")
        type_
        (ParserLenient.map3OrSucceed
            (\comma0 part1 part2Up ->
                Just { comma0 = comma0, part1 = part1, part2 = part2Up }
            )
            (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
            type_
            (ParserLenient.map2OrSucceed
                (\comma1 part2 ->
                    Just { comma1 = comma1, part2 = part2 }
                )
                (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
                type_
                Nothing
            )
            Nothing
        )
        (symbolStartFollowedByWhitespaceAndComments ")")


{-| [`Parser`](#Parser) for a potentially space-separated [`Expression`](ElmFormatSyntax#Expression).
Use for declaration results but not for expressions that need to be tightly grouped like in arguments.
-}
expression : Parser ElmFormatSyntax.Expression
expression =
    extendedSubExpression
        { afterCommitting = .extensionRightParser
        , validateRightPrecedence = \_ -> True
        }


extendedSubExpression :
    { afterCommitting : InfixOperatorInfo -> Parser ExtensionRight
    , validateRightPrecedence : InfixOperatorInfo -> Bool
    }
    -> Parser ElmFormatSyntax.Expression
extendedSubExpression info =
    ParserLenient.loopWhileSucceedsOntoResultFromParser
        (infixOperatorAndThen info)
        expressionCallOrNotSpaceSeparated
        applyExtensionRight
        Basics.identity


infixOperatorAndThen :
    { afterCommitting : InfixOperatorInfo -> Parser ExtensionRight
    , validateRightPrecedence : InfixOperatorInfo -> Bool
    }
    -> Parser ExtensionRight
infixOperatorAndThen extensionRightConstraints =
    let
        toResult : InfixOperatorInfo -> Maybe InfixOperatorInfo
        toResult info =
            if info |> extensionRightConstraints.validateRightPrecedence then
                Just info

            else
                Nothing

        apRResult : Maybe InfixOperatorInfo
        apRResult =
            toResult precedence1ApR

        appendResult : Maybe InfixOperatorInfo
        appendResult =
            toResult precedence5append

        apLResult : Maybe InfixOperatorInfo
        apLResult =
            toResult precedence1ApL

        composeRResult : Maybe InfixOperatorInfo
        composeRResult =
            toResult precedence9ComposeR

        eqResult : Maybe InfixOperatorInfo
        eqResult =
            toResult precedence4Eq

        mulResult : Maybe InfixOperatorInfo
        mulResult =
            toResult precedence7Mul

        consResult : Maybe InfixOperatorInfo
        consResult =
            toResult precedence5Cons

        addResult : Maybe InfixOperatorInfo
        addResult =
            toResult precedence6Add

        subResult : Maybe InfixOperatorInfo
        subResult =
            toResult precedence6Sub

        ignoreResult : Maybe InfixOperatorInfo
        ignoreResult =
            toResult precedence6Ignore

        andResult : Maybe InfixOperatorInfo
        andResult =
            toResult precedence3And

        keepResult : Maybe InfixOperatorInfo
        keepResult =
            toResult precedence5Keep

        composeLResult : Maybe InfixOperatorInfo
        composeLResult =
            toResult precedence9ComposeL

        neqResult : Maybe InfixOperatorInfo
        neqResult =
            toResult precedence4Neq

        idivResult : Maybe InfixOperatorInfo
        idivResult =
            toResult precedence7Idiv

        fdivResult : Maybe InfixOperatorInfo
        fdivResult =
            toResult precedence7Fdiv

        slashResult : Maybe InfixOperatorInfo
        slashResult =
            toResult precedence7Slash

        orResult : Maybe InfixOperatorInfo
        orResult =
            toResult precedence2Or

        leResult : Maybe InfixOperatorInfo
        leResult =
            toResult precedence4Le

        geResult : Maybe InfixOperatorInfo
        geResult =
            toResult precedence4Ge

        gtResult : Maybe InfixOperatorInfo
        gtResult =
            toResult precedence4Gt

        questionMarkResult : Maybe InfixOperatorInfo
        questionMarkResult =
            toResult precedence8QuestionMark

        ltResult : Maybe InfixOperatorInfo
        ltResult =
            toResult precedence4Lt

        powResult : Maybe InfixOperatorInfo
        powResult =
            toResult precedence8Pow
    in
    ParserLenient.whileAtMost3WithoutLinebreakAnd2PartUtf16ToResultAndThen
        isOperatorSymbolCharAsString
        (\operator ->
            case operator of
                "|>" ->
                    apRResult

                "|" ->
                    apRResult

                "++" ->
                    appendResult

                "<|" ->
                    apLResult

                ">>" ->
                    composeRResult

                "==" ->
                    eqResult

                "===" ->
                    eqResult

                "*" ->
                    mulResult

                "::" ->
                    consResult

                "+" ->
                    addResult

                "-" ->
                    subResult

                "|." ->
                    ignoreResult

                "&&" ->
                    andResult

                "|=" ->
                    keepResult

                "<<" ->
                    composeLResult

                "/=" ->
                    neqResult

                "!=" ->
                    neqResult

                "!==" ->
                    neqResult

                "//" ->
                    idivResult

                "/" ->
                    fdivResult

                "</>" ->
                    slashResult

                "||" ->
                    orResult

                "<=" ->
                    leResult

                ">=" ->
                    geResult

                ">" ->
                    gtResult

                "<?>" ->
                    questionMarkResult

                "<" ->
                    ltResult

                "^" ->
                    powResult

                "**" ->
                    powResult

                _ ->
                    Nothing
        )
        extensionRightConstraints.afterCommitting


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo -> rightInfo.leftPrecedence > leftPrecedence
            }
    }


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo -> rightInfo.leftPrecedence >= leftPrecedence
            }
    }


infixNonAssociative : Int -> String -> InfixOperatorInfo
infixNonAssociative leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting =
                \rightInfo ->
                    if rightInfo.leftPrecedence - leftPrecedence == 0 then
                        ParserLenient.problem

                    else
                        rightInfo.extensionRightParser
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo -> rightInfo.leftPrecedence >= leftPrecedence
            }
    }


precedence1ApR : InfixOperatorInfo
precedence1ApR =
    infixLeft 1 "|>"


precedence1ApL : InfixOperatorInfo
precedence1ApL =
    infixRight 1 "<|"


precedence2Or : InfixOperatorInfo
precedence2Or =
    infixRight 2 "||"


precedence3And : InfixOperatorInfo
precedence3And =
    infixRight 3 "&&"


precedence4Eq : InfixOperatorInfo
precedence4Eq =
    infixNonAssociative 4 "=="


precedence4Neq : InfixOperatorInfo
precedence4Neq =
    infixNonAssociative 4 "/="


precedence4Le : InfixOperatorInfo
precedence4Le =
    infixNonAssociative 4 "<="


precedence4Ge : InfixOperatorInfo
precedence4Ge =
    infixNonAssociative 4 ">="


precedence4Gt : InfixOperatorInfo
precedence4Gt =
    infixNonAssociative 4 ">"


precedence4Lt : InfixOperatorInfo
precedence4Lt =
    infixNonAssociative 4 "<"


precedence5append : InfixOperatorInfo
precedence5append =
    infixRight 5 "++"


precedence5Cons : InfixOperatorInfo
precedence5Cons =
    infixRight 5 "::"


precedence5Keep : InfixOperatorInfo
precedence5Keep =
    infixLeft 5 "|="


precedence6Add : InfixOperatorInfo
precedence6Add =
    infixLeft 6 "+"


precedence6Sub : InfixOperatorInfo
precedence6Sub =
    infixLeft 6 "-"


precedence6Ignore : InfixOperatorInfo
precedence6Ignore =
    infixLeft 6 "|."


precedence7Idiv : InfixOperatorInfo
precedence7Idiv =
    infixLeft 7 "//"


precedence7Mul : InfixOperatorInfo
precedence7Mul =
    infixLeft 7 "*"


precedence7Fdiv : InfixOperatorInfo
precedence7Fdiv =
    infixLeft 7 "/"


precedence7Slash : InfixOperatorInfo
precedence7Slash =
    infixRight 7 "</>"


precedence8QuestionMark : InfixOperatorInfo
precedence8QuestionMark =
    infixLeft 8 "<?>"


precedence8Pow : InfixOperatorInfo
precedence8Pow =
    infixRight 8 "^"


precedence9ComposeR : InfixOperatorInfo
precedence9ComposeR =
    infixRight 9 ">>"


precedence9ComposeL : InfixOperatorInfo
precedence9ComposeL =
    infixLeft 9 "<<"


applyExtensionRight : ExtensionRight -> ElmFormatSyntax.Expression -> ElmFormatSyntax.Expression
applyExtensionRight (ExtendRightByOperation operation) left =
    ElmFormatSyntax.ExpressionInfixOperation
        { left = left
        , operator = operation.operator
        , right = operation.expression
        }


expressionCallOrNotSpaceSeparated : Parser ElmFormatSyntax.Expression
expressionCallOrNotSpaceSeparated =
    ParserLenient.map2
        (\startExpression arguments ->
            case arguments of
                [] ->
                    startExpression

                argument0 :: argument1Up ->
                    ElmFormatSyntax.ExpressionCall
                        { called = startExpression
                        , argument0 = argument0
                        , argument1Up = argument1Up
                        }
        )
        (ParserLenient.lazy (\() -> expressionNotSpaceSeparated))
        (parseListWhileSucceeds
            (positivelyIndentedFollowedBy
                (ParserLenient.lazy (\() -> expressionNotSpaceSeparated))
            )
        )


extensionRightParser :
    { afterCommitting : InfixOperatorInfo -> Parser ExtensionRight
    , symbol : String
    , validateRightPrecedence : InfixOperatorInfo -> Bool
    }
    -> Parser ExtensionRight
extensionRightParser extensionRightInfo =
    ParserLenient.map2WithStartLocation
        (\locationAfterOperator whitespaceAndCommentsAfterOperator right ->
            ExtendRightByOperation
                { operator =
                    { start =
                        { row = locationAfterOperator.row
                        , column =
                            locationAfterOperator.column
                                - (extensionRightInfo.symbol |> String.length)
                        }
                    , commentsAfter = whitespaceAndCommentsAfterOperator
                    , value = extensionRightInfo.symbol
                    }
                , expression = right
                }
        )
        whitespaceAndComments
        (ParserLenient.lazy
            (\() ->
                extendedSubExpression
                    { afterCommitting = extensionRightInfo.afterCommitting
                    , validateRightPrecedence = extensionRightInfo.validateRightPrecedence
                    }
            )
        )


type ExtensionRight
    = ExtendRightByOperation
        { operator : ElmFormatSyntax.WithStartAndCommentsAfter String
        , expression : ElmFormatSyntax.Expression
        }


type alias InfixOperatorInfo =
    { leftPrecedence : Int
    , symbol : String
    , extensionRightParser : Parser ExtensionRight
    }


expressionNotSpaceSeparated : Parser ElmFormatSyntax.Expression
expressionNotSpaceSeparated =
    -- TODO optimize
    ParserLenient.oneOf9
        expressionReference
        expressionUnit
        expressionOperatorFunction
        -- expressionParenthesizedOrTupleOrTriple must be below operator function and ()
        expressionParenthesizedOrTupleOrTriple
        expressionString
        expressionCaseOf
        expressionIfThenElse
        expressionLetIn
        (ParserLenient.oneOf7
            expressionRecord
            expressionNumber
            expressionGlsl
            -- list must be below glsl
            expressionList
            expressionLambda
            expressionRecordAccessFunction
            (ParserLenient.oneOf2
                expressionChar
                expressionNegation
            )
        )


expressionUnit : Parser ElmFormatSyntax.Expression
expressionUnit =
    symbolMapStartFollowedByWhitespaceAndComments ElmFormatSyntax.ExpressionUnit "()"


expressionOperatorFunction : Parser ElmFormatSyntax.Expression
expressionOperatorFunction =
    ParserLenient.offsetSourceValidateFollowedBy
        (\offset source ->
            case source |> String.slice offset (offset + 1) of
                "(" ->
                    case source |> String.slice (offset + 1) (offset + 2) of
                        "." ->
                            -- isOperatorSymbolCharAsString contains .
                            -- which would be ambiguous with (.fieldAccess)
                            False

                        "-" ->
                            -- isOperatorSymbolCharAsString contains -
                            -- which would be ambiguous with (-negated)
                            case source |> String.slice (offset + 2) (offset + 3) of
                                ")" ->
                                    True

                                _ ->
                                    False

                        symbolAfterOpenParen ->
                            symbolAfterOpenParen |> isOperatorSymbolCharAsString

                _ ->
                    False
        )
        (ParserLenient.map2WithStartLocation
            (\openParenStart operator commentsAfterClosedParen ->
                ElmFormatSyntax.ExpressionOperatorFunction
                    { openParenStart = openParenStart
                    , operator = operator
                    , closedParen =
                        { start =
                            { row = openParenStart.row
                            , column = openParenStart.column + 1 + (operator |> String.length)
                            }
                        , commentsAfter = commentsAfterClosedParen
                        }
                    }
            )
            (ParserLenient.symbolFollowedBy "("
                (ParserLenient.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                    (\_ symbol -> symbol)
                    isOperatorSymbolCharAsString
                    isAllowedOperatorToken
                    ")"
                )
            )
            whitespaceAndComments
        )


expressionString : Parser ElmFormatSyntax.Expression
expressionString =
    ParserLenient.map ElmFormatSyntax.ExpressionString stringLiteral


expressionChar : Parser ElmFormatSyntax.Expression
expressionChar =
    ParserLenient.map ElmFormatSyntax.ExpressionChar charLiteral


expressionNumber : Parser ElmFormatSyntax.Expression
expressionNumber =
    ParserLenient.oneOf2Map
        ElmFormatSyntax.ExpressionHex
        (withStartFollowedByWhitespaceAndComments
            (ParserLenient.symbolFollowedBy "0x"
                (ParserLenient.whileAtLeast1WithoutLinebreak Char.isHexDigit)
            )
        )
        ElmFormatSyntax.ExpressionNumber
        (withStartFollowedByWhitespaceAndComments
            (ParserLenient.map2
                (\digits exp ->
                    digits ++ exp ++ ""
                )
                (ParserLenient.ifFollowedByWhileWithoutLinebreak
                    Char.isDigit
                    (\c ->
                        case c of
                            '.' ->
                                True

                            _ ->
                                c |> Char.isDigit
                    )
                )
                (ParserLenient.map2OrSucceed
                    (\sign expDigits -> "e" ++ sign ++ expDigits)
                    (ParserLenient.oneOf6
                        (ParserLenient.symbol "e+" "")
                        (ParserLenient.symbol "e-" "-")
                        (ParserLenient.symbol "e" "")
                        (ParserLenient.symbol "E+" "")
                        (ParserLenient.symbol "E-" "-")
                        (ParserLenient.symbol "E" "")
                    )
                    (ParserLenient.whileAtLeast1WithoutLinebreak Char.isDigit)
                    ""
                )
            )
        )


expressionReference : Parser ElmFormatSyntax.Expression
expressionReference =
    mapWithStartFollowedByWhitespaceAndComments
        ElmFormatSyntax.ExpressionReference
        dotSeparatedName


expressionRecordAccessFunction : Parser ElmFormatSyntax.Expression
expressionRecordAccessFunction =
    mapWithStartFollowedByWhitespaceAndComments
        ElmFormatSyntax.ExpressionRecordAccessFunction
        (ParserLenient.symbolFollowedBy "."
            nameLowercase
        )


expressionNegation : Parser ElmFormatSyntax.Expression
expressionNegation =
    ParserLenient.offsetSourceValidateFollowedBy
        (\offset source ->
            (case source |> String.slice (offset + 1) (offset + 2) of
                " " ->
                    False

                "\n" ->
                    False

                "{" ->
                    case source |> String.slice (offset + 2) (offset + 3) of
                        "-" ->
                            False

                        _ ->
                            True

                _ ->
                    True
            )
                && (case source |> String.slice (offset - 1) offset of
                        " " ->
                            True

                        "\n" ->
                            True

                        "," ->
                            True

                        "=" ->
                            True

                        "(" ->
                            True

                        -- from ->
                        ">" ->
                            True

                        -- from in keyword of a let...in
                        "n" ->
                            case source |> String.slice (offset - 2) (offset - 1) of
                                "i" ->
                                    (source |> String.slice (offset - 3) (offset - 2))
                                        |> String.all (\char -> Basics.not (Char.Extra.isLatinAlphaNumOrUnderscoreFast char))

                                _ ->
                                    False

                        "}" ->
                            case source |> String.slice (offset - 2) (offset - 1) of
                                "-" ->
                                    True

                                _ ->
                                    False

                        _ ->
                            False
                   )
        )
        (ParserLenient.symbolStartFollowedBy
            (\minusStart inner ->
                ElmFormatSyntax.ExpressionNegation
                    { minusStart = minusStart
                    , inner = inner
                    }
            )
            "-"
            (ParserLenient.lazy (\() -> expressionNotSpaceSeparated))
        )


expressionList : Parser ElmFormatSyntax.Expression
expressionList =
    ParserLenient.map3
        (\openSquare elements closedSquare ->
            ElmFormatSyntax.ExpressionList
                { openSquare = openSquare
                , elements = elements
                , closedSquare = closedSquare
                }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "[")
        (commaSeparated expression)
        (symbolStartFollowedByWhitespaceAndComments "]")


expressionIfThenElse : Parser ElmFormatSyntax.Expression
expressionIfThenElse =
    ParserLenient.map6
        (\ifKeyword condition thenKeyword onTrue elseKeyword onFalse ->
            ElmFormatSyntax.ExpressionIfThenElse
                { ifKeyword = ifKeyword
                , condition = condition
                , thenKeyword = thenKeyword
                , onTrue = onTrue
                , elseKeyword = elseKeyword
                , onFalse = onFalse
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "if")
        expression
        (ParserLenient.oneOf2
            (symbolStartFollowedByWhitespaceAndComments "then")
            (symbolStartFollowedByWhitespaceAndComments "of")
        )
        expression
        (symbolStartFollowedByWhitespaceAndComments "else")
        expression


expressionRecord : Parser ElmFormatSyntax.Expression
expressionRecord =
    ParserLenient.map3
        (\openCurly maybeInner closedCurly ->
            ElmFormatSyntax.ExpressionRecord
                { openCurly = openCurly
                , inner = maybeInner
                , closedCurly = closedCurly
                }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "{")
        (ParserLenient.orSucceed
            (ParserLenient.map2
                (\firstName inner ->
                    Just
                        { firstName = firstName
                        , inner = inner
                        }
                )
                (withStartFollowedByWhitespaceAndComments nameLowercase)
                (ParserLenient.oneOf2Map
                    ElmFormatSyntax.ExpressionRecordStartUpdate
                    (ParserLenient.map2
                        (\bar fields -> { bar = bar, fields = fields })
                        (symbolStartFollowedByWhitespaceAndCommasAndComments "|")
                        (commaSeparated expressionField)
                    )
                    ElmFormatSyntax.ExpressionRecordStartField
                    (ParserLenient.map3
                        (\equals value tailingFields ->
                            { firstEquals = equals
                            , firstValue = value
                            , tailingFields = tailingFields
                            }
                        )
                        (ParserLenient.oneOf2
                            (symbolStartFollowedByWhitespaceAndComments "=")
                            (symbolStartFollowedByWhitespaceAndComments ":")
                        )
                        expression
                        (tailingCommaSeparated expressionField)
                    )
                )
            )
            Nothing
        )
        (symbolStartFollowedByWhitespaceAndComments "}")


expressionField : Parser { name : ElmFormatSyntax.WithStartAndCommentsAfter String, equals : { start : Elm.Syntax.Range.Location, commentsAfter : List ElmFormatSyntax.Comment }, value : ElmFormatSyntax.Expression }
expressionField =
    ParserLenient.map3
        (\name equals value -> { name = name, equals = equals, value = value })
        (withStartFollowedByWhitespaceAndComments nameLowercase)
        (ParserLenient.oneOf2
            (symbolStartFollowedByWhitespaceAndComments "=")
            (symbolStartFollowedByWhitespaceAndComments ":")
        )
        expression


expressionCaseOf : Parser ElmFormatSyntax.Expression
expressionCaseOf =
    ParserLenient.map4
        (\caseKeyword matched ofKeyword cases ->
            ElmFormatSyntax.ExpressionCaseOf
                { caseKeyword = caseKeyword
                , matched = matched
                , ofKeyword = ofKeyword
                , cases = cases
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "case")
        expression
        (ParserLenient.oneOf2
            (symbolStartFollowedByWhitespaceAndComments "of")
            (symbolStartFollowedByWhitespaceAndComments "then")
        )
        (parseListWhileSucceeds
            (positivelyIndentedFollowedBy
                (ParserLenient.withIndentSetToColumn
                    (ParserLenient.map3
                        (\casePattern arrow result ->
                            { pattern = casePattern, arrow = arrow, result = result }
                        )
                        pattern
                        (ParserLenient.oneOf2
                            (symbolStartFollowedByWhitespaceAndComments "->")
                            (symbolStartFollowedByWhitespaceAndComments "=>")
                        )
                        expression
                    )
                )
            )
        )


expressionLetIn : Parser ElmFormatSyntax.Expression
expressionLetIn =
    ParserLenient.withIndentSetToColumn
        (ParserLenient.map4
            (\letKeyword letDeclarations inKeyword result ->
                ElmFormatSyntax.ExpressionLetIn
                    { letKeyword = letKeyword
                    , declarations = letDeclarations
                    , inKeyword = inKeyword
                    , result = result
                    }
            )
            (keywordStartFollowedByWhitespaceAndComments "let")
            (parseListWhileSucceeds letDeclaration)
            (symbolStartFollowedByWhitespaceAndComments "in")
            expression
        )


letDeclaration : Parser ElmFormatSyntax.LetDeclaration
letDeclaration =
    ParserLenient.withIndentSetToColumn
        (ParserLenient.oneOf2
            (ParserLenient.map5
                (\startName maybeSignature parameters equals parsedExpression ->
                    ElmFormatSyntax.LetVariable
                        { name = startName
                        , signature = maybeSignature
                        , parameters = parameters
                        , equals = equals
                        , expression = parsedExpression
                        }
                )
                (withStartFollowedByWhitespaceAndComments nameLowercase)
                (ParserLenient.map3OrSucceed
                    (\colon parsedType implementationName ->
                        Just
                            { colon = colon
                            , type_ = parsedType
                            , implementationName = implementationName
                            }
                    )
                    (symbolStartFollowedByWhitespaceAndComments ":")
                    type_
                    (withStartFollowedByWhitespaceAndComments nameLowercaseNotInfixOrKeyword)
                    Nothing
                )
                (parseListWhileSucceeds patternNotSpaceSeparated)
                (ParserLenient.oneOf2
                    (symbolStartFollowedByWhitespaceAndComments "=")
                    (symbolStartFollowedByWhitespaceAndComments "->")
                )
                expression
            )
            (ParserLenient.map3
                (\casePattern equals destructuredExpression ->
                    ElmFormatSyntax.LetDestructuring
                        { pattern = casePattern
                        , equals = equals
                        , expression = destructuredExpression
                        }
                )
                -- consider being more lenient here than the elm compiler and use pattern
                patternNotSpaceSeparated
                (symbolStartFollowedByWhitespaceAndComments "=")
                expression
            )
        )


expressionGlsl : Parser ElmFormatSyntax.Expression
expressionGlsl =
    ParserLenient.map2
        (\startGlsl afterStart ->
            ElmFormatSyntax.ExpressionGlsl
                { startGlsl = startGlsl
                , content = afterStart.content
                , endGlsl = afterStart.endGlsl
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "[glsl|")
        (ParserLenient.loopUntil
            (symbolStartFollowedByWhitespaceAndComments "|]")
            (ParserLenient.oneOf2
                (ParserLenient.symbol "|" "|")
                (ParserLenient.atLeastOneWhile
                    (\c ->
                        case c of
                            '|' ->
                                False

                            _ ->
                                True
                    )
                )
            )
            ""
            (\extension soFar ->
                soFar ++ extension ++ ""
            )
            (\end content -> { content = content, endGlsl = end })
        )


expressionLambda : Parser ElmFormatSyntax.Expression
expressionLambda =
    ParserLenient.map4
        (\backslash parameters arrow result ->
            ElmFormatSyntax.ExpressionLambda
                { backslash = backslash
                , parameters = parameters
                , arrow = arrow
                , result = result
                }
        )
        (symbolStartFollowedByWhitespaceAndComments "\\")
        (parseListWhileSucceeds patternNotSpaceSeparated)
        (ParserLenient.oneOf2
            (symbolStartFollowedByWhitespaceAndComments "->")
            (symbolStartFollowedByWhitespaceAndComments "=>")
        )
        expression


expressionParenthesizedOrTupleOrTriple : Parser ElmFormatSyntax.Expression
expressionParenthesizedOrTupleOrTriple =
    ParserLenient.map3
        (\openParen part0 maybePart1Up ->
            case maybePart1Up of
                Err afterInner ->
                    ElmFormatSyntax.ExpressionParenthesized
                        { openParen = openParen
                        , inner = part0
                        , closedParenStart = afterInner.closedParenStart
                        , fieldAccesses = afterInner.fieldAccesses
                        , commentsAfter = afterInner.commentsAfter
                        }

                Ok part1Up ->
                    case part1Up.part2 of
                        Nothing ->
                            ElmFormatSyntax.ExpressionTuple
                                { openParen = openParen
                                , part0 = part0
                                , comma0 = part1Up.comma0
                                , part1 = part1Up.part1
                                , closedParen = part1Up.closedParen
                                }

                        Just part2 ->
                            ElmFormatSyntax.ExpressionTriple
                                { openParen = openParen
                                , part0 = part0
                                , comma0 = part1Up.comma0
                                , part1 = part1Up.part1
                                , comma1 = part2.comma1
                                , part2 = part2.part2
                                , closedParen = part1Up.closedParen
                                }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "(")
        expression
        (ParserLenient.oneOf2
            (ParserLenient.map2WithStartLocation
                (\closedParenStart fieldAccesses commentsAfter ->
                    Err
                        { closedParenStart = closedParenStart
                        , fieldAccesses = fieldAccesses
                        , commentsAfter = commentsAfter
                        }
                )
                (ParserLenient.symbolFollowedBy ")"
                    (parseListWhileSucceeds
                        (ParserLenient.symbolFollowedBy "."
                            nameLowercase
                        )
                    )
                )
                whitespaceAndComments
            )
            (ParserLenient.map4
                (\comma0 part1 part2Up closedParen ->
                    Ok
                        { comma0 = comma0
                        , part1 = part1
                        , part2 = part2Up
                        , closedParen = closedParen
                        }
                )
                (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
                expression
                (ParserLenient.map2OrSucceed
                    (\comma1 part2 ->
                        Just { comma1 = comma1, part2 = part2 }
                    )
                    (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
                    expression
                    Nothing
                )
                (symbolStartFollowedByWhitespaceAndComments ")")
            )
        )


{-| [`Parser`](#Parser) for a potentially space-separated [`Pattern`](ElmFormatSyntax#Pattern).
Use for case patterns but not for patterns that need to be tightly grouped like in parameters.
-}
pattern : Parser ElmFormatSyntax.Pattern
pattern =
    ParserLenient.map3
        (\left consed maybeAsExtension ->
            let
                leftMaybeConsed : ElmFormatSyntax.Pattern
                leftMaybeConsed =
                    case consed of
                        [] ->
                            left

                        cons0 :: cons1Up ->
                            ElmFormatSyntax.PatternListCons
                                { head = left
                                , cons0 = cons0
                                , cons1Up = cons1Up
                                }
            in
            case maybeAsExtension of
                Nothing ->
                    leftMaybeConsed

                Just asExtension ->
                    ElmFormatSyntax.PatternAs
                        { pattern = leftMaybeConsed
                        , asKeyword = asExtension.asKeyword
                        , variable = asExtension.variable
                        }
        )
        (ParserLenient.lazy (\() -> patternNotAsOrCons))
        (parseListWhileSucceeds
            (ParserLenient.map2
                (\cons part ->
                    { cons = cons, part = part }
                )
                (symbolStartFollowedByWhitespaceAndComments "::")
                (ParserLenient.lazy (\() -> patternNotAsOrCons))
            )
        )
        (ParserLenient.map2OrSucceed
            (\asKeyword variable ->
                Just
                    { asKeyword = asKeyword
                    , variable = variable
                    }
            )
            (keywordStartFollowedByWhitespaceAndComments "as")
            (withStartFollowedByWhitespaceAndComments nameLowercase)
            Nothing
        )


patternNotAsOrCons : Parser ElmFormatSyntax.Pattern
patternNotAsOrCons =
    ParserLenient.oneOf9
        patternVariable
        patternIgnored
        patternVariant
        patternParens
        patternRecord
        patternString
        patternList
        patternNumber
        patternChar


patternNotSpaceSeparated : Parser ElmFormatSyntax.Pattern
patternNotSpaceSeparated =
    ParserLenient.oneOf9
        patternVariable
        variantWithoutValuesPattern
        patternIgnored
        patternParens
        patternRecord
        patternString
        patternList
        patternNumber
        patternChar


patternVariable : Parser ElmFormatSyntax.Pattern
patternVariable =
    mapWithStartFollowedByWhitespaceAndComments
        ElmFormatSyntax.PatternVariable
        nameLowercase


patternIgnored : ParserLenient.Parser ElmFormatSyntax.Pattern
patternIgnored =
    symbolMapStartFollowedByWhitespaceAndComments ElmFormatSyntax.PatternIgnored "_"


variantWithoutValuesPattern : Parser ElmFormatSyntax.Pattern
variantWithoutValuesPattern =
    mapWithStartFollowedByWhitespaceAndComments
        (\name -> ElmFormatSyntax.PatternVariant { name = name, values = [] })
        dotSeparatedNameUppercase


patternVariant : Parser ElmFormatSyntax.Pattern
patternVariant =
    ParserLenient.map2
        (\name values -> ElmFormatSyntax.PatternVariant { name = name, values = values })
        (withStartFollowedByWhitespaceAndComments dotSeparatedNameUppercase)
        (parseListWhileSucceeds patternNotSpaceSeparated)


patternNumber : Parser ElmFormatSyntax.Pattern
patternNumber =
    -- can be optimized
    ParserLenient.oneOf2Map
        ElmFormatSyntax.PatternHex
        (withStartFollowedByWhitespaceAndComments
            (ParserLenient.symbolFollowedBy "0x"
                (ParserLenient.whileAtLeast1WithoutLinebreak Char.isHexDigit)
            )
        )
        ElmFormatSyntax.PatternInt
        (withStartFollowedByWhitespaceAndComments
            (ParserLenient.whileAtLeast1WithoutLinebreak Char.isDigit)
        )


patternChar : Parser ElmFormatSyntax.Pattern
patternChar =
    ParserLenient.map ElmFormatSyntax.PatternChar charLiteral


patternString : Parser ElmFormatSyntax.Pattern
patternString =
    ParserLenient.map ElmFormatSyntax.PatternString stringLiteral


patternList : Parser ElmFormatSyntax.Pattern
patternList =
    ParserLenient.map3
        (\openSquare maybeElements closedSquare ->
            ElmFormatSyntax.PatternListExact
                { openSquare = openSquare
                , elements = maybeElements
                , closedSquare = closedSquare
                }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "[")
        (commaSeparated pattern)
        (symbolStartFollowedByWhitespaceAndComments "]")


patternRecord : Parser ElmFormatSyntax.Pattern
patternRecord =
    ParserLenient.map3
        (\openCurly maybeFields closedCurly ->
            ElmFormatSyntax.PatternRecord
                { openCurly = openCurly
                , fields = maybeFields
                , closedCurly = closedCurly
                }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "{")
        (commaSeparated (withStartFollowedByWhitespaceAndComments nameLowercase))
        (symbolStartFollowedByWhitespaceAndComments "}")


patternParens : Parser ElmFormatSyntax.Pattern
patternParens =
    ParserLenient.map3
        (\openParen maybeParts closedParen ->
            case maybeParts of
                Nothing ->
                    ElmFormatSyntax.PatternUnit
                        { openParen = openParen
                        , closedParen = closedParen
                        }

                Just part0Up ->
                    case part0Up.part1Up of
                        Nothing ->
                            ElmFormatSyntax.PatternParenthesized
                                { openParen = openParen
                                , inner = part0Up.part0
                                , closedParen = closedParen
                                }

                        Just part1Up ->
                            case part1Up.part2 of
                                Nothing ->
                                    ElmFormatSyntax.PatternTuple
                                        { openParen = openParen
                                        , part0 = part0Up.part0
                                        , comma0 = part1Up.comma0
                                        , part1 = part1Up.part1
                                        , closedParen = closedParen
                                        }

                                Just part2 ->
                                    ElmFormatSyntax.PatternTriple
                                        { openParen = openParen
                                        , part0 = part0Up.part0
                                        , comma0 = part1Up.comma0
                                        , part1 = part1Up.part1
                                        , comma1 = part2.comma1
                                        , part2 = part2.part2
                                        , closedParen = closedParen
                                        }
        )
        (symbolStartFollowedByWhitespaceAndCommasAndComments "(")
        (ParserLenient.map2OrSucceed
            (\part0 part1Up ->
                Just { part0 = part0, part1Up = part1Up }
            )
            pattern
            (ParserLenient.map3OrSucceed
                (\comma0 part1 part2Up ->
                    Just { comma0 = comma0, part1 = part1, part2 = part2Up }
                )
                (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
                pattern
                (ParserLenient.map2OrSucceed
                    (\comma1 part2 ->
                        Just { comma1 = comma1, part2 = part2 }
                    )
                    (symbolStartFollowedByWhitespaceAndCommasAndComments ",")
                    pattern
                    Nothing
                )
                Nothing
            )
            Nothing
        )
        (symbolStartFollowedByWhitespaceAndComments ")")


charLiteral :
    Parser
        { startSingleQuoteStart : Elm.Syntax.Range.Location
        , content : Char
        , endSingleQuote : ElmFormatSyntax.StartAndCommentsAfter
        }
charLiteral =
    ParserLenient.map2WithStartLocation
        (\start content endQuote ->
            { startSingleQuoteStart = start
            , content = content
            , endSingleQuote = endQuote
            }
        )
        (ParserLenient.symbolFollowedBy "'"
            (ParserLenient.oneOf2
                (ParserLenient.symbolFollowedBy "\\" escapedCharValueMap)
                ParserLenient.anyChar
            )
        )
        (symbolStartFollowedByWhitespaceAndComments "'")


stringLiteral :
    Parser
        { quotingStyle : ElmFormatSyntax.StringQuotingStyle
        , startQuoteStart : Elm.Syntax.Range.Location
        , content : String
        , endQuote : ElmFormatSyntax.StartAndCommentsAfter
        }
stringLiteral =
    ParserLenient.oneOf2
        tripleQuotedStringLiteralOfterTripleDoubleQuote
        singleQuotedStringLiteralAfterDoubleQuote


singleQuotedStringLiteralAfterDoubleQuote :
    Parser
        { quotingStyle : ElmFormatSyntax.StringQuotingStyle
        , startQuoteStart : Elm.Syntax.Range.Location
        , content : String
        , endQuote : ElmFormatSyntax.StartAndCommentsAfter
        }
singleQuotedStringLiteralAfterDoubleQuote =
    ParserLenient.symbolStartFollowedBy
        (\start string ->
            { quotingStyle = ElmFormatSyntax.StringSingleDoubleQuoted
            , startQuoteStart = start
            , content = string.content
            , endQuote = string.endQuote
            }
        )
        "\""
        (ParserLenient.loopUntil
            (symbolStartFollowedByWhitespaceAndComments "\"")
            (ParserLenient.oneOf2Map
                Basics.identity
                (ParserLenient.whileAtLeast1WithoutLinebreak
                    (\c ->
                        case c of
                            '"' ->
                                False

                            '\\' ->
                                False

                            _ ->
                                not (Char.Extra.isUtf16Surrogate c)
                    )
                )
                String.fromChar
                (ParserLenient.symbolFollowedBy "\\" escapedCharValueMap)
            )
            ""
            (\extension soFar ->
                soFar ++ extension ++ ""
            )
            (\endQuote content ->
                { endQuote = endQuote, content = content }
            )
        )


tripleQuotedStringLiteralOfterTripleDoubleQuote :
    Parser
        { quotingStyle : ElmFormatSyntax.StringQuotingStyle
        , startQuoteStart : Elm.Syntax.Range.Location
        , content : String
        , endQuote : ElmFormatSyntax.StartAndCommentsAfter
        }
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserLenient.symbolStartFollowedBy
        (\start string ->
            { quotingStyle = ElmFormatSyntax.StringTripleDoubleQuoted
            , startQuoteStart = start
            , content = string.content
            , endQuote = string.endQuote
            }
        )
        "\"\"\""
        (ParserLenient.loopUntil
            (symbolStartFollowedByWhitespaceAndComments "\"\"\"")
            (ParserLenient.oneOf3Map
                Basics.identity
                (ParserLenient.symbol "\"" "\"")
                String.fromChar
                (ParserLenient.symbolFollowedBy "\\" escapedCharValueMap)
                Basics.identity
                (ParserLenient.atLeastOneWhile
                    (\c ->
                        case c of
                            '"' ->
                                False

                            '\\' ->
                                False

                            _ ->
                                not (Char.Extra.isUtf16Surrogate c)
                    )
                )
            )
            ""
            (\extension soFar ->
                soFar ++ extension ++ ""
            )
            (\endQuote content ->
                { endQuote = endQuote, content = content }
            )
        )


escapedCharValueMap : Parser Char
escapedCharValueMap =
    ParserLenient.oneOf7
        (ParserLenient.symbol "'" '\'')
        (ParserLenient.symbol "\"" '"')
        (ParserLenient.symbol "n" '\n')
        (ParserLenient.symbol "t" '\t')
        -- even though elm-format will change \r to a unicode version. When you don't use elm-format, this will not happen.
        (ParserLenient.symbol "r" '\u{000D}')
        (ParserLenient.symbol "\\" '\\')
        (ParserLenient.symbolFollowedBy "u{"
            (ParserLenient.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    Char.fromCode (hexStringToInt hex)
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserLenient.followedBySymbol "}"
            )
        )


hexStringToInt : String -> Int
hexStringToInt string =
    String.foldr
        (\c soFar ->
            { exponent = soFar.exponent + 1
            , result = soFar.result + 16 ^ soFar.exponent * charToHex c
            }
        )
        exponent0Result0
        string
        |> .result


exponent0Result0 : { exponent : Int, result : Int }
exponent0Result0 =
    { exponent = 0, result = 0 }


charToHex : Char -> Int
charToHex c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        -- 'F'
        _ ->
            15


isAllowedOperatorToken : String -> Bool
isAllowedOperatorToken operatorCandidateToValidate =
    case operatorCandidateToValidate of
        "==" ->
            True

        "/=" ->
            True

        "::" ->
            True

        "++" ->
            True

        "+" ->
            True

        "*" ->
            True

        "<|" ->
            True

        "|>" ->
            True

        "||" ->
            True

        "<=" ->
            True

        ">=" ->
            True

        "|=" ->
            True

        "|." ->
            True

        "//" ->
            True

        "</>" ->
            True

        "<?>" ->
            True

        "^" ->
            True

        "<<" ->
            True

        ">>" ->
            True

        "<" ->
            True

        ">" ->
            True

        "/" ->
            True

        "&&" ->
            True

        "-" ->
            True

        _ ->
            False


isOperatorSymbolCharAsString : String -> Bool
isOperatorSymbolCharAsString c =
    case c of
        "|" ->
            True

        "+" ->
            True

        "<" ->
            True

        ">" ->
            True

        "=" ->
            True

        "*" ->
            True

        ":" ->
            True

        "-" ->
            True

        "/" ->
            True

        "&" ->
            True

        "." ->
            True

        "?" ->
            True

        "^" ->
            True

        -- only for != to /= conversion
        "!" ->
            True

        _ ->
            False
