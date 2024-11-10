module ElmSyntaxParserLenient exposing
    ( Parser, run, module_
    , Comments, commentsToList
    , expose, exposing_
    , moduleHeader, import_, declarations, declaration
    , type_, pattern, expression
    , multiLineComment, singleLineComment, whitespaceAndComments
    , moduleName, nameLowercase, nameUppercase
    , RopeFilled(..)
    )

{-| Like [`Elm.Parser`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Parser)
but able to parse badly indented code and similar somewhat incorrect syntax,
similar to elm-format.

This is **not** a fault-tolerant parser!
So if you write something it can't recognize in a file,
the whole thing will fail.

Also, precise ranges of some parts in in the parsed result are not reliable.
Though they will still be correct when viewed relative to each other
and will tell you how many lines they span.
This means [`ElmSyntaxPrint`](ElmSyntaxPrint)
can pick this up and format it in a way compatible
with the compiler or [`Elm.Parser`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Parser).

Some additional lenient parsing:

  - `f | g` → `f |> g`

  - `a != b` or `a !== b` → `a /= b`

  - `a === b` → `a == b`

  - `a ** b` → `a ^ b`

  - `\a => b` or `\a. b` → `\a -> b`

  - merges consecutive `,` in record, list or explicit exposing

  - removes extra `,` before first record field, list element or expose

  - merges consecutive `|` in choice type declaration

  - removes remove extra `|` before first variant declaration

  - corrects `port module` to `module` if no ports exist and the other way round

  - corrects exposing `(...)` → `(..)`

  - removes empty `exposing ()` after import

  - corrects expression record field name-value separators

    `{ name : value }` or `{ name value }`

    → `{ name = value }`

  - corrects type record field name-value separators

    `{ name = value }` or `{ name value }`

    → `{ name : value }`

  - expands expression record field punning

    `{ field }` → `{ field = field }`

  - corrects `->` to `=` in an expression declaration and let expression declaration

    `function parameters -> result`

    → `function parameters = result`

  - allows omitting the name before the type in an expression declaration or let expression declaration

        : Type
        function parameters =
            result

    →

        function : Type
        function parameters =
            result

  - allows matching everything before

        3 |> String.toInt case
            Nothing ->
                0

            Just n ->
                n

    →

        case 3 |> String.toInt of
            Nothing ->
                0

            Just n ->
                n

  - moves import statements anywhere at the top level to the import section

@docs Parser, run, module_

That's all you'll need most of the time.

Sometimes it's useful to parse only some part of the syntax,
to, say, display only an expression in an article
or reparse only the touched declarations on save.

@docs Comments, commentsToList
@docs expose, exposing_
@docs moduleHeader, import_, declarations, declaration
@docs type_, pattern, expression


### whitespace

@docs multiLineComment, singleLineComment, whitespaceAndComments


### low-level

@docs moduleName, nameLowercase, nameUppercase
@docs RopeFilled

-}

import Char.Extra
import Elm.Syntax.Declaration
import Elm.Syntax.Documentation
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
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation
import ParserFast


{-| Can turn a String into syntax or Nothing.
See [`ElmSyntaxParserLenient.run`](#run)

(This is not related to [`elm/parser`](https://dark.elm.dmy.fr/packages/elm/parser/latest/).
[Open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)
if you need a way to covert to that)

-}
type alias Parser a =
    ParserFast.Parser a


{-| Turn a given source String into `Just` the parsed syntax
or `Nothing` if any unrecognizable part is found.
-}
run : Parser a -> String -> Maybe a
run syntaxParser source =
    ParserFast.run syntaxParser source


{-| [`Parser`](#Parser) for an [`Elm.Syntax.File.File`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-File#File)
-}
module_ : Parser Elm.Syntax.File.File
module_ =
    ParserFast.map4
        (\moduleHeaderResult moduleComments importsResult declarationsResult ->
            let
                importStartLocation : Elm.Syntax.Range.Location
                importStartLocation =
                    case importsResult.syntax of
                        (Elm.Syntax.Node.Node import0Range _) :: _ ->
                            import0Range.start

                        [] ->
                            case declarationsResult.syntax of
                                declarationAndLateImports0 :: _ ->
                                    declarationAndLateImports0.declaration
                                        |> Elm.Syntax.Node.range
                                        |> .start

                                [] ->
                                    -- invalid syntax
                                    { row = 2, column = 1 }

                moduleHeaderBasedOnExistingPorts :
                    Elm.Syntax.Module.DefaultModuleData
                    -> Elm.Syntax.Module.Module
                moduleHeaderBasedOnExistingPorts existingModuleHeaderInfo =
                    if
                        declarationsResult.syntax
                            |> List.any
                                (\declarationAndLateImports ->
                                    declarationAndLateImports.declaration
                                        |> Elm.Syntax.Node.value
                                        |> declarationIsPort
                                )
                    then
                        Elm.Syntax.Module.PortModule existingModuleHeaderInfo

                    else
                        Elm.Syntax.Module.NormalModule existingModuleHeaderInfo
            in
            { moduleDefinition =
                moduleHeaderResult.syntax
                    |> Elm.Syntax.Node.map
                        (\syntaxModuleHeader ->
                            case syntaxModuleHeader of
                                Elm.Syntax.Module.EffectModule effectModuleHeader ->
                                    Elm.Syntax.Module.EffectModule effectModuleHeader

                                Elm.Syntax.Module.NormalModule normalModuleHeader ->
                                    moduleHeaderBasedOnExistingPorts normalModuleHeader

                                Elm.Syntax.Module.PortModule normalModuleHeader ->
                                    moduleHeaderBasedOnExistingPorts normalModuleHeader
                        )
            , imports =
                (declarationsResult.syntax
                    |> List.concatMap .lateImports
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ lateImport) ->
                            Elm.Syntax.Node.Node
                                { start = importStartLocation
                                , end = importStartLocation
                                }
                                lateImport
                        )
                )
                    ++ importsResult.syntax
            , declarations =
                declarationsResult.syntax
                    |> List.map .declaration
            , comments =
                moduleHeaderResult.comments
                    |> ropePrependTo moduleComments
                    |> ropePrependTo importsResult.comments
                    |> ropePrependTo declarationsResult.comments
                    |> commentsToList
            }
        )
        (whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
            moduleHeader
        )
        (whitespaceAndCommentsEndsTopIndentedFollowedByComments
            (ParserFast.map2OrSucceed
                (\moduleDocumentation commentsAfter ->
                    ropeOne moduleDocumentation |> ropeFilledPrependTo commentsAfter
                )
                documentationComment
                whitespaceAndCommentsEndsTopIndented
                ropeEmpty
            )
        )
        (manyWithComments import_)
        (manyWithComments
            (topIndentedFollowedBy
                (ParserFast.map3
                    (\declarationParsed commentsAfter lateImportsResult ->
                        { comments =
                            declarationParsed.comments
                                |> ropePrependTo commentsAfter
                                |> ropePrependTo lateImportsResult.comments
                        , syntax =
                            { declaration = declarationParsed.syntax
                            , lateImports = lateImportsResult.syntax
                            }
                        }
                    )
                    declaration
                    whitespaceAndComments
                    (manyWithComments import_)
                )
            )
        )


declarationIsPort : Elm.Syntax.Declaration.Declaration -> Bool
declarationIsPort syntaxDeclaration =
    case syntaxDeclaration of
        Elm.Syntax.Declaration.PortDeclaration _ ->
            True

        Elm.Syntax.Declaration.FunctionDeclaration _ ->
            False

        Elm.Syntax.Declaration.AliasDeclaration _ ->
            False

        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
            False

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            False

        Elm.Syntax.Declaration.Destructuring _ _ ->
            False


{-| [`Parser`](#Parser) for an [`Elm.Syntax.ModuleName.ModuleName`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
-}
moduleName : Parser (Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName)
moduleName =
    ParserFast.map2WithRange
        (\range head tail ->
            Elm.Syntax.Node.Node range (head :: tail)
        )
        nameUppercase
        (ParserFast.loopWhileSucceedsRightToLeftStackUnsafe
            (ParserFast.symbolFollowedBy "." nameUppercase)
            []
            (::)
        )


exposeDefinition : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing))
exposeDefinition =
    ParserFast.map2WithRange
        (\range commentsAfterExposing exposingListInnerResult ->
            { comments =
                commentsAfterExposing
                    |> ropePrependTo exposingListInnerResult.comments
            , syntax =
                Elm.Syntax.Node.Node range exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing" whitespaceAndCommentsEndsPositivelyIndented)
        exposing_


{-| [`Parser`](#Parser) for an [`Elm.Syntax.Exposing.Exposing`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#Exposing)
(the stuff after `exposing` in an import or module)
-}
exposing_ : Parser { comments : Comments, syntax : Elm.Syntax.Exposing.Exposing }
exposing_ =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2
            (\commentsBefore inner ->
                { comments = commentsBefore |> ropePrependTo inner.comments
                , syntax = inner.syntax
                }
            )
            whitespaceAndComments
            (ParserFast.oneOf3
                (ParserFast.mapWithRange
                    (\range comments ->
                        { comments = comments
                        , syntax = Elm.Syntax.Exposing.All range
                        }
                    )
                    (ParserFast.symbolFollowedBy ".." whitespaceAndCommentsEndsPositivelyIndented)
                )
                (ParserFast.mapWithRange
                    (\range comments ->
                        { comments = comments
                        , syntax = Elm.Syntax.Exposing.All range
                        }
                    )
                    (ParserFast.symbolFollowedBy "..." whitespaceAndCommentsEndsPositivelyIndented)
                )
                (exposingWithinParensExplicitMap identity)
            )
        )
        |> ParserFast.followedBySymbol ")"


exposingWithinParensExplicitMap : (Elm.Syntax.Exposing.Exposing -> syntax) -> ParserFast.Parser (WithComments syntax)
exposingWithinParensExplicitMap exposingToSyntax =
    ParserFast.map4
        (\commentsBeforeHeadElement headElement commentsAfterHeadElement tailElements ->
            { comments =
                commentsBeforeHeadElement
                    |> ropePrependTo headElement.comments
                    |> ropePrependTo commentsAfterHeadElement
                    |> ropePrependTo tailElements.comments
            , syntax =
                Elm.Syntax.Exposing.Explicit
                    (headElement.syntax :: tailElements.syntax)
                    |> exposingToSyntax
            }
        )
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
            ropeEmpty
        )
        expose
        whitespaceAndCommentsEndsPositivelyIndented
        (manyWithComments
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map4
                    (\commentsBefore commentsWithExtraComma result commentsAfter ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo commentsWithExtraComma
                                |> ropePrependTo result.comments
                                |> ropePrependTo commentsAfter
                        , syntax = result.syntax
                        }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                        ropeEmpty
                    )
                    expose
                    whitespaceAndCommentsEndsPositivelyIndented
                )
            )
        )


{-| [`Parser`](#Parser) for a single [`Elm.Syntax.Exposing.TopLevelExpose`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#TopLevelExpose)
-}
expose : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose }
expose =
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
infixExpose =
    ParserFast.map2WithRange
        (\range infixName () ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Exposing.InfixExpose infixName)
            }
        )
        (ParserFast.symbolFollowedBy "("
            (ParserFast.ifFollowedByWhileWithoutLinebreak
                (\c -> c /= ')' && c /= '\n' && c /= ' ')
                (\c -> c /= ')' && c /= '\n' && c /= ' ')
            )
        )
        (ParserFast.symbol ")" ())


typeExpose : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
typeExpose =
    ParserFast.map3
        (\(Elm.Syntax.Node.Node typeNameRange typeExposeName) commentsBeforeMaybeOpen maybeOpen ->
            case maybeOpen of
                Nothing ->
                    { comments = commentsBeforeMaybeOpen
                    , syntax =
                        Elm.Syntax.Node.Node typeNameRange (Elm.Syntax.Exposing.TypeOrAliasExpose typeExposeName)
                    }

                Just open ->
                    { comments = commentsBeforeMaybeOpen |> ropePrependTo open.comments
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start = typeNameRange.start
                            , end = open.syntax.end
                            }
                            (Elm.Syntax.Exposing.TypeExpose { name = typeExposeName, open = Just open.syntax })
                    }
        )
        nameUppercaseNode
        whitespaceAndComments
        (ParserFast.map2WithRangeOrSucceed
            (\range left right ->
                Just { comments = left |> ropePrependTo right, syntax = range }
            )
            (ParserFast.symbolFollowedBy "(" whitespaceAndCommentsEndsPositivelyIndented)
            (ParserFast.symbolFollowedBy ".." whitespaceAndCommentsEndsPositivelyIndented
                |> ParserFast.followedBySymbol ")"
            )
            Nothing
        )


functionExpose : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
functionExpose =
    nameLowercaseMapWithRange
        (\range name ->
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Exposing.FunctionExpose name)
            }
        )


{-| [`Parser`](#Parser) for an [`Elm.Syntax.Module.Module`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.Module.Module }
moduleHeader =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : Parser (WithComments ( String, Elm.Syntax.Node.Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual fnTypeName ->
            { comments = commentsAfterFnName |> ropePrependTo commentsAfterEqual
            , syntax = ( fnName, fnTypeName )
            }
        )
        nameLowercase
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
        nameUppercaseNode


whereBlock : Parser (WithComments { command : Maybe (Elm.Syntax.Node.Node String), subscription : Maybe (Elm.Syntax.Node.Node String) })
whereBlock =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map4
            (\commentsBeforeHead head commentsAfterHead tail ->
                let
                    pairs : List ( String, Elm.Syntax.Node.Node String )
                    pairs =
                        head.syntax :: tail.syntax
                in
                { comments =
                    commentsBeforeHead
                        |> ropePrependTo head.comments
                        |> ropePrependTo commentsAfterHead
                        |> ropePrependTo tail.comments
                , syntax =
                    { command =
                        pairs
                            |> listFirstWhere
                                (\( fnName, _ ) ->
                                    case fnName of
                                        "command" ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.map Tuple.second
                    , subscription =
                        pairs
                            |> listFirstWhere
                                (\( fnName, _ ) ->
                                    case fnName of
                                        "subscription" ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.map Tuple.second
                    }
                }
            )
            whitespaceAndCommentsEndsPositivelyIndented
            effectWhereClause
            whitespaceAndCommentsEndsPositivelyIndented
            (manyWithComments
                (ParserFast.symbolFollowedBy "," (surroundedByWhitespaceAndCommentsEndsPositivelyIndented effectWhereClause))
            )
        )
        |> ParserFast.followedBySymbol "}"


listFirstWhere : (a -> Bool) -> List a -> Maybe a
listFirstWhere predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                listFirstWhere predicate xs


effectWhereClauses : Parser (WithComments { command : Maybe (Elm.Syntax.Node.Node String), subscription : Maybe (Elm.Syntax.Node.Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            { comments = commentsBefore |> ropePrependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" whitespaceAndCommentsEndsPositivelyIndented)
        whereBlock


effectModuleDefinition : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
effectModuleDefinition =
    ParserFast.map7WithRange
        (\range commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            { comments =
                commentsAfterEffect
                    |> ropePrependTo commentsAfterModule
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo whereClauses.comments
                    |> ropePrependTo commentsAfterWhereClauses
                    |> ropePrependTo exp.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.EffectModule
                        { moduleName = name
                        , exposingList = exp.syntax
                        , command = whereClauses.syntax.command
                        , subscription = whereClauses.syntax.subscription
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "effect" whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.keywordFollowedBy "module" whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        whitespaceAndCommentsEndsPositivelyIndented
        effectWhereClauses
        whitespaceAndCommentsEndsPositivelyIndented
        exposeDefinition


normalModuleDefinition : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
normalModuleDefinition =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleNameNode commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterModule
                    |> ropePrependTo commentsAfterModuleName
                    |> ropePrependTo exposingList.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.NormalModule
                        { moduleName = moduleNameNode
                        , exposingList = exposingList.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "module" whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        whitespaceAndCommentsEndsPositivelyIndented
        exposeDefinition


portModuleDefinition : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleNameNode commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterModule
                    |> ropePrependTo commentsAfterModuleName
                    |> ropePrependTo exposingList.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.PortModule { moduleName = moduleNameNode, exposingList = exposingList.syntax })
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.keywordFollowedBy "module" whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        whitespaceAndCommentsEndsPositivelyIndented
        exposeDefinition


{-| [`Parser`](#Parser) for a single [`Elm.Syntax.Import.Import`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import#Import)
-}
import_ : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.Import.Import }
import_ =
    ParserFast.map5WithStartLocation
        (\start commentsAfterImport mod commentsAfterModuleName maybeModuleAlias maybeExposingResult ->
            let
                commentsBeforeAlias : Comments
                commentsBeforeAlias =
                    commentsAfterImport
                        |> ropePrependTo commentsAfterModuleName
            in
            case maybeModuleAlias of
                Nothing ->
                    case maybeExposingResult.syntax of
                        Nothing ->
                            let
                                (Elm.Syntax.Node.Node modRange _) =
                                    mod
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = modRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                            }

                        Just exposingListValue ->
                            let
                                (Elm.Syntax.Node.Node exposingRange _) =
                                    exposingListValue
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Just exposingListValue
                                    }
                            }

                Just moduleAliasResult ->
                    case maybeExposingResult.syntax of
                        Nothing ->
                            let
                                (Elm.Syntax.Node.Node aliasRange _) =
                                    moduleAliasResult.syntax
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo moduleAliasResult.comments
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = aliasRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Nothing
                                    }
                            }

                        Just exposingListValue ->
                            let
                                (Elm.Syntax.Node.Node exposingRange _) =
                                    exposingListValue
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo moduleAliasResult.comments
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Just exposingListValue
                                    }
                            }
        )
        (ParserFast.keywordFollowedBy "import" whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        whitespaceAndComments
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    { comments = commentsBefore |> ropePrependTo commentsAfter
                    , syntax = moduleAliasNode
                    }
            )
            (ParserFast.keywordFollowedBy "as" whitespaceAndCommentsEndsPositivelyIndented)
            (nameUppercaseMapWithRange
                (\range moduleAlias ->
                    Elm.Syntax.Node.Node range [ moduleAlias ]
                )
            )
            whitespaceAndComments
            Nothing
        )
        (ParserFast.map2OrSucceed
            (\exposingResult commentsAfter ->
                { comments = exposingResult.comments |> ropePrependTo commentsAfter
                , syntax = exposingResult.syntax
                }
            )
            (ParserFast.map2WithRange
                (\range commentsAfterExposing exposingListInnerResult ->
                    { comments =
                        commentsAfterExposing
                            |> ropePrependTo exposingListInnerResult.comments
                    , syntax =
                        case exposingListInnerResult.syntax of
                            Nothing ->
                                Nothing

                            Just exposingListInner ->
                                Just (Elm.Syntax.Node.Node range exposingListInner)
                    }
                )
                (ParserFast.symbolFollowedBy "exposing" whitespaceAndCommentsEndsPositivelyIndented)
                (ParserFast.symbolFollowedBy "("
                    (ParserFast.map2
                        (\commentsBefore inner ->
                            { comments = commentsBefore |> ropePrependTo inner.comments
                            , syntax = inner.syntax
                            }
                        )
                        whitespaceAndComments
                        (ParserFast.oneOf4
                            (ParserFast.mapWithRange
                                (\range comments ->
                                    { comments = comments
                                    , syntax = Just (Elm.Syntax.Exposing.All range)
                                    }
                                )
                                (ParserFast.symbolFollowedBy ".." whitespaceAndCommentsEndsPositivelyIndented)
                                |> ParserFast.followedBySymbol ")"
                            )
                            (ParserFast.mapWithRange
                                (\range comments ->
                                    { comments = comments
                                    , syntax = Just (Elm.Syntax.Exposing.All range)
                                    }
                                )
                                (ParserFast.symbolFollowedBy "..." whitespaceAndCommentsEndsPositivelyIndented)
                                |> ParserFast.followedBySymbol ")"
                            )
                            (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                            (exposingWithinParensExplicitMap Just
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
            whitespaceAndComments
            { comments = ropeEmpty, syntax = Nothing }
        )


{-| [`Parser`](#Parser) for a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
and comments in between
-}
declarations : Parser { comments : Comments, syntax : List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration) }
declarations =
    manyWithComments
        (topIndentedFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    { comments = declarationParsed.comments |> ropePrependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
                )
                declaration
                whitespaceAndComments
            )
        )


{-| [`Parser`](#Parser) for an [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)
-}
declaration : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration }
declaration =
    ParserFast.oneOf5
        functionDeclarationWithoutDocumentation
        declarationWithDocumentation
        typeOrTypeAliasDefinitionWithoutDocumentation
        portDeclarationWithoutDocumentation
        infixDeclaration


documentationComment : Parser (Elm.Syntax.Node.Node Elm.Syntax.Documentation.Documentation)
documentationComment =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiCommentMapWithRange Elm.Syntax.Node.Node
        ( '{', "-" )
        ( '-', "}" )


declarationWithDocumentation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
declarationWithDocumentation =
    ParserFast.map2
        (\documentation afterDocumentation ->
            let
                start : Elm.Syntax.Range.Location
                start =
                    (Elm.Syntax.Node.range documentation).start
            in
            case afterDocumentation.syntax of
                FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                    case functionDeclarationAfterDocumentation.signature of
                        Just signature ->
                            let
                                (Elm.Syntax.Node.Node implementationNameRange _) =
                                    signature.implementationName

                                (Elm.Syntax.Node.Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments = afterDocumentation.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = expressionRange.end }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature =
                                            Just
                                                (Elm.Syntax.Node.combine (\name value -> { name = name, typeAnnotation = value })
                                                    functionDeclarationAfterDocumentation.startName
                                                    signature.typeAnnotation
                                                )
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = implementationNameRange.start, end = expressionRange.end }
                                                { name = signature.implementationName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                            }

                        Nothing ->
                            let
                                (Elm.Syntax.Node.Node startNameRange _) =
                                    functionDeclarationAfterDocumentation.startName

                                (Elm.Syntax.Node.Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments = afterDocumentation.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = expressionRange.end }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = startNameRange.start, end = expressionRange.end }
                                                { name = functionDeclarationAfterDocumentation.startName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                            }

                TypeDeclarationAfterDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : Elm.Syntax.Range.Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (Elm.Syntax.Node.Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (Elm.Syntax.Node.Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    { comments = afterDocumentation.comments
                    , syntax =
                        Elm.Syntax.Node.Node { start = start, end = end }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Just documentation
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                    }

                TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Elm.Syntax.Node.Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments = afterDocumentation.comments
                    , syntax =
                        Elm.Syntax.Node.Node { start = start, end = typeAnnotationRange.end }
                            (Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Just documentation
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }

                PortDeclarationAfterDocumentation portDeclarationAfterName ->
                    let
                        (Elm.Syntax.Node.Node typeAnnotationRange _) =
                            portDeclarationAfterName.typeAnnotation
                    in
                    { comments =
                        ropeOne documentation
                            |> ropeFilledPrependTo afterDocumentation.comments
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start = portDeclarationAfterName.startLocation
                            , end = typeAnnotationRange.end
                            }
                            (Elm.Syntax.Declaration.PortDeclaration
                                { name = portDeclarationAfterName.name
                                , typeAnnotation = portDeclarationAfterName.typeAnnotation
                                }
                            )
                    }
        )
        documentationComment
        (whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
            (ParserFast.oneOf3
                functionAfterDocumentation
                typeOrTypeAliasDefinitionAfterDocumentation
                portDeclarationAfterDocumentation
            )
        )
        |> ParserFast.validate
            (\result ->
                let
                    (Elm.Syntax.Node.Node _ decl) =
                        result.syntax
                in
                case decl of
                    Elm.Syntax.Declaration.FunctionDeclaration letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Elm.Syntax.Node.Node _ signature) ->
                                let
                                    (Elm.Syntax.Node.Node _ implementationName) =
                                        implementation.name

                                    (Elm.Syntax.Node.Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Elm.Syntax.Node.Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName

                    _ ->
                        True
            )


type DeclarationAfterDocumentation
    = FunctionDeclarationAfterDocumentation
        { startName : Elm.Syntax.Node.Node String
        , signature :
            Maybe
                { typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
                , implementationName : Elm.Syntax.Node.Node String
                }
        , arguments : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    | TypeDeclarationAfterDocumentation
        { name : Elm.Syntax.Node.Node String
        , parameters : List (Elm.Syntax.Node.Node String)
        , headVariant : Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor
        , tailVariantsReverse : List (Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor)
        }
    | TypeAliasDeclarationAfterDocumentation
        { name : Elm.Syntax.Node.Node String
        , parameters : List (Elm.Syntax.Node.Node String)
        , typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }
    | PortDeclarationAfterDocumentation
        { startLocation : Elm.Syntax.Range.Location
        , name : Elm.Syntax.Node.Node String
        , typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }


type TypeOrTypeAliasDeclarationWithoutDocumentation
    = TypeDeclarationWithoutDocumentation
        { name : Elm.Syntax.Node.Node String
        , parameters : List (Elm.Syntax.Node.Node String)
        , headVariant : Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor
        , tailVariantsReverse : List (Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor)
        }
    | TypeAliasDeclarationWithoutDocumentation
        { name : Elm.Syntax.Node.Node String
        , parameters : List (Elm.Syntax.Node.Node String)
        , typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }


functionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
functionAfterDocumentation =
    ParserFast.oneOf2
        (ParserFast.map6
            (\startName commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
                { comments =
                    (commentsAfterStartName |> ropePrependTo maybeSignature.comments)
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    FunctionDeclarationAfterDocumentation
                        { startName = startName
                        , signature = maybeSignature.syntax
                        , arguments = arguments.syntax
                        , expression = result.syntax
                        }
                }
            )
            -- infix declarations itself don't have documentation
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.map4OrSucceed
                (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> ropePrependTo typeAnnotationResult.comments
                            |> ropePrependTo implementationName.comments
                            |> ropePrependTo afterImplementationName
                    , syntax =
                        Just
                            { implementationName = implementationName.syntax
                            , typeAnnotation = typeAnnotationResult.syntax
                            }
                    }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNode
                )
                whitespaceAndCommentsEndsPositivelyIndented
                { comments = ropeEmpty, syntax = Nothing }
            )
            parameterPatternsEqual
            whitespaceAndCommentsEndsPositivelyIndented
            expressionFollowedByOptimisticLayout
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeTypeAnnotation typeAnnotationResult commentsBetweenTypeAndName nameNode afterImplementationName arguments commentsAfterEqual result ->
                { comments =
                    commentsBeforeTypeAnnotation
                        |> ropePrependTo typeAnnotationResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    FunctionDeclarationAfterDocumentation
                        { startName =
                            -- dummy
                            Elm.Syntax.Node.Node
                                { start = start, end = start }
                                (nameNode |> Elm.Syntax.Node.value)
                        , signature =
                            Just
                                { implementationName = nameNode
                                , typeAnnotation = typeAnnotationResult.syntax
                                }
                        , arguments = arguments.syntax
                        , expression = result.syntax
                        }
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
            parameterPatternsEqual
            whitespaceAndCommentsEndsPositivelyIndented
            expressionFollowedByOptimisticLayout
        )


functionDeclarationWithoutDocumentation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
functionDeclarationWithoutDocumentation =
    ParserFast.oneOf2
        (ParserFast.map6WithStartLocation
            (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
                let
                    (Elm.Syntax.Node.Node expressionRange _) =
                        result.syntax
                in
                case maybeSignature of
                    Nothing ->
                        { comments =
                            commentsAfterStartName
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo result.comments
                        , syntax =
                            Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                                (Elm.Syntax.Declaration.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                                            { name = startNameNode
                                            , arguments = arguments.syntax
                                            , expression = result.syntax
                                            }
                                    }
                                )
                        }

                    Just signature ->
                        let
                            (Elm.Syntax.Node.Node implementationNameRange _) =
                                signature.implementationName
                        in
                        { comments =
                            (commentsAfterStartName |> ropePrependTo signature.comments)
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo result.comments
                        , syntax =
                            Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                                (Elm.Syntax.Declaration.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            (Elm.Syntax.Node.combine (\name typeAnnotation -> { name = name, typeAnnotation = typeAnnotation })
                                                startNameNode
                                                signature.typeAnnotation
                                            )
                                    , declaration =
                                        Elm.Syntax.Node.Node { start = implementationNameRange.start, end = expressionRange.end }
                                            { name = signature.implementationName
                                            , arguments = arguments.syntax
                                            , expression = result.syntax
                                            }
                                    }
                                )
                        }
            )
            functionNameNotInfixNode
            whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.map4OrSucceed
                (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> ropePrependTo typeAnnotationResult.comments
                                |> ropePrependTo implementationName.comments
                                |> ropePrependTo afterImplementationName
                        , implementationName = implementationName.syntax
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNode
                )
                whitespaceAndCommentsEndsPositivelyIndented
                Nothing
            )
            parameterPatternsEqual
            whitespaceAndCommentsEndsPositivelyIndented
            expressionFollowedByOptimisticLayout
            |> ParserFast.validate
                (\result ->
                    let
                        (Elm.Syntax.Node.Node _ decl) =
                            result.syntax
                    in
                    case decl of
                        Elm.Syntax.Declaration.FunctionDeclaration letFunctionDeclaration ->
                            case letFunctionDeclaration.signature of
                                Nothing ->
                                    True

                                Just (Elm.Syntax.Node.Node _ signature) ->
                                    let
                                        (Elm.Syntax.Node.Node _ implementationName) =
                                            implementation.name

                                        (Elm.Syntax.Node.Node _ implementation) =
                                            letFunctionDeclaration.declaration

                                        (Elm.Syntax.Node.Node _ signatureName) =
                                            signature.name
                                    in
                                    implementationName == signatureName

                        Elm.Syntax.Declaration.AliasDeclaration _ ->
                            True

                        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                            True

                        Elm.Syntax.Declaration.PortDeclaration _ ->
                            True

                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                            True

                        Elm.Syntax.Declaration.Destructuring _ _ ->
                            True
                )
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeTypeAnnotation typeAnnotationResult commentsBetweenTypeAndName nameNode afterImplementationName arguments commentsAfterEqual result ->
                { comments =
                    commentsBeforeTypeAnnotation
                        |> ropePrependTo typeAnnotationResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    Elm.Syntax.Node.Node
                        { start = start
                        , end = result.syntax |> Elm.Syntax.Node.range |> .end
                        }
                        (Elm.Syntax.Declaration.FunctionDeclaration
                            { documentation = Nothing
                            , signature =
                                Just
                                    (Elm.Syntax.Node.Node
                                        { start = start
                                        , end = typeAnnotationResult.syntax |> Elm.Syntax.Node.range |> .end
                                        }
                                        { name =
                                            -- dummy
                                            Elm.Syntax.Node.Node
                                                { start = start, end = start }
                                                (nameNode |> Elm.Syntax.Node.value)
                                        , typeAnnotation = typeAnnotationResult.syntax
                                        }
                                    )
                            , declaration =
                                Elm.Syntax.Node.Node
                                    { start = nameNode |> Elm.Syntax.Node.range |> .start
                                    , end = result.syntax |> Elm.Syntax.Node.range |> .end
                                    }
                                    { name = nameNode
                                    , arguments = arguments.syntax
                                    , expression = result.syntax
                                    }
                            }
                        )
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
            parameterPatternsEqual
            whitespaceAndCommentsEndsPositivelyIndented
            expressionFollowedByOptimisticLayout
        )


parameterPatternsEqual : Parser (WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)))
parameterPatternsEqual =
    untilWithComments
        (ParserFast.oneOf2
            (ParserFast.symbol "=" ())
            (ParserFast.symbol "->" ())
        )
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> ropePrependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            patternNotSpaceSeparated
            whitespaceAndCommentsEndsPositivelyIndented
        )


infixDeclaration : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            { comments =
                commentsAfterInfix
                    |> ropePrependTo commentsAfterDirection
                    |> ropePrependTo commentsAfterPrecedence
                    |> ropePrependTo commentsAfterOperator
                    |> ropePrependTo commentsAfterEqual
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Declaration.InfixDeclaration
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "infix" whitespaceAndCommentsEndsPositivelyIndented)
        infixDirection
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.integerDecimalMapWithRange Elm.Syntax.Node.Node)
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "("
            (ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                (\operatorRange operator ->
                    Elm.Syntax.Node.Node
                        { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                        , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                        }
                        operator
                )
                isOperatorSymbolCharAsString
                isAllowedOperatorToken
                ")"
            )
        )
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
        nameLowercaseNode


infixDirection : Parser (Elm.Syntax.Node.Node Elm.Syntax.Infix.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "right" Elm.Syntax.Infix.Right))
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "left" Elm.Syntax.Infix.Left))
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "non" Elm.Syntax.Infix.Non))


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    ParserFast.map5
        (\commentsAfterPort nameNode commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (Elm.Syntax.Node.Node nameRange _) =
                    nameNode
            in
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo typeAnnotationResult.comments
                    |> ropePrependTo commentsAfterColon
            , syntax =
                PortDeclarationAfterDocumentation
                    { startLocation = { row = nameRange.start.row, column = 1 }
                    , name = nameNode
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndCommentsEndsPositivelyIndented)
        nameLowercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
        type_


portDeclarationWithoutDocumentation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
portDeclarationWithoutDocumentation =
    ParserFast.map5
        (\commentsAfterPort nameNode commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (Elm.Syntax.Node.Node nameRange _) =
                    nameNode

                (Elm.Syntax.Node.Node typeRange _) =
                    typeAnnotationResult.syntax
            in
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo commentsAfterColon
                    |> ropePrependTo typeAnnotationResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = { row = nameRange.start.row, column = 1 }
                    , end = typeRange.end
                    }
                    (Elm.Syntax.Declaration.PortDeclaration
                        { name = nameNode
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndCommentsEndsPositivelyIndented)
        nameLowercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
        type_


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    ParserFast.map2
        (\commentsAfterType declarationAfterDocumentation ->
            { comments = commentsAfterType |> ropePrependTo declarationAfterDocumentation.comments
            , syntax = declarationAfterDocumentation.syntax
            }
        )
        (ParserFast.keywordFollowedBy "type" whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            typeAliasDefinitionAfterDocumentationAfterTypePrefix
            customTypeDefinitionAfterDocumentationAfterTypePrefix
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEquals typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEquals
                    |> ropePrependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" whitespaceAndCommentsEndsPositivelyIndented)
        nameUppercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        whitespaceAndCommentsEndsPositivelyIndented
        type_


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map7
        (\name commentsAfterName parameters commentsAfterEqual commentsBeforeHeadVariant headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo commentsBeforeHeadVariant
                    |> ropePrependTo headVariant.comments
                    |> ropePrependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "|" whitespaceAndCommentsEndsPositivelyIndented)
            ropeEmpty
        )
        variantDeclarationFollowedByOptimisticLayout
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map3
                        (\commentsBeforePipe commentsWithExtraPipe variantResult ->
                            { comments =
                                commentsBeforePipe
                                    |> ropePrependTo commentsWithExtraPipe
                                    |> ropePrependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
                        )
                        whitespaceAndCommentsEndsPositivelyIndented
                        (ParserFast.orSucceed
                            (ParserFast.symbolFollowedBy "|" whitespaceAndCommentsEndsPositivelyIndented)
                            ropeEmpty
                        )
                        variantDeclarationFollowedByOptimisticLayout
                    )
                )
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    ParserFast.map2WithStartLocation
        (\start commentsAfterType afterStart ->
            let
                allComments : Comments
                allComments =
                    commentsAfterType |> ropePrependTo afterStart.comments
            in
            case afterStart.syntax of
                TypeDeclarationWithoutDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : Elm.Syntax.Range.Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (Elm.Syntax.Node.Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (Elm.Syntax.Node.Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    { comments = allComments
                    , syntax =
                        Elm.Syntax.Node.Node { start = start, end = end }
                            (Elm.Syntax.Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                    }

                TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Elm.Syntax.Node.Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments = allComments
                    , syntax =
                        Elm.Syntax.Node.Node { start = start, end = typeAnnotationRange.end }
                            (Elm.Syntax.Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }
        )
        (ParserFast.keywordFollowedBy "type"
            whitespaceAndCommentsEndsPositivelyIndented
        )
        (ParserFast.oneOf2
            typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            customTypeDefinitionWithoutDocumentationAfterTypePrefix
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEqual typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" whitespaceAndCommentsEndsPositivelyIndented)
        nameUppercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        whitespaceAndCommentsEndsPositivelyIndented
        type_


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map7
        (\name commentsAfterName parameters commentsAfterEqual commentsBeforeHeadVariant headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo commentsBeforeHeadVariant
                    |> ropePrependTo headVariant.comments
                    |> ropePrependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "|" whitespaceAndCommentsEndsPositivelyIndented)
            ropeEmpty
        )
        variantDeclarationFollowedByOptimisticLayout
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map3
                        (\commentsBeforePipe commentsWithExtraPipe variantResult ->
                            { comments =
                                commentsBeforePipe
                                    |> ropePrependTo commentsWithExtraPipe
                                    |> ropePrependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
                        )
                        whitespaceAndCommentsEndsPositivelyIndented
                        (ParserFast.orSucceed
                            (ParserFast.symbolFollowedBy "|" whitespaceAndCommentsEndsPositivelyIndented)
                            ropeEmpty
                        )
                        variantDeclarationFollowedByOptimisticLayout
                    )
                )
            )
        )


variantDeclarationFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor))
variantDeclarationFollowedByOptimisticLayout =
    ParserFast.map3
        (\nameNode commentsAfterName argumentsReverse ->
            let
                (Elm.Syntax.Node.Node nameRange _) =
                    nameNode

                fullRange : Elm.Syntax.Range.Range
                fullRange =
                    case argumentsReverse.syntax of
                        (Elm.Syntax.Node.Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }

                        [] ->
                            nameRange
            in
            { comments =
                commentsAfterName
                    |> ropePrependTo argumentsReverse.comments
            , syntax =
                Elm.Syntax.Node.Node fullRange
                    { name = nameNode
                    , arguments = List.reverse argumentsReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments = typeAnnotationResult.comments |> ropePrependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


typeGenericListEquals : Parser (WithComments (List (Elm.Syntax.Node.Node String)))
typeGenericListEquals =
    untilWithComments (ParserFast.symbol "=" ())
        (ParserFast.map2
            (\name commentsAfterName ->
                { comments = commentsAfterName
                , syntax = name
                }
            )
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
        )


{-| [`Parser`](#Parser) for an [`Elm.Syntax.TypeAnnotation.TypeAnnotation`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation#TypeAnnotation)
-}
type_ : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation }
type_ =
    ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
        (ParserFast.map2
            (\startType commentsAfter ->
                { comments =
                    startType.comments
                        |> ropePrependTo commentsAfter
                , syntax = startType.syntax
                }
            )
            (ParserFast.lazy (\() -> typeAnnotationNotFunction))
            whitespaceAndComments
        )
        (ParserFast.symbolFollowedBy "->"
            (positivelyIndentedPlusFollowedBy 2
                (ParserFast.map3
                    (\commentsAfterArrow typeAnnotationResult commentsAfterType ->
                        { comments =
                            commentsAfterArrow
                                |> ropePrependTo typeAnnotationResult.comments
                                |> ropePrependTo commentsAfterType
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.lazy (\() -> typeAnnotationNotFunction))
                    whitespaceAndComments
                )
            )
        )
        (\inType outType ->
            { comments =
                inType.comments
                    |> ropePrependTo outType.comments
            , syntax =
                Elm.Syntax.Node.combine Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType.syntax outType.syntax
            }
        )


typeNotSpaceSeparated : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeNotSpaceSeparated =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithoutArguments
        genericTypeAnnotation
        recordTypeAnnotation


typeAnnotationNotFunction : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeAnnotationNotFunction =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithArgumentsOptimisticLayout
        genericTypeAnnotation
        recordTypeAnnotation


parensTypeAnnotation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
parensTypeAnnotation =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = ropeEmpty
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start = { row = end.row, column = end.column - 2 }
                            , end = end
                            }
                            Elm.Syntax.TypeAnnotation.Unit
                    }
                )
            )
            (ParserFast.map4WithRange
                (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart lastToSecondPart ->
                    { comments =
                        commentsBeforeFirstPart
                            |> ropePrependTo firstPart.comments
                            |> ropePrependTo commentsAfterFirstPart
                            |> ropePrependTo lastToSecondPart.comments
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case lastToSecondPart.syntax of
                                Nothing ->
                                    -- parenthesized types are not a `Tupled [ firstPart.syntax ]`
                                    -- but their Range still extends to both parens.
                                    -- This is done to not break behavior of v7.
                                    -- This will likely change in v8 after discussion in issues like https://github.com/stil4m/elm-syntax/issues/204
                                    let
                                        (Elm.Syntax.Node.Node _ firstPartType) =
                                            firstPart.syntax
                                    in
                                    firstPartType

                                Just firstAndMaybeThirdPart ->
                                    case firstAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            Elm.Syntax.TypeAnnotation.Tupled [ firstPart.syntax, firstAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            Elm.Syntax.TypeAnnotation.Tupled [ firstPart.syntax, firstAndMaybeThirdPart.secondPart, thirdPart ]
                            )
                    }
                )
                whitespaceAndCommentsEndsPositivelyIndented
                type_
                whitespaceAndCommentsEndsPositivelyIndented
                (ParserFast.oneOf2
                    (ParserFast.symbol ")"
                        { comments = ropeEmpty, syntax = Nothing }
                    )
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore secondPartResult commentsAfter maybeThirdPartResult ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo secondPartResult.comments
                                        |> ropePrependTo commentsAfter
                                , syntax = Just { maybeThirdPart = maybeThirdPartResult.syntax, secondPart = secondPartResult.syntax }
                                }
                            )
                            whitespaceAndCommentsEndsPositivelyIndented
                            type_
                            whitespaceAndCommentsEndsPositivelyIndented
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")"
                                    { comments = ropeEmpty, syntax = Nothing }
                                )
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore thirdPartResult commentsAfter ->
                                            { comments =
                                                commentsBefore
                                                    |> ropePrependTo thirdPartResult.comments
                                                    |> ropePrependTo commentsAfter
                                            , syntax = Just thirdPartResult.syntax
                                            }
                                        )
                                        whitespaceAndCommentsEndsPositivelyIndented
                                        type_
                                        whitespaceAndCommentsEndsPositivelyIndented
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


genericTypeAnnotation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
genericTypeAnnotation =
    nameLowercaseMapWithRange
        (\range var ->
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.TypeAnnotation.GenericType var)
            }
        )


recordTypeAnnotation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
recordTypeAnnotation =
    ParserFast.map2WithRange
        (\range commentsBefore afterCurly ->
            case afterCurly of
                Nothing ->
                    { comments = commentsBefore
                    , syntax =
                        Elm.Syntax.Node.Node range typeAnnotationRecordEmpty
                    }

                Just afterCurlyResult ->
                    { comments =
                        commentsBefore
                            |> ropePrependTo afterCurlyResult.comments
                    , syntax =
                        Elm.Syntax.Node.Node range afterCurlyResult.syntax
                    }
        )
        (ParserFast.symbolFollowedBy "{" whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            (ParserFast.symbol "}" Nothing)
            (ParserFast.map4
                (\commentsBeforeFirstName firstNameNode commentsAfterFirstName afterFirstName ->
                    Just
                        { comments =
                            commentsBeforeFirstName
                                |> ropePrependTo commentsAfterFirstName
                                |> ropePrependTo afterFirstName.comments
                        , syntax =
                            case afterFirstName.syntax of
                                RecordExtensionExpressionAfterName fields ->
                                    Elm.Syntax.TypeAnnotation.GenericRecord firstNameNode fields

                                FieldsAfterName fieldsAfterName ->
                                    Elm.Syntax.TypeAnnotation.Record (Elm.Syntax.Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                        }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                    ropeEmpty
                )
                nameLowercaseNode
                whitespaceAndCommentsEndsPositivelyIndented
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithRange
                            (\range commentsBefore head tail ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo head.comments
                                        |> ropePrependTo tail.comments
                                , syntax =
                                    RecordExtensionExpressionAfterName
                                        (Elm.Syntax.Node.Node range (head.syntax :: tail.syntax))
                                }
                            )
                            whitespaceAndCommentsEndsPositivelyIndented
                            typeRecordFieldDefinition
                            (manyWithComments
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore commentsWithExtraComma field ->
                                            { comments =
                                                commentsBefore
                                                    |> ropePrependTo commentsWithExtraComma
                                                    |> ropePrependTo field.comments
                                            , syntax = field.syntax
                                            }
                                        )
                                        whitespaceAndCommentsEndsPositivelyIndented
                                        (ParserFast.orSucceed
                                            (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                                            ropeEmpty
                                        )
                                        typeRecordFieldDefinition
                                    )
                                )
                            )
                        )
                    )
                    (ParserFast.map4
                        (\commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
                            { comments =
                                commentsBeforeFirstFieldValue
                                    |> ropePrependTo firstFieldValue.comments
                                    |> ropePrependTo commentsAfterFirstFieldValue
                                    |> ropePrependTo tailFields.comments
                            , syntax =
                                FieldsAfterName
                                    { firstFieldValue = firstFieldValue.syntax
                                    , tailFields = tailFields.syntax
                                    }
                            }
                        )
                        (ParserFast.oneOf2OrSucceed
                            (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
                            (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
                            ropeEmpty
                        )
                        type_
                        whitespaceAndCommentsEndsPositivelyIndented
                        (ParserFast.orSucceed
                            (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                            { comments = ropeEmpty, syntax = [] }
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
        )


typeAnnotationRecordEmpty : Elm.Syntax.TypeAnnotation.TypeAnnotation
typeAnnotationRecordEmpty =
    Elm.Syntax.TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.RecordDefinition)
    | FieldsAfterName { firstFieldValue : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation, tailFields : List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments Elm.Syntax.TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserFast.map4
        (\commentsBefore commentsWithExtraComma head tail ->
            { comments =
                commentsWithExtraComma
                    |> ropePrependTo commentsBefore
                    |> ropePrependTo head.comments
                    |> ropePrependTo tail.comments
            , syntax = head.syntax :: tail.syntax
            }
        )
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
            ropeEmpty
        )
        typeRecordFieldDefinition
        (manyWithComments
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map3
                    (\commentsBefore commentsWithExtraComma field ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo commentsWithExtraComma
                                |> ropePrependTo field.comments
                        , syntax = field.syntax
                        }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                        ropeEmpty
                    )
                    typeRecordFieldDefinition
                )
            )
        )


typeRecordFieldDefinition : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.RecordField))
typeRecordFieldDefinition =
    ParserFast.map6WithRange
        (\range commentsBeforeName name commentsAfterName commentsAfterColon value commentsAfterValue ->
            { comments =
                commentsBeforeName
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo commentsAfterColon
                    |> ropePrependTo value.comments
                    |> ropePrependTo commentsAfterValue
            , syntax = Elm.Syntax.Node.Node range ( name, value.syntax )
            }
        )
        whitespaceAndCommentsEndsPositivelyIndented
        nameLowercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.oneOf2OrSucceed
            (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
            (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
            ropeEmpty
        )
        type_
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        whitespaceAndCommentsEndsPositivelyIndented


typedTypeAnnotationWithoutArguments : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    ParserFast.map2WithRange
        (\range startName afterStartName ->
            let
                name : ( Elm.Syntax.ModuleName.ModuleName, String )
                name =
                    case afterStartName of
                        Nothing ->
                            ( [], startName )

                        Just ( qualificationAfterStartName, unqualified ) ->
                            ( startName :: qualificationAfterStartName, unqualified )
            in
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node range name) [])
            }
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


maybeDotNamesUppercaseTuple : Parser (Maybe ( List String, String ))
maybeDotNamesUppercaseTuple =
    ParserFast.map2OrSucceed
        (\firstName afterFirstName ->
            case afterFirstName of
                Nothing ->
                    Just ( [], firstName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( firstName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." nameUppercase)
        (ParserFast.lazy (\() -> maybeDotNamesUppercaseTuple))
        Nothing


typedTypeAnnotationWithArgumentsOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typedTypeAnnotationWithArgumentsOptimisticLayout =
    ParserFast.map3
        (\nameNode commentsAfterName argsReverse ->
            let
                (Elm.Syntax.Node.Node nameRange _) =
                    nameNode

                range : Elm.Syntax.Range.Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (Elm.Syntax.Node.Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments =
                commentsAfterName
                    |> ropePrependTo argsReverse.comments
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.TypeAnnotation.Typed nameNode (List.reverse argsReverse.syntax))
            }
        )
        (ParserFast.map2WithRange
            (\range startName afterStartName ->
                let
                    name : ( Elm.Syntax.ModuleName.ModuleName, String )
                    name =
                        case afterStartName of
                            Nothing ->
                                ( [], startName )

                            Just ( qualificationAfterStartName, unqualified ) ->
                                ( startName :: qualificationAfterStartName, unqualified )
                in
                Elm.Syntax.Node.Node range name
            )
            nameUppercase
            maybeDotNamesUppercaseTuple
        )
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments =
                            typeAnnotationResult.comments
                                |> ropePrependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


subExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
subExpression =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpression

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccess

                "[" ->
                    listOrGlslExpression

                "{" ->
                    recordExpressionFollowedByRecordAccess

                "." ->
                    recordAccessFunctionExpression

                "-" ->
                    negationOperation

                "'" ->
                    charLiteralExpression

                _ ->
                    referenceOrNumberExpression
        )


referenceOrNumberExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
referenceOrNumberExpression =
    ParserFast.oneOf3
        qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
        numberExpression


followedByMultiRecordAccess : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)) -> Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
followedByMultiRecordAccess beforeRecordAccesses =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (ParserFast.symbolFollowedBy "." nameLowercaseNode)
        beforeRecordAccesses
        (\fieldNode leftResult ->
            let
                (Elm.Syntax.Node.Node fieldRange _) =
                    fieldNode

                (Elm.Syntax.Node.Node leftRange _) =
                    leftResult.syntax
            in
            { comments = leftResult.comments
            , syntax =
                Elm.Syntax.Node.Node { start = leftRange.start, end = fieldRange.end }
                    (Elm.Syntax.Expression.RecordAccess leftResult.syntax fieldNode)
            }
        )
        Basics.identity


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


{-| [`Parser`](#Parser) for an [`Elm.Syntax.Expression.Expression`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Expression)
-}
expression : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression }
expression =
    expressionFollowedByOptimisticLayout


expressionFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
expressionFollowedByOptimisticLayout =
    extendedSubExpressionOptimisticLayout
        { afterCommitting = .extensionRightParser
        , validateRightPrecedence = Just
        }


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                { comments = ropeEmpty
                , syntax =
                    Elm.Syntax.Node.Node
                        -- TODO for v8: don't include extra end width (from bug in elm/parser) in range
                        { start = { row = range.start.row, column = range.start.column - 6 }
                        , end = { row = range.end.row, column = range.end.column + 2 }
                        }
                        (Elm.Syntax.Expression.GLSLExpression s)
                }
            )
            (ParserFast.loopUntil
                (ParserFast.symbol "|]" ())
                (ParserFast.oneOf2
                    (ParserFast.symbol "|" "|")
                    (ParserFast.while
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
                identity
            )
        )


listOrGlslExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
listOrGlslExpression =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.map2WithRange
            (\range commentsBefore elements ->
                { comments = commentsBefore |> ropePrependTo elements.comments
                , syntax =
                    Elm.Syntax.Node.Node
                        { start = { row = range.start.row, column = range.start.column - 1 }
                        , end = range.end
                        }
                        elements.syntax
                }
            )
            whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = ropeEmpty, syntax = Elm.Syntax.Expression.ListExpr [] })
                (ParserFast.map3
                    (\commentsBeforeHead head tail ->
                        { comments =
                            commentsBeforeHead
                                |> ropePrependTo head.comments
                                |> ropePrependTo tail.comments
                        , syntax = Elm.Syntax.Expression.ListExpr (head.syntax :: tail.syntax)
                        }
                    )
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                        ropeEmpty
                    )
                    expressionFollowedByOptimisticLayout
                    (positivelyIndentedFollowedBy
                        (manyWithComments
                            (ParserFast.symbolFollowedBy ","
                                (ParserFast.map3
                                    (\commentsBefore commentsWithExtraComma expressionResult ->
                                        { comments =
                                            commentsBefore
                                                |> ropePrependTo commentsWithExtraComma
                                                |> ropePrependTo expressionResult.comments
                                        , syntax = expressionResult.syntax
                                        }
                                    )
                                    whitespaceAndCommentsEndsPositivelyIndented
                                    (ParserFast.orSucceed
                                        (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                                        ropeEmpty
                                    )
                                    expressionFollowedByOptimisticLayout
                                    |> endsPositivelyIndented
                                )
                            )
                        )
                    )
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )


recordExpressionFollowedByRecordAccess : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordExpressionFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2WithRange
            (\range commentsBefore afterCurly ->
                { comments =
                    commentsBefore
                        |> ropePrependTo afterCurly.comments
                , syntax = Elm.Syntax.Node.Node (rangeMoveStartLeftByOneColumn range) afterCurly.syntax
                }
            )
            whitespaceAndCommentsEndsPositivelyIndented
            recordContentsCurlyEnd
            |> followedByMultiRecordAccess
        )


recordContentsCurlyEnd : Parser (WithComments Elm.Syntax.Expression.Expression)
recordContentsCurlyEnd =
    ParserFast.oneOf3
        (ParserFast.map5
            (\nameNode commentsAfterName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
                { comments =
                    commentsAfterName
                        |> ropePrependTo afterNameBeforeFields.comments
                        |> ropePrependTo tailFields.comments
                        |> ropePrependTo commentsBeforeClosingCurly
                , syntax =
                    case afterNameBeforeFields.syntax of
                        RecordUpdateFirstSetter firstField ->
                            Elm.Syntax.Expression.RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                        FieldsFirstValue firstFieldValue ->
                            Elm.Syntax.Expression.RecordExpr
                                (Elm.Syntax.Node.combine Tuple.pair nameNode firstFieldValue
                                    :: tailFields.syntax
                                )

                        FieldsFirstValuePunned () ->
                            Elm.Syntax.Expression.RecordExpr
                                (Elm.Syntax.Node.Node (nameNode |> Elm.Syntax.Node.range)
                                    ( nameNode
                                    , -- dummy
                                      Elm.Syntax.Node.Node
                                        { start = nameNode |> Elm.Syntax.Node.range |> .end
                                        , end = nameNode |> Elm.Syntax.Node.range |> .end
                                        }
                                        (Elm.Syntax.Expression.FunctionOrValue []
                                            (nameNode |> Elm.Syntax.Node.value)
                                        )
                                    )
                                    :: tailFields.syntax
                                )
                }
            )
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore setterResult ->
                            { comments = commentsBefore |> ropePrependTo setterResult.comments
                            , syntax = RecordUpdateFirstSetter setterResult.syntax
                            }
                        )
                        whitespaceAndCommentsEndsPositivelyIndented
                        recordSetterNodeWithLayout
                    )
                )
                (ParserFast.map2
                    (\commentsBefore maybeValueResult ->
                        case maybeValueResult of
                            Nothing ->
                                { comments = commentsBefore
                                , syntax = FieldsFirstValuePunned ()
                                }

                            Just expressionResult ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo expressionResult.comments
                                , syntax = FieldsFirstValue expressionResult.syntax
                                }
                    )
                    (ParserFast.oneOf2OrSucceed
                        (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
                        (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
                        ropeEmpty
                    )
                    (ParserFast.mapOrSucceed
                        Just
                        (expressionFollowedByOptimisticLayout
                            |> endsPositivelyIndented
                        )
                        Nothing
                    )
                    |> endsPositivelyIndented
                )
            )
            recordFields
            (whitespaceAndCommentsEndsPositivelyIndented |> ParserFast.followedBySymbol "}")
        )
        (ParserFast.symbol "}" { comments = ropeEmpty, syntax = Elm.Syntax.Expression.RecordExpr [] })
        -- prefixed comma
        (ParserFast.map2
            (\recordFieldsResult commentsAfterFields ->
                { comments =
                    recordFieldsResult.comments
                        |> ropePrependTo commentsAfterFields
                , syntax =
                    Elm.Syntax.Expression.RecordExpr recordFieldsResult.syntax
                }
            )
            recordFields
            (whitespaceAndCommentsEndsPositivelyIndented |> ParserFast.followedBySymbol "}")
        )


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Elm.Syntax.Node.Node Elm.Syntax.Expression.RecordSetter)
    | FieldsFirstValue (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
    | FieldsFirstValuePunned ()


recordFields : Parser (WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Expression.RecordSetter)))
recordFields =
    manyWithComments
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map3
                (\commentsBefore commentsWithExtraComma setterResult ->
                    { comments =
                        commentsBefore
                            |> ropePrependTo commentsWithExtraComma
                            |> ropePrependTo setterResult.comments
                    , syntax = setterResult.syntax
                    }
                )
                whitespaceAndCommentsEndsPositivelyIndented
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                    ropeEmpty
                )
                recordSetterNodeWithLayout
            )
        )


recordSetterNodeWithLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.RecordSetter))
recordSetterNodeWithLayout =
    ParserFast.map4WithRange
        (\range nameNode commentsAfterName commentsAfterEquals maybeValueResult ->
            -- This extra whitespace is just included for compatibility with earlier version
            -- TODO for v8: remove
            case maybeValueResult of
                Nothing ->
                    { comments =
                        commentsAfterName |> ropePrependTo commentsAfterEquals
                    , syntax =
                        Elm.Syntax.Node.Node range
                            ( nameNode
                            , -- dummy
                              Elm.Syntax.Node.Node
                                { start = nameNode |> Elm.Syntax.Node.range |> .end
                                , end = nameNode |> Elm.Syntax.Node.range |> .end
                                }
                                (Elm.Syntax.Expression.FunctionOrValue []
                                    (nameNode |> Elm.Syntax.Node.value)
                                )
                            )
                    }

                Just expressionResult ->
                    { comments =
                        commentsAfterName
                            |> ropePrependTo commentsAfterEquals
                            |> ropePrependTo expressionResult.comments
                    , syntax =
                        Elm.Syntax.Node.Node range
                            ( nameNode
                            , expressionResult.syntax
                            )
                    }
        )
        nameLowercaseNode
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.oneOf2OrSucceed
            (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
            (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
            ropeEmpty
        )
        (ParserFast.mapOrSucceed
            Just
            (expressionFollowedByOptimisticLayout
                |> endsPositivelyIndented
            )
            Nothing
        )


literalExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
literalExpression =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string)
            }
        )


charLiteralExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
charLiteralExpression =
    characterLiteralMapWithRange
        (\range char ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.CharLiteral char)
            }
        )


lambdaExpressionFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
lambdaExpressionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs commentsAfterArrow expressionResult ->
            let
                (Elm.Syntax.Node.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                commentsAfterBackslash
                    |> ropePrependTo firstArg.comments
                    |> ropePrependTo commentsAfterFirstArg
                    |> ropePrependTo secondUpArgs.comments
                    |> ropePrependTo commentsAfterArrow
                    |> ropePrependTo expressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (Elm.Syntax.Expression.LambdaExpression
                        { args = firstArg.syntax :: secondUpArgs.syntax
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.symbolFollowedBy "\\" whitespaceAndCommentsEndsPositivelyIndented)
        patternNotSpaceSeparated
        whitespaceAndCommentsEndsPositivelyIndented
        (untilWithComments
            (ParserFast.oneOf3
                (ParserFast.symbol "->" ())
                (ParserFast.symbol "=>" ())
                (ParserFast.symbol "." ())
            )
            (ParserFast.map2
                (\patternResult commentsAfter ->
                    { comments =
                        patternResult.comments
                            |> ropePrependTo commentsAfter
                    , syntax = patternResult.syntax
                    }
                )
                patternNotSpaceSeparated
                whitespaceAndCommentsEndsPositivelyIndented
            )
        )
        whitespaceAndCommentsEndsPositivelyIndented
        expressionFollowedByOptimisticLayout


caseExpressionFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
caseExpressionFollowedByOptimisticLayout =
    ParserFast.map4WithStartLocation
        (\start commentsAfterCase casedExpressionResult commentsAfterOf casesResult ->
            let
                ( firstCase, lastToSecondCase ) =
                    casesResult.syntax
            in
            { comments =
                commentsAfterCase
                    |> ropePrependTo casedExpressionResult.comments
                    |> ropePrependTo commentsAfterOf
                    |> ropePrependTo casesResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = start
                    , end =
                        case lastToSecondCase of
                            ( _, Elm.Syntax.Node.Node lastCaseExpressionRange _ ) :: _ ->
                                lastCaseExpressionRange.end

                            [] ->
                                let
                                    ( _, Elm.Syntax.Node.Node firstCaseExpressionRange _ ) =
                                        firstCase
                                in
                                firstCaseExpressionRange.end
                    }
                    (Elm.Syntax.Expression.CaseExpression
                        { expression = casedExpressionResult.syntax
                        , cases = firstCase :: List.reverse lastToSecondCase
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "case" whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        (positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "of" whitespaceAndCommentsEndsPositivelyIndented)
        )
        (ParserFast.withIndentSetToColumn caseStatementsFollowedByOptimisticLayout)


caseStatementsFollowedByOptimisticLayout : Parser (WithComments ( Elm.Syntax.Expression.Case, List Elm.Syntax.Expression.Case ))
caseStatementsFollowedByOptimisticLayout =
    ParserFast.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> ropePrependTo commentsAfterFirstCasePattern
                    |> ropePrependTo commentsAfterFirstCaseArrowRight
                    |> ropePrependTo firstCaseExpressionResult.comments
                    |> ropePrependTo lastToSecondCase.comments
            , syntax =
                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                , lastToSecondCase.syntax
                )
            }
        )
        pattern
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "->" whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        (manyWithCommentsReverse caseStatementFollowedByOptimisticLayout)


caseStatementFollowedByOptimisticLayout : Parser (WithComments Elm.Syntax.Expression.Case)
caseStatementFollowedByOptimisticLayout =
    topIndentedFollowedBy
        (ParserFast.map4
            (\patternResult commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    patternResult.comments
                        |> ropePrependTo commentsBeforeArrowRight
                        |> ropePrependTo commentsAfterArrowRight
                        |> ropePrependTo expr.comments
                , syntax = ( patternResult.syntax, expr.syntax )
                }
            )
            pattern
            whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.symbolFollowedBy "->" whitespaceAndCommentsEndsPositivelyIndented)
            expressionFollowedByOptimisticLayout
        )


letExpressionFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
letExpressionFollowedByOptimisticLayout =
    ParserFast.map3WithStartLocation
        (\start letDeclarationsResult commentsAfterIn expressionResult ->
            let
                (Elm.Syntax.Node.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                letDeclarationsResult.comments
                    |> ropePrependTo commentsAfterIn
                    |> ropePrependTo expressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (Elm.Syntax.Expression.LetExpression
                        { declarations = letDeclarationsResult.declarations
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.withIndentSetToColumn
            (ParserFast.keywordFollowedBy "let"
                (ParserFast.map2
                    (\commentsAfterLet letDeclarationsResult ->
                        { comments =
                            commentsAfterLet
                                |> ropePrependTo letDeclarationsResult.comments
                        , declarations = letDeclarationsResult.syntax
                        }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
        )
        -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        (positivelyIndentedPlusFollowedBy 2
            whitespaceAndCommentsEndsPositivelyIndented
        )
        expressionFollowedByOptimisticLayout


letDeclarationsIn : Parser (WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration)))
letDeclarationsIn =
    topIndentedFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                { comments =
                    headLetResult.comments
                        |> ropePrependTo commentsAfter
                        |> ropePrependTo tailLetResult.comments
                , syntax = headLetResult.syntax :: tailLetResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunctionFollowedByOptimisticLayout
                letDestructuringDeclarationFollowedByOptimisticLayout
            )
            whitespaceAndComments
            (untilWithComments
                (ParserFast.keyword "in" ())
                letBlockElementFollowedByOptimisticLayout
            )
        )


letBlockElementFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letBlockElementFollowedByOptimisticLayout =
    topIndentedFollowedBy
        (ParserFast.oneOf2
            letFunctionFollowedByOptimisticLayout
            letDestructuringDeclarationFollowedByOptimisticLayout
        )


letDestructuringDeclarationFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letDestructuringDeclarationFollowedByOptimisticLayout =
    ParserFast.map4
        (\patternResult commentsAfterPattern commentsAfterEquals expressionResult ->
            let
                (Elm.Syntax.Node.Node patternRange _) =
                    patternResult.syntax

                (Elm.Syntax.Node.Node destructuredExpressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                patternResult.comments
                    |> ropePrependTo commentsAfterPattern
                    |> ropePrependTo commentsAfterEquals
                    |> ropePrependTo expressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node { start = patternRange.start, end = destructuredExpressionRange.end }
                    (Elm.Syntax.Expression.LetDestructuring patternResult.syntax expressionResult.syntax)
            }
        )
        patternNotSpaceSeparated
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout


letFunctionFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letFunctionFollowedByOptimisticLayout =
    ParserFast.oneOf2
        (ParserFast.map6WithStartLocation
            (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
                case maybeSignature of
                    Nothing ->
                        let
                            (Elm.Syntax.Node.Node expressionRange _) =
                                expressionResult.syntax
                        in
                        { comments =
                            commentsAfterStartName
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo expressionResult.comments
                        , syntax =
                            Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                                (Elm.Syntax.Expression.LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                                            { name = startNameNode
                                            , arguments = arguments.syntax
                                            , expression = expressionResult.syntax
                                            }
                                    }
                                )
                        }

                    Just signature ->
                        let
                            (Elm.Syntax.Node.Node implementationNameRange _) =
                                signature.implementationName

                            (Elm.Syntax.Node.Node expressionRange _) =
                                expressionResult.syntax
                        in
                        { comments =
                            (commentsAfterStartName |> ropePrependTo signature.comments)
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo expressionResult.comments
                        , syntax =
                            Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                                (Elm.Syntax.Expression.LetFunction
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            (Elm.Syntax.Node.combine (\name value -> { name = name, typeAnnotation = value })
                                                startNameNode
                                                signature.typeAnnotation
                                            )
                                    , declaration =
                                        Elm.Syntax.Node.Node { start = implementationNameRange.start, end = expressionRange.end }
                                            { name = signature.implementationName
                                            , arguments = arguments.syntax
                                            , expression = expressionResult.syntax
                                            }
                                    }
                                )
                        }
            )
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.map4OrSucceed
                (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> ropePrependTo typeAnnotationResult.comments
                                |> ropePrependTo implementationName.comments
                                |> ropePrependTo afterImplementationName
                        , implementationName = implementationName.syntax
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNode
                )
                whitespaceAndCommentsEndsPositivelyIndented
                Nothing
            )
            parameterPatternsEqual
            whitespaceAndCommentsEndsPositivelyIndented
            expressionFollowedByOptimisticLayout
            |> ParserFast.validate
                (\result ->
                    let
                        (Elm.Syntax.Node.Node _ letDeclaration) =
                            result.syntax
                    in
                    case letDeclaration of
                        Elm.Syntax.Expression.LetDestructuring _ _ ->
                            True

                        Elm.Syntax.Expression.LetFunction letFunctionDeclaration ->
                            case letFunctionDeclaration.signature of
                                Nothing ->
                                    True

                                Just (Elm.Syntax.Node.Node _ signature) ->
                                    let
                                        (Elm.Syntax.Node.Node _ implementationName) =
                                            implementation.name

                                        (Elm.Syntax.Node.Node _ implementation) =
                                            letFunctionDeclaration.declaration

                                        (Elm.Syntax.Node.Node _ signatureName) =
                                            signature.name
                                    in
                                    implementationName == signatureName
                )
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeTypeAnnotation typeAnnotationResult commentsBetweenTypeAndName nameNode afterImplementationName arguments commentsAfterEqual result ->
                { comments =
                    commentsBeforeTypeAnnotation
                        |> ropePrependTo typeAnnotationResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    Elm.Syntax.Node.Node
                        { start = start
                        , end = result.syntax |> Elm.Syntax.Node.range |> .end
                        }
                        (Elm.Syntax.Expression.LetFunction
                            { documentation = Nothing
                            , signature =
                                Just
                                    (Elm.Syntax.Node.Node
                                        { start = start
                                        , end = typeAnnotationResult.syntax |> Elm.Syntax.Node.range |> .end
                                        }
                                        { name =
                                            -- dummy
                                            Elm.Syntax.Node.Node
                                                { start = start, end = start }
                                                (nameNode |> Elm.Syntax.Node.value)
                                        , typeAnnotation = typeAnnotationResult.syntax
                                        }
                                    )
                            , declaration =
                                Elm.Syntax.Node.Node
                                    { start = nameNode |> Elm.Syntax.Node.range |> .start
                                    , end = result.syntax |> Elm.Syntax.Node.range |> .end
                                    }
                                    { name = nameNode
                                    , arguments = arguments.syntax
                                    , expression = result.syntax
                                    }
                            }
                        )
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndCommentsEndsPositivelyIndented)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNode
            whitespaceAndCommentsEndsPositivelyIndented
            parameterPatternsEqual
            whitespaceAndCommentsEndsPositivelyIndented
            expressionFollowedByOptimisticLayout
        )


numberExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
numberExpression =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Floatable n)
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Integer n)
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Hex n)
            }
        )


ifBlockExpressionFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
ifBlockExpressionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterIf condition commentsAfterThen ifTrue commentsAfterElse ifFalse ->
            let
                (Elm.Syntax.Node.Node ifFalseRange _) =
                    ifFalse.syntax
            in
            { comments =
                commentsAfterIf
                    |> ropePrependTo condition.comments
                    |> ropePrependTo commentsAfterThen
                    |> ropePrependTo ifTrue.comments
                    |> ropePrependTo commentsAfterElse
                    |> ropePrependTo ifFalse.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = start
                    , end = ifFalseRange.end
                    }
                    (Elm.Syntax.Expression.IfBlock
                        condition.syntax
                        ifTrue.syntax
                        ifFalse.syntax
                    )
            }
        )
        (ParserFast.keywordFollowedBy "if" whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        (positivelyIndentedFollowedBy
            (ParserFast.oneOf2
                (ParserFast.keywordFollowedBy "then" whitespaceAndCommentsEndsPositivelyIndented)
                (ParserFast.keywordFollowedBy "->" whitespaceAndCommentsEndsPositivelyIndented)
            )
        )
        expressionFollowedByOptimisticLayout
        (positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "else" whitespaceAndCommentsEndsPositivelyIndented)
        )
        expressionFollowedByOptimisticLayout


negationOperation : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
negationOperation =
    ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        negationAfterMinus

                    -- not "\n" or "\r" since expressions are always indented
                    "(" ->
                        negationAfterMinus

                    ")" ->
                        negationAfterMinus

                    -- from the end of a multiline comment
                    "}" ->
                        negationAfterMinus

                    -- TODO only for tests
                    "" ->
                        negationAfterMinus

                    _ ->
                        ParserFast.problem
            )
        )


negationAfterMinus : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
negationAfterMinus =
    ParserFast.map
        (\subExpressionResult ->
            let
                (Elm.Syntax.Node.Node subExpressionRange _) =
                    subExpressionResult.syntax
            in
            { comments = subExpressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start =
                        { row = subExpressionRange.start.row
                        , column = subExpressionRange.start.column - 1
                        }
                    , end = subExpressionRange.end
                    }
                    (Elm.Syntax.Expression.Negation subExpressionResult.syntax)
            }
        )
        subExpression


qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node range
                    (case after of
                        Nothing ->
                            Elm.Syntax.Expression.FunctionOrValue [] firstName

                        Just ( qualificationAfter, unqualified ) ->
                            Elm.Syntax.Expression.FunctionOrValue (firstName :: qualificationAfter) unqualified
                    )
            }
        )
        nameUppercase
        maybeDotReferenceExpressionTuple
        |> followedByMultiRecordAccess


maybeDotReferenceExpressionTuple : Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                ( firstName :: qualificationAfter, unqualified )
                    )
                    nameUppercase
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\name -> Just ( [], name ))
                nameLowercase
            )
        )
        Nothing


unqualifiedFunctionReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
unqualifiedFunctionReferenceExpressionFollowedByRecordAccess =
    nameLowercaseMapWithRange
        (\range unqualified ->
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Expression.FunctionOrValue [] unqualified)
            }
        )
        |> followedByMultiRecordAccess


recordAccessFunctionExpression : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordAccessFunctionExpression =
    ParserFast.symbolFollowedBy "."
        (nameLowercaseMapWithRange
            (\range field ->
                { comments = ropeEmpty
                , syntax =
                    Elm.Syntax.Node.Node (range |> rangeMoveStartLeftByOneColumn)
                        (Elm.Syntax.Expression.RecordAccessFunction ("." ++ field))
                }
            )
        )


rangeMoveStartLeftByOneColumn : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range
rangeMoveStartLeftByOneColumn range =
    { start = { row = range.start.row, column = range.start.column - 1 }
    , end = range.end
    }


tupledExpressionIfNecessaryFollowedByRecordAccess : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
tupledExpressionIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf3
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = ropeEmpty
                    , syntax =
                        Elm.Syntax.Node.Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            Elm.Syntax.Expression.UnitExpr
                    }
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            tupledExpressionInnerAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node
                    { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                    , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                    }
                    (Elm.Syntax.Expression.PrefixOperator operator)
            }
        )
        isOperatorSymbolCharAsString
        isAllowedOperatorToken
        ")"


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.map3WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> ropePrependTo firstPart.comments
                    |> ropePrependTo tailParts.comments
            , syntax =
                case tailParts.syntax of
                    TupledParenthesized () () ->
                        Elm.Syntax.Node.Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (Elm.Syntax.Expression.ParenthesizedExpression firstPart.syntax)

                    TupledTwoOrThree secondPart maybeThirdPart ->
                        Elm.Syntax.Node.Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case maybeThirdPart of
                                Nothing ->
                                    Elm.Syntax.Expression.TupledExpression [ firstPart.syntax, secondPart ]

                                Just thirdPart ->
                                    Elm.Syntax.Expression.TupledExpression [ firstPart.syntax, secondPart, thirdPart ]
                            )
            }
        )
        whitespaceAndCommentsEndsPositivelyIndented
        expressionFollowedByOptimisticLayout
        (positivelyIndentedFollowedBy
            (ParserFast.oneOf2
                (ParserFast.symbol ")"
                    { comments = ropeEmpty, syntax = TupledParenthesized () () }
                )
                (ParserFast.symbolFollowedBy ","
                    (ParserFast.map3
                        (\commentsBefore partResult maybeThirdPart ->
                            { comments =
                                commentsBefore
                                    |> ropePrependTo partResult.comments
                                    |> ropePrependTo maybeThirdPart.comments
                            , syntax = TupledTwoOrThree partResult.syntax maybeThirdPart.syntax
                            }
                        )
                        whitespaceAndCommentsEndsPositivelyIndented
                        expressionFollowedByOptimisticLayout
                        (positivelyIndentedFollowedBy
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore partResult ->
                                            { comments =
                                                commentsBefore
                                                    |> ropePrependTo partResult.comments
                                            , syntax = Just partResult.syntax
                                            }
                                        )
                                        whitespaceAndCommentsEndsPositivelyIndented
                                        expressionFollowedByOptimisticLayout
                                        |> endsPositivelyIndented
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        |> followedByMultiRecordAccess


type Tupled
    = TupledParenthesized () ()
    | TupledTwoOrThree (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression) (Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))



---


extendedSubExpressionOptimisticLayout :
    { info_
        | afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
        , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
extendedSubExpressionOptimisticLayout info =
    ParserFast.map2
        (\expressionResult maybeCases ->
            case maybeCases of
                Nothing ->
                    expressionResult

                Just cases ->
                    { comments =
                        expressionResult.comments
                            |> ropePrependTo cases.comments
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start =
                                expressionResult.syntax
                                    |> Elm.Syntax.Node.range
                                    |> .start
                            , end = cases.end
                            }
                            (Elm.Syntax.Expression.CaseExpression
                                { expression = expressionResult.syntax
                                , cases = cases.cases
                                }
                            )
                    }
        )
        (ParserFast.loopWhileSucceedsOntoResultFromParser
            (positivelyIndentedFollowedBy
                (infixOperatorAndThen info)
            )
            subExpressionMaybeAppliedFollowedByOptimisticLayout
            (\extensionRightResult leftNodeWithComments ->
                { comments =
                    leftNodeWithComments.comments
                        |> ropePrependTo extensionRightResult.comments
                , syntax =
                    leftNodeWithComments.syntax
                        |> applyExtensionRight extensionRightResult.syntax
                }
            )
            Basics.identity
        )
        (ParserFast.orSucceed
            (ParserFast.keywordFollowedBy "case"
                (ParserFast.map2
                    (\commentsAfterCase casesResult ->
                        let
                            ( firstCase, lastToSecondCase ) =
                                casesResult.syntax
                        in
                        Just
                            { comments =
                                commentsAfterCase
                                    |> ropePrependTo casesResult.comments
                            , end =
                                case lastToSecondCase of
                                    ( _, Elm.Syntax.Node.Node lastCaseExpressionRange _ ) :: _ ->
                                        lastCaseExpressionRange.end

                                    [] ->
                                        let
                                            ( _, Elm.Syntax.Node.Node firstCaseExpressionRange _ ) =
                                                firstCase
                                        in
                                        firstCaseExpressionRange.end
                            , cases = firstCase :: List.reverse lastToSecondCase
                            }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.withIndentSetToColumn
                        (ParserFast.lazy (\() -> caseStatementsFollowedByOptimisticLayout))
                    )
                )
            )
            Nothing
        )


extensionRightParser :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , direction : Elm.Syntax.Infix.InfixDirection
    , symbol : String
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
extensionRightParser extensionRightInfo =
    ParserFast.map2
        (\commentsBefore right ->
            { comments = commentsBefore |> ropePrependTo right.comments
            , syntax =
                ExtendRightByOperation
                    { symbol = extensionRightInfo.symbol
                    , direction = extensionRightInfo.direction
                    , expression = right.syntax
                    }
            }
        )
        whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.lazy
            (\() -> extendedSubExpressionOptimisticLayout extensionRightInfo)
        )


infixOperatorAndThen :
    { info_
        | afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
        , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
infixOperatorAndThen extensionRightConstraints =
    let
        toResult : InfixOperatorInfo -> Maybe InfixOperatorInfo
        toResult =
            extensionRightConstraints.validateRightPrecedence

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
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ToResultAndThen
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


subExpressionMaybeAppliedFollowedByOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
subExpressionMaybeAppliedFollowedByOptimisticLayout =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpressionOptimisticLayout

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied

                "[" ->
                    listOrGlslExpressionOptimisticLayout

                "{" ->
                    recordExpressionFollowedByRecordAccessMaybeApplied

                "c" ->
                    caseOrUnqualifiedReferenceExpressionMaybeApplied

                "\\" ->
                    lambdaExpressionFollowedByOptimisticLayout

                "l" ->
                    letOrUnqualifiedReferenceExpressionMaybeApplied

                "i" ->
                    ifOrUnqualifiedReferenceExpressionMaybeApplied

                "." ->
                    recordAccessFunctionExpressionMaybeApplied

                "-" ->
                    negationOperationOptimisticLayout

                "'" ->
                    charLiteralExpressionOptimisticLayout

                _ ->
                    referenceOrNumberExpressionMaybeApplied
        )


negationOperationOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
negationOperationOptimisticLayout =
    negationOperation |> followedByOptimisticLayout


charLiteralExpressionOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
charLiteralExpressionOptimisticLayout =
    charLiteralExpression |> followedByOptimisticLayout


literalExpressionOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
literalExpressionOptimisticLayout =
    literalExpression |> followedByOptimisticLayout


listOrGlslExpressionOptimisticLayout : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
listOrGlslExpressionOptimisticLayout =
    listOrGlslExpression |> followedByOptimisticLayout


followedByOptimisticLayout : Parser (WithComments a) -> Parser (WithComments a)
followedByOptimisticLayout parser =
    ParserFast.map2
        (\result commentsAfter ->
            { comments = result.comments |> ropePrependTo commentsAfter
            , syntax = result.syntax
            }
        )
        parser
        whitespaceAndComments


recordAccessFunctionExpressionMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordAccessFunctionExpressionMaybeApplied =
    recordAccessFunctionExpression |> followedByMultiArgumentApplication


recordExpressionFollowedByRecordAccessMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordExpressionFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if record access
    recordExpressionFollowedByRecordAccess
        |> followedByMultiArgumentApplication


tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if not parenthesized
    tupledExpressionIfNecessaryFollowedByRecordAccess
        |> followedByMultiArgumentApplication


caseOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
caseOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        caseExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


letOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
letOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        letExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


ifOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
ifOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        ifBlockExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


referenceOrNumberExpressionMaybeApplied : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
referenceOrNumberExpressionMaybeApplied =
    ParserFast.oneOf3
        (qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (numberExpression |> followedByOptimisticLayout)


followedByMultiArgumentApplication : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)) -> Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
followedByMultiArgumentApplication appliedExpressionParser =
    ParserFast.map3
        (\leftExpressionResult commentsBeforeExtension maybeArgsReverse ->
            { comments =
                leftExpressionResult.comments
                    |> ropePrependTo commentsBeforeExtension
                    |> ropePrependTo maybeArgsReverse.comments
            , syntax =
                case maybeArgsReverse.syntax of
                    [] ->
                        leftExpressionResult.syntax

                    (Elm.Syntax.Node.Node lastArgRange _) :: _ ->
                        let
                            (Elm.Syntax.Node.Node leftRange _) =
                                leftExpressionResult.syntax
                        in
                        Elm.Syntax.Node.Node { start = leftRange.start, end = lastArgRange.end }
                            (Elm.Syntax.Expression.Application
                                (leftExpressionResult.syntax :: List.reverse maybeArgsReverse.syntax)
                            )
            }
        )
        appliedExpressionParser
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfter ->
                        { comments = arg.comments |> ropePrependTo commentsAfter
                        , syntax = arg.syntax
                        }
                    )
                    subExpression
                    whitespaceAndComments
                )
            )
        )


applyExtensionRight : ExtensionRight -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
applyExtensionRight (ExtendRightByOperation operation) leftNode =
    let
        (Elm.Syntax.Node.Node leftRange _) =
            leftNode

        (Elm.Syntax.Node.Node rightExpressionRange _) =
            operation.expression
    in
    Elm.Syntax.Node.Node { start = leftRange.start, end = rightExpressionRange.end }
        (Elm.Syntax.Expression.OperatorApplication operation.symbol
            operation.direction
            leftNode
            operation.expression
        )


type alias InfixOperatorInfo =
    { leftPrecedence : Int
    , symbol : String
    , extensionRightParser : Parser (WithComments ExtensionRight)
    }


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = Elm.Syntax.Infix.Left
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence > leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = Elm.Syntax.Infix.Right
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
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
                    if rightInfo.leftPrecedence == leftPrecedence then
                        ParserFast.problem

                    else
                        rightInfo.extensionRightParser
            , direction = Elm.Syntax.Infix.Non
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


type ExtensionRight
    = ExtendRightByOperation
        { symbol : String
        , direction : Elm.Syntax.Infix.InfixDirection
        , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }


{-| [`Parser`](#Parser) for an [`Elm.Syntax.Pattern.Pattern`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern#Pattern)
-}
pattern : Parser { comments : Comments, syntax : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern }
pattern =
    ParserFast.map2
        (\leftMaybeConsed maybeAsExtension ->
            case maybeAsExtension of
                Nothing ->
                    leftMaybeConsed

                Just asExtension ->
                    { comments =
                        leftMaybeConsed.comments
                            |> ropePrependTo asExtension.comments
                    , syntax =
                        Elm.Syntax.Node.combine Elm.Syntax.Pattern.AsPattern leftMaybeConsed.syntax asExtension.syntax
                    }
        )
        (ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
            (ParserFast.map2
                (\startPatternResult commentsAfter ->
                    { comments = startPatternResult.comments |> ropePrependTo commentsAfter
                    , syntax = startPatternResult.syntax
                    }
                )
                (ParserFast.lazy (\() -> composablePattern))
                whitespaceAndCommentsEndsPositivelyIndented
            )
            (ParserFast.symbolFollowedBy "::"
                (ParserFast.map3
                    (\commentsAfterCons patternResult commentsAfterTailSubPattern ->
                        { comments =
                            commentsAfterCons
                                |> ropePrependTo patternResult.comments
                                |> ropePrependTo commentsAfterTailSubPattern
                        , syntax = patternResult.syntax
                        }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.lazy (\() -> composablePattern))
                    whitespaceAndCommentsEndsPositivelyIndented
                )
            )
            (\consed afterCons ->
                { comments = consed.comments |> ropePrependTo afterCons.comments
                , syntax =
                    Elm.Syntax.Node.combine Elm.Syntax.Pattern.UnConsPattern consed.syntax afterCons.syntax
                }
            )
        )
        (ParserFast.orSucceed
            (ParserFast.keywordFollowedBy "as"
                (ParserFast.map2
                    (\commentsAfterAs name ->
                        Just
                            { comments = commentsAfterAs
                            , syntax = name
                            }
                    )
                    whitespaceAndCommentsEndsPositivelyIndented
                    nameLowercaseNode
                )
            )
            Nothing
        )


parensPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
parensPattern =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                { comments =
                    commentsBeforeHead
                        |> ropePrependTo contentResult.comments
                , syntax =
                    Elm.Syntax.Node.Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                        contentResult.syntax
                }
            )
            whitespaceAndCommentsEndsPositivelyIndented
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Elm.Syntax.Pattern.UnitPattern })
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        { comments =
                            headResult.comments
                                |> ropePrependTo commentsAfterHead
                                |> ropePrependTo tailResult.comments
                        , syntax =
                            case tailResult.syntax of
                                Nothing ->
                                    Elm.Syntax.Pattern.ParenthesizedPattern headResult.syntax

                                Just secondAndMaybeThirdPart ->
                                    case secondAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            Elm.Syntax.Pattern.TuplePattern [ headResult.syntax, secondAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            Elm.Syntax.Pattern.TuplePattern [ headResult.syntax, secondAndMaybeThirdPart.secondPart, thirdPart ]
                        }
                    )
                    pattern
                    whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    { comments =
                                        commentsBefore
                                            |> ropePrependTo secondPart.comments
                                            |> ropePrependTo commentsAfter
                                            |> ropePrependTo maybeThirdPart.comments
                                    , syntax = Just { maybeThirdPart = maybeThirdPart.syntax, secondPart = secondPart.syntax }
                                    }
                                )
                                whitespaceAndCommentsEndsPositivelyIndented
                                pattern
                                whitespaceAndCommentsEndsPositivelyIndented
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                { comments =
                                                    commentsBefore
                                                        |> ropePrependTo thirdPart.comments
                                                        |> ropePrependTo commentsAfter
                                                , syntax = Just thirdPart.syntax
                                                }
                                            )
                                            whitespaceAndCommentsEndsPositivelyIndented
                                            pattern
                                            whitespaceAndCommentsEndsPositivelyIndented
                                            |> ParserFast.followedBySymbol ")"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


varPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
varPattern =
    nameLowercaseMapWithRange
        (\range var ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.VarPattern var)
            }
        )


numberPart : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n -> { comments = ropeEmpty, syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.IntPattern n) })
        (\range n -> { comments = ropeEmpty, syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.HexPattern n) })


charPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
charPattern =
    characterLiteralMapWithRange
        (\range char ->
            { comments = ropeEmpty, syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.CharPattern char) }
        )


listPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = Elm.Syntax.Node.Node range patternListEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> ropePrependTo elements.comments
                    , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.ListPattern elements.syntax)
                    }
        )
        (ParserFast.symbolFollowedBy "[" whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map4
                (\commentsBeforeHead head commentsAfterHead tail ->
                    Just
                        { comments =
                            commentsBeforeHead
                                |> ropePrependTo head.comments
                                |> ropePrependTo tail.comments
                                |> ropePrependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                    ropeEmpty
                )
                pattern
                whitespaceAndCommentsEndsPositivelyIndented
                (manyWithComments
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore commentsWithExtraComma v commentsAfter ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo commentsWithExtraComma
                                        |> ropePrependTo v.comments
                                        |> ropePrependTo commentsAfter
                                , syntax = v.syntax
                                }
                            )
                            whitespaceAndCommentsEndsPositivelyIndented
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                                ropeEmpty
                            )
                            pattern
                            whitespaceAndCommentsEndsPositivelyIndented
                        )
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : Elm.Syntax.Pattern.Pattern
patternListEmpty =
    Elm.Syntax.Pattern.ListPattern []


composablePattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
composablePattern =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithConsumeArgs
        allPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


patternNotSpaceSeparated : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
patternNotSpaceSeparated =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


allPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range Elm.Syntax.Pattern.AllPattern
            }
        )


stringPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
stringPattern =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = ropeEmpty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.StringPattern string)
            }
        )


qualifiedPatternWithConsumeArgs : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3
        (\(Elm.Syntax.Node.Node nameRange name) afterStartName argsReverse ->
            let
                range : Elm.Syntax.Range.Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (Elm.Syntax.Node.Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments = afterStartName |> ropePrependTo argsReverse.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Pattern.NamedPattern
                        name
                        (List.reverse argsReverse.syntax)
                    )
            }
        )
        qualifiedNameRefNode
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfterArg ->
                        { comments = arg.comments |> ropePrependTo commentsAfterArg
                        , syntax = arg.syntax
                        }
                    )
                    patternNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


qualifiedNameRefNode : Parser (Elm.Syntax.Node.Node Elm.Syntax.Pattern.QualifiedNameRef)
qualifiedNameRefNode =
    ParserFast.map2WithRange
        (\range firstName after ->
            Elm.Syntax.Node.Node range
                (case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
                )
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = ropeEmpty
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Pattern.NamedPattern
                        (case after of
                            Nothing ->
                                { moduleName = [], name = firstName }

                            Just ( qualificationAfter, unqualified ) ->
                                { moduleName = firstName :: qualificationAfter, name = unqualified }
                        )
                        []
                    )
            }
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


recordPattern : Parser (WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            { comments = commentsBeforeElements |> ropePrependTo elements.comments
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.RecordPattern elements.syntax)
            }
        )
        (ParserFast.symbolFollowedBy "{" whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            (ParserFast.symbol "}" { comments = ropeEmpty, syntax = [] })
            (ParserFast.map4
                (\commentsBeforeHead head commentsAfterHead tail ->
                    { comments =
                        commentsBeforeHead
                            |> ropePrependTo commentsAfterHead
                            |> ropePrependTo tail.comments
                    , syntax = head :: tail.syntax
                    }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                    ropeEmpty
                )
                nameLowercaseNode
                whitespaceAndCommentsEndsPositivelyIndented
                (manyWithComments
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBeforeName commentsWithExtraComma name afterName ->
                                { comments =
                                    commentsBeforeName
                                        |> ropePrependTo commentsWithExtraComma
                                        |> ropePrependTo afterName
                                , syntax = name
                                }
                            )
                            whitespaceAndCommentsEndsPositivelyIndented
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," whitespaceAndCommentsEndsPositivelyIndented)
                                ropeEmpty
                            )
                            nameLowercaseNode
                            whitespaceAndCommentsEndsPositivelyIndented
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
        )


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


escapedCharValueMap : (Char -> res) -> Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf7
        (ParserFast.symbol "'" (charToRes '\''))
        (ParserFast.symbol "\"" (charToRes '"'))
        (ParserFast.symbol "n" (charToRes '\n'))
        (ParserFast.symbol "t" (charToRes '\t'))
        -- even though elm-format will change \r to a unicode version. When you don't use elm-format, this will not happen.
        (ParserFast.symbol "r" (charToRes '\u{000D}'))
        (ParserFast.symbol "\\" (charToRes '\\'))
        (ParserFast.symbolFollowedBy "u{"
            (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    charToRes (Char.fromCode (hexStringToInt hex))
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserFast.followedBySymbol "}"
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
        { exponent = 0, result = 0 }
        string
        |> .result


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


characterLiteralMapWithRange : (Elm.Syntax.Range.Range -> Char -> res) -> Parser res
characterLiteralMapWithRange rangeAndCharToRes =
    ParserFast.symbolFollowedBy "'"
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity))
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            ParserFast.anyChar
            |> ParserFast.followedBySymbol "'"
        )


singleOrTripleQuotedStringLiteralMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> Parser res
singleOrTripleQuotedStringLiteralMapWithRange rangeAndStringToRes =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            (ParserFast.symbolFollowedBy "\"\""
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"" ())
        (ParserFast.oneOf2
            (ParserFast.whileAtLeast1WithoutLinebreak (\c -> c /= '"' && c /= '\\' && not (Char.Extra.isUtf16Surrogate c)))
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"\"\"" ())
        (ParserFast.oneOf3
            (ParserFast.symbol "\"" "\"")
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.while (\c -> c /= '"' && c /= '\\' && not (Char.Extra.isUtf16Surrogate c)))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


{-| [`Parser`](#Parser) for a name used for
record field names and unqualified function/value references
-}
nameLowercase : Parser String
nameLowercase =
    ParserFast.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


nameLowercaseNode : Parser (Elm.Syntax.Node.Node String)
nameLowercaseNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


nameLowercaseMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> Parser res
nameLowercaseMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak
        rangeAndNameToResult
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNotInfixNode : Parser (Elm.Syntax.Node.Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        (\name ->
            case name of
                "infix" ->
                    False

                nameNotInfix ->
                    nameNotInfix |> isNotReserved
        )


{-| [`Parser`](#Parser) for a name used for
type names, variant names, record type alias constructor function names and module names
-}
nameUppercase : Parser String
nameUppercase =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


nameUppercaseMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> Parser res
nameUppercaseMapWithRange rangeAndNameToRes =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndNameToRes
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


nameUppercaseNode : Parser (Elm.Syntax.Node.Node String)
nameUppercaseNode =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


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


{-| [`Parser`](#Parser) for a `--...` comment
-}
singleLineComment : Parser (Elm.Syntax.Node.Node String)
singleLineComment =
    ParserFast.symbolFollowedBy "--"
        (ParserFast.whileMapWithRange
            (\c -> c /= '\u{000D}' && c /= '\n' && not (Char.Extra.isUtf16Surrogate c))
            (\range content ->
                Elm.Syntax.Node.Node
                    { start = { row = range.start.row, column = range.start.column - 2 }
                    , end =
                        { row = range.start.row
                        , column = range.end.column
                        }
                    }
                    ("--" ++ content)
            )
        )


{-| [`Parser`](#Parser) for a `{-...-}` comment,
also verifying that it itself isn't a documentation comment
-}
multiLineComment : Parser (Elm.Syntax.Node.Node String)
multiLineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice (offset + 2) (offset + 3) source of
                "|" ->
                    ParserFast.problem

                _ ->
                    multiLineCommentNoCheck
        )


multiLineCommentNoCheck : Parser (Elm.Syntax.Node.Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiCommentMapWithRange Elm.Syntax.Node.Node
        ( '{', "-" )
        ( '-', "}" )


{-| [`Parser`](#Parser) for the space between syntax tokens
which can contain spaces, linebreaks, [`multiLineComment`](#multiLineComment)s
and [`singleLineComment`](#singleLineComment)s
-}
whitespaceAndComments : Parser Comments
whitespaceAndComments =
    ParserFast.skipWhileWhitespaceBacktrackableFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserFast.offsetSourceAndThenOrSucceed
            (\offset source ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to empty
                        Just fromSingleLineCommentNode

                    "{-" ->
                        Just fromMultilineCommentNodeOrEmptyOnProblem

                    _ ->
                        Nothing
            )
            ropeEmpty
        )


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserFast.map2OrSucceed
        (\comment commentsAfter ->
            ropeOne comment |> ropeFilledPrependTo commentsAfter
        )
        (multiLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        ropeEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    ParserFast.map2
        (\content commentsAfter ->
            ropeOne content |> ropeFilledPrependTo commentsAfter
        )
        (singleLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : Parser Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            singleLineComment
            multiLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        ropeEmpty
        (\right soFar -> soFar |> ropePrependToFilled (ropeOne right))
        identity


whitespaceAndCommentsEndsPositivelyIndented : Parser Comments
whitespaceAndCommentsEndsPositivelyIndented =
    whitespaceAndComments |> endsPositivelyIndented


endsPositivelyIndented : Parser a -> Parser a
endsPositivelyIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent ->
            (column > 1)
                && (indent |> List.all (\nestedIndent -> column /= nestedIndent))
        )
        parser


{-| Check that the indentation of an already parsed token
would be valid
-}
positivelyIndentedPlusFollowedBy : Int -> Parser a -> Parser a
positivelyIndentedPlusFollowedBy extraIndent nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            case indent of
                [] ->
                    if column > 1 + extraIndent then
                        nextParser

                    else
                        ParserFast.problem

                highestIndent :: lowerIndents ->
                    if
                        (column > 1)
                            && (column /= highestIndent + extraIndent)
                            && (lowerIndents |> List.all (\nestedIndent -> column /= nestedIndent))
                    then
                        nextParser

                    else
                        ParserFast.problem
        )


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if
                (column > 1)
                    && (indent |> List.all (\nestedIndent -> column /= nestedIndent))
            then
                nextParser

            else
                ParserFast.problem
        )


whitespaceAndCommentsEndsTopIndentedFollowedByComments : Parser Comments -> Parser Comments
whitespaceAndCommentsEndsTopIndentedFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> ropePrependTo afterComments
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedByWithComments : Parser (WithComments syntax) -> Parser (WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore |> ropePrependTo after.comments
            , syntax = after.syntax
            }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedBy : Parser syntax -> Parser (WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore, syntax = after }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndented : Parser Comments
whitespaceAndCommentsEndsTopIndented =
    whitespaceAndComments |> endsTopIndented


endsTopIndented : Parser a -> Parser a
endsTopIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent ->
            case indent of
                [] ->
                    column == 1

                highestIndent :: _ ->
                    column - highestIndent == 0
        )
        parser


topIndentedFollowedBy : Parser a -> Parser a
topIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            case indent of
                [] ->
                    if column == 1 then
                        nextParser

                    else
                        ParserFast.problem

                highestIndent :: _ ->
                    if column - highestIndent == 0 then
                        nextParser

                    else
                        ParserFast.problem
        )


surroundedByWhitespaceAndCommentsEndsPositivelyIndented : Parser (WithComments b) -> Parser (WithComments b)
surroundedByWhitespaceAndCommentsEndsPositivelyIndented x =
    ParserFast.map3
        (\before v after ->
            { comments =
                before
                    |> ropePrependTo v.comments
                    |> ropePrependTo after
            , syntax = v.syntax
            }
        )
        whitespaceAndCommentsEndsPositivelyIndented
        x
        whitespaceAndCommentsEndsPositivelyIndented


type alias WithComments res =
    { comments : Comments, syntax : res }


{-| A bag of comment nodes.
Each comment string contains the `{-`, `-}` or `--`.

Access with [`commentsToList`](#commentsToList)

-}
type alias Comments =
    Maybe (RopeFilled (Elm.Syntax.Node.Node String))


{-| Extract a list of comment nodes from parse result [`Comments`](#Comments)
-}
commentsToList : Comments -> List (Elm.Syntax.Node.Node String)
commentsToList comments =
    ropeToList comments


untilWithComments : ParserFast.Parser () -> ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
untilWithComments end element =
    ParserFast.loopUntil
        end
        element
        ( ropeEmpty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> ropePrependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


manyWithComments : ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
manyWithComments p =
    ParserFast.loopWhileSucceeds p
        ( ropeEmpty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> ropePrependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


{-| Same as `manyWithComments` except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithCommentsReverse : ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
manyWithCommentsReverse p =
    ParserFast.loopWhileSucceeds p
        { comments = ropeEmpty, syntax = [] }
        (\pResult soFar ->
            { comments = soFar.comments |> ropePrependTo pResult.comments
            , syntax = pResult.syntax :: soFar.syntax
            }
        )
        (\result -> result)


type alias Rope a =
    Maybe (RopeFilled a)


{-| Constantly appending lists of comments when combining parse can get expensive,
so we summarize everything in this temporary structure
and only convert to a list when we're done.

Inspired by [miniBill/elm-rope](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)

-}
type RopeFilled a
    = RopeLeaf a ()
    | RopeBranch2 (RopeFilled a) (RopeFilled a)


ropeEmpty : Rope a_
ropeEmpty =
    Nothing


ropeOne : a -> RopeFilled a
ropeOne onlyElement =
    RopeLeaf onlyElement ()


ropeFilledPrependTo : Rope a -> RopeFilled a -> Rope a
ropeFilledPrependTo right leftLikelyFilled =
    Just
        (case right of
            Nothing ->
                leftLikelyFilled

            Just rightLikelyFilled ->
                RopeBranch2 leftLikelyFilled rightLikelyFilled
        )


ropePrependToFilled : RopeFilled a -> Rope a -> Rope a
ropePrependToFilled rightLikelyFilled left =
    Just
        (case left of
            Nothing ->
                rightLikelyFilled

            Just leftLikelyFilled ->
                RopeBranch2 leftLikelyFilled rightLikelyFilled
        )


ropePrependTo : Rope a -> Rope a -> Rope a
ropePrependTo right left =
    case left of
        Nothing ->
            right

        Just leftLikelyFilled ->
            case right of
                Nothing ->
                    left

                Just rightLikelyFilled ->
                    Just (RopeBranch2 leftLikelyFilled rightLikelyFilled)


ropeToList : Rope a -> List a
ropeToList rope =
    case rope of
        Nothing ->
            []

        Just ropeLikelyFilled ->
            ropeLikelyFilledToListInto [] ropeLikelyFilled


ropeLikelyFilledToListInto : List a -> RopeFilled a -> List a
ropeLikelyFilledToListInto initialAcc ropeLikelyFilled =
    -- IGNORE TCO
    case ropeLikelyFilled of
        RopeLeaf onlyElement () ->
            onlyElement :: initialAcc

        RopeBranch2 left right ->
            ropeLikelyFilledToListInto
                (ropeLikelyFilledToListInto
                    initialAcc
                    right
                )
                left
