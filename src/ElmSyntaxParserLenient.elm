module ElmSyntaxParserLenient exposing
    ( Parser, run, module_
    , moduleName, functionName, typeName, expose, exposing_
    , moduleHeader, documentationComment, import_, declarations, declaration
    , type_, pattern, expression
    )

{-| Like [`Elm.Syntax.Parser`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Parser)
but able to parse badly indented code (TODO) similar to elm.format.

Some additional lenient parsing (TODO):

  - merge consecutive commas

  -     function parameters : Type = result

    →

        function : Type
        function parameters =
            result

  -     { field0 value, field1 value }

    →

        { field0 = value, field1 = value }

    or

        { field0 = value, field1 = value }

  -     { field0, field1 }

    →

        { field0 = field0, field1 = field1 }

  -     f | g | h

    →

        f |> g |> h

  -     3 |> String.toInt
        of case
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

  -     type A =
            | A
            | B

    →

        type A
            = A
            | B

@docs Parser, run, module_

That's all you'll need most of the time.

Sometimes it's useful to parse only some part of the syntax,
to, say, display only an expression in an article
or reparse only the touched declarations on save.

@docs moduleName, functionName, typeName, expose, exposing_
@docs moduleHeader, documentationComment, import_, declarations, declaration
@docs type_, pattern, expression

-}

import Char.Extra
import Elm.Parser.Layout
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
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation
import List.Extra
import ParserFast
import ParserWithComments
import Rope


type alias Parser a =
    ParserFast.Parser a


run : Parser a -> String -> Maybe a
run syntaxParser source =
    ParserFast.run syntaxParser source


module_ : Parser Elm.Syntax.File.File
module_ =
    ParserFast.map4
        (\moduleHeaderResults moduleComments importsResult declarationsResult ->
            { moduleDefinition = moduleHeaderResults.syntax
            , imports = importsResult.syntax
            , declarations = declarationsResult.syntax
            , comments =
                moduleHeaderResults.comments
                    |> Rope.prependTo moduleComments
                    |> Rope.prependTo importsResult.comments
                    |> Rope.prependTo declarationsResult.comments
                    |> Rope.toList
            }
        )
        (Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
            moduleHeader
        )
        (Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndentedFollowedByComments
            (ParserFast.map2OrSucceed
                (\moduleDocumentation commentsAfter ->
                    Rope.one moduleDocumentation |> Rope.filledPrependTo commentsAfter
                )
                documentationComment
                Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndented
                Rope.empty
            )
        )
        (ParserWithComments.many import_)
        declarations


moduleName : ParserFast.Parser (Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName)
moduleName =
    ParserFast.map2WithRange
        (\range head tail ->
            Elm.Syntax.Node.Node range (head :: tail)
        )
        typeName
        (ParserFast.loopWhileSucceedsRightToLeftStackUnsafe
            (ParserFast.symbolFollowedBy "." typeName)
            []
            (::)
        )


exposeDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing))
exposeDefinition =
    -- TODO inline
    ParserFast.map2WithRange
        (\range commentsAfterExposing exposingListInnerResult ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = Elm.Syntax.Node.Node range exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        exposing_


exposing_ : ParserFast.Parser (ParserWithComments.WithComments Elm.Syntax.Exposing.Exposing)
exposing_ =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2
            (\commentsBefore inner ->
                { comments = commentsBefore |> Rope.prependTo inner.comments
                , syntax = inner.syntax
                }
            )
            Elm.Parser.Layout.whitespaceAndComments
            (ParserFast.oneOf2
                (ParserFast.map3
                    (\headElement commentsAfterHeadElement tailElements ->
                        { comments =
                            headElement.comments
                                |> Rope.prependTo commentsAfterHeadElement
                                |> Rope.prependTo tailElements.comments
                        , syntax =
                            Elm.Syntax.Exposing.Explicit
                                (headElement.syntax
                                    :: tailElements.syntax
                                )
                        }
                    )
                    expose
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    (ParserWithComments.many
                        (ParserFast.symbolFollowedBy ","
                            (Elm.Parser.Layout.surroundedByWhitespaceAndCommentsEndsPositivelyIndented expose)
                        )
                    )
                )
                (ParserFast.mapWithRange
                    (\range commentsAfterDotDot ->
                        { comments = commentsAfterDotDot
                        , syntax = Elm.Syntax.Exposing.All range
                        }
                    )
                    (ParserFast.symbolFollowedBy ".." Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
                )
            )
        )
        |> ParserFast.followedBySymbol ")"


expose : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
expose =
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
infixExpose =
    ParserFast.map2WithRange
        (\range infixName () ->
            { comments = Rope.empty
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


typeExpose : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
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
                    { comments = commentsBeforeMaybeOpen |> Rope.prependTo open.comments
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start = typeNameRange.start
                            , end = open.syntax.end
                            }
                            (Elm.Syntax.Exposing.TypeExpose { name = typeExposeName, open = Just open.syntax })
                    }
        )
        typeNameNode
        Elm.Parser.Layout.whitespaceAndComments
        (ParserFast.map2WithRangeOrSucceed
            (\range left right ->
                Just { comments = left |> Rope.prependTo right, syntax = range }
            )
            (ParserFast.symbolFollowedBy "(" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
            (ParserFast.symbolFollowedBy ".." Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                |> ParserFast.followedBySymbol ")"
            )
            Nothing
        )


functionExpose : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
functionExpose =
    functionNameMapWithRange
        (\range name ->
            { comments = Rope.empty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Exposing.FunctionExpose name)
            }
        )


moduleHeader : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
moduleHeader =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : Parser (ParserWithComments.WithComments ( String, Elm.Syntax.Node.Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual fnTypeName ->
            { comments = commentsAfterFnName |> Rope.prependTo commentsAfterEqual
            , syntax = ( fnName, fnTypeName )
            }
        )
        functionName
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        typeNameNode


whereBlock : Parser (ParserWithComments.WithComments { command : Maybe (Elm.Syntax.Node.Node String), subscription : Maybe (Elm.Syntax.Node.Node String) })
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
                        |> Rope.prependTo head.comments
                        |> Rope.prependTo commentsAfterHead
                        |> Rope.prependTo tail.comments
                , syntax =
                    { command =
                        pairs
                            |> List.Extra.find
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
                            |> List.Extra.find
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
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            effectWhereClause
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy "," (Elm.Parser.Layout.surroundedByWhitespaceAndCommentsEndsPositivelyIndented effectWhereClause))
            )
        )
        |> ParserFast.followedBySymbol "}"


effectWhereClauses : Parser (ParserWithComments.WithComments { command : Maybe (Elm.Syntax.Node.Node String), subscription : Maybe (Elm.Syntax.Node.Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            { comments = commentsBefore |> Rope.prependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        whereBlock


effectModuleDefinition : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
effectModuleDefinition =
    ParserFast.map7WithRange
        (\range commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            { comments =
                commentsAfterEffect
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo whereClauses.comments
                    |> Rope.prependTo commentsAfterWhereClauses
                    |> Rope.prependTo exp.comments
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
        (ParserFast.keywordFollowedBy "effect" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.keywordFollowedBy "module" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        effectWhereClauses
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        exposeDefinition


normalModuleDefinition : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
normalModuleDefinition =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleNameNode commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.NormalModule
                        { moduleName = moduleNameNode
                        , exposingList = exposingList.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "module" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        exposeDefinition


portModuleDefinition : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleNameNode commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.PortModule { moduleName = moduleNameNode, exposingList = exposingList.syntax })
            }
        )
        (ParserFast.keywordFollowedBy "port" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.keywordFollowedBy "module" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        exposeDefinition


import_ : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Import.Import))
import_ =
    ParserFast.map5WithStartLocation
        (\start commentsAfterImport mod commentsAfterModuleName maybeModuleAlias maybeExposingList ->
            let
                commentsBeforeAlias : ParserWithComments.Comments
                commentsBeforeAlias =
                    commentsAfterImport
                        |> Rope.prependTo commentsAfterModuleName
            in
            case maybeModuleAlias of
                Nothing ->
                    case maybeExposingList of
                        Nothing ->
                            let
                                (Elm.Syntax.Node.Node modRange _) =
                                    mod
                            in
                            { comments = commentsBeforeAlias
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
                                    exposingListValue.syntax
                            in
                            { comments =
                                commentsBeforeAlias |> Rope.prependTo exposingListValue.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Just exposingListValue.syntax
                                    }
                            }

                Just moduleAliasResult ->
                    case maybeExposingList of
                        Nothing ->
                            let
                                (Elm.Syntax.Node.Node aliasRange _) =
                                    moduleAliasResult.syntax
                            in
                            { comments =
                                commentsBeforeAlias |> Rope.prependTo moduleAliasResult.comments
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
                                    exposingListValue.syntax
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> Rope.prependTo moduleAliasResult.comments
                                    |> Rope.prependTo exposingListValue.comments
                            , syntax =
                                Elm.Syntax.Node.Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Just exposingListValue.syntax
                                    }
                            }
        )
        (ParserFast.keywordFollowedBy "import" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        moduleName
        Elm.Parser.Layout.whitespaceAndComments
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    { comments = commentsBefore |> Rope.prependTo commentsAfter
                    , syntax = moduleAliasNode
                    }
            )
            (ParserFast.keywordFollowedBy "as" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
            (typeNameMapWithRange
                (\range moduleAlias ->
                    Elm.Syntax.Node.Node range [ moduleAlias ]
                )
            )
            Elm.Parser.Layout.whitespaceAndComments
            Nothing
        )
        (ParserFast.map2OrSucceed
            (\exposingResult commentsAfter ->
                Just
                    { comments = exposingResult.comments |> Rope.prependTo commentsAfter
                    , syntax = exposingResult.syntax
                    }
            )
            exposeDefinition
            Elm.Parser.Layout.whitespaceAndComments
            Nothing
        )


declarations : Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)))
declarations =
    ParserWithComments.many
        (Elm.Parser.Layout.moduleLevelIndentedFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    { comments = declarationParsed.comments |> Rope.prependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
                )
                declaration
                Elm.Parser.Layout.whitespaceAndComments
            )
        )


declaration : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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


declarationWithDocumentation : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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
                        Rope.one documentation
                            |> Rope.filledPrependTo afterDocumentation.comments
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
        (Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
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


functionAfterDocumentation : Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
functionAfterDocumentation =
    ParserFast.map6
        (\startName commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            { comments =
                (case maybeSignature of
                    Nothing ->
                        commentsAfterStartName

                    Just signature ->
                        commentsAfterStartName |> Rope.prependTo signature.comments
                )
                    |> Rope.prependTo arguments.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo result.comments
            , syntax =
                FunctionDeclarationAfterDocumentation
                    { startName = startName
                    , signature = maybeSignature |> Maybe.map .syntax
                    , arguments = arguments.syntax
                    , expression = result.syntax
                    }
            }
        )
        -- infix declarations itself don't have documentation
        functionNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , syntax =
                        { implementationName = implementationName.syntax
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                    }
            )
            (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
            type_
            (Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndentedFollowedBy
                functionNameNode
            )
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            Nothing
        )
        parameterPatternsEqual
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        expressionFollowedByOptimisticLayout


functionDeclarationWithoutDocumentation : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
functionDeclarationWithoutDocumentation =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            let
                (Elm.Syntax.Node.Node expressionRange _) =
                    result.syntax
            in
            case maybeSignature of
                Nothing ->
                    { comments =
                        commentsAfterStartName
                            |> Rope.prependTo arguments.comments
                            |> Rope.prependTo commentsAfterEqual
                            |> Rope.prependTo result.comments
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
                        (commentsAfterStartName |> Rope.prependTo signature.comments)
                            |> Rope.prependTo arguments.comments
                            |> Rope.prependTo commentsAfterEqual
                            |> Rope.prependTo result.comments
                    , syntax =
                        Elm.Syntax.Node.Node { start = startNameStart, end = expressionRange.end }
                            (Elm.Syntax.Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Just (Elm.Syntax.Node.combine Elm.Syntax.Signature.Signature startNameNode signature.typeAnnotation)
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
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            )
            (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
            type_
            (Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndentedFollowedBy
                functionNameNode
            )
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            Nothing
        )
        parameterPatternsEqual
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
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

                    _ ->
                        True
            )


parameterPatternsEqual : Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)))
parameterPatternsEqual =
    ParserWithComments.until (ParserFast.symbol "=" ())
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            patternNotSpaceSeparated
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        )


infixDeclaration : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            { comments =
                commentsAfterInfix
                    |> Rope.prependTo commentsAfterDirection
                    |> Rope.prependTo commentsAfterPrecedence
                    |> Rope.prependTo commentsAfterOperator
                    |> Rope.prependTo commentsAfterEqual
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Declaration.InfixDeclaration
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "infix" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        infixDirection
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.integerDecimalMapWithRange Elm.Syntax.Node.Node)
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
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
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        functionNameNode


infixDirection : Parser (Elm.Syntax.Node.Node Elm.Syntax.Infix.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "right" Elm.Syntax.Infix.Right))
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "left" Elm.Syntax.Infix.Left))
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "non" Elm.Syntax.Infix.Non))


portDeclarationAfterDocumentation : Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    ParserFast.map5
        (\commentsAfterPort ((Elm.Syntax.Node.Node nameRange _) as name) commentsAfterName commentsAfterColon typeAnnotationResult ->
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo typeAnnotationResult.comments
                    |> Rope.prependTo commentsAfterColon
            , syntax =
                PortDeclarationAfterDocumentation
                    { startLocation = { row = nameRange.start.row, column = 1 }
                    , name = name
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "port" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        functionNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        type_


portDeclarationWithoutDocumentation : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
portDeclarationWithoutDocumentation =
    ParserFast.map5
        (\commentsAfterPort ((Elm.Syntax.Node.Node nameRange _) as name) commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (Elm.Syntax.Node.Node { end } _) =
                    typeAnnotationResult.syntax
            in
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = { row = nameRange.start.row, column = 1 }
                    , end = end
                    }
                    (Elm.Syntax.Declaration.PortDeclaration
                        { name = name
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "port" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        functionNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        type_


typeOrTypeAliasDefinitionAfterDocumentation : Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    ParserFast.map2
        (\commentsAfterType declarationAfterDocumentation ->
            { comments = commentsAfterType |> Rope.prependTo declarationAfterDocumentation.comments
            , syntax = declarationAfterDocumentation.syntax
            }
        )
        (ParserFast.keywordFollowedBy "type" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            typeAliasDefinitionAfterDocumentationAfterTypePrefix
            customTypeDefinitionAfterDocumentationAfterTypePrefix
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEquals typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        typeNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        type_


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\name commentsAfterName parameters commentsAfterEqual headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariant.comments
                    |> Rope.prependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        typeNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        valueConstructorOptimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.symbolFollowedBy "|"
                (Elm.Parser.Layout.positivelyIndentedPlusFollowedBy 1
                    (ParserFast.map2
                        (\commentsBeforePipe variantResult ->
                            { comments =
                                commentsBeforePipe
                                    |> Rope.prependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
                        )
                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    ParserFast.map2WithStartLocation
        (\start commentsAfterType afterStart ->
            let
                allComments : ParserWithComments.Comments
                allComments =
                    commentsAfterType |> Rope.prependTo afterStart.comments
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
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        )
        (ParserFast.oneOf2
            typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            customTypeDefinitionWithoutDocumentationAfterTypePrefix
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (ParserWithComments.WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEqual typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        typeNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        type_


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (ParserWithComments.WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\name commentsAfterName parameters commentsAfterEqual headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariant.comments
                    |> Rope.prependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        typeNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        typeGenericListEquals
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        valueConstructorOptimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.symbolFollowedBy "|"
                (Elm.Parser.Layout.positivelyIndentedPlusFollowedBy 1
                    (ParserFast.map2
                        (\commentsBeforePipe variantResult ->
                            { comments =
                                commentsBeforePipe
                                    |> Rope.prependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
                        )
                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


valueConstructorOptimisticLayout : Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor))
valueConstructorOptimisticLayout =
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
                    |> Rope.prependTo argumentsReverse.comments
            , syntax =
                Elm.Syntax.Node.Node fullRange
                    { name = nameNode
                    , arguments = List.reverse argumentsReverse.syntax
                    }
            }
        )
        typeNameNode
        Elm.Parser.Layout.whitespaceAndComments
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments = typeAnnotationResult.comments |> Rope.prependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    Elm.Parser.Layout.whitespaceAndComments
                )
            )
        )


typeGenericListEquals : Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node String)))
typeGenericListEquals =
    ParserWithComments.until (ParserFast.symbol "=" ())
        (ParserFast.map2
            (\name commentsAfterName ->
                { comments = commentsAfterName
                , syntax = name
                }
            )
            functionNameNode
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        )


type_ : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
type_ =
    ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
        (ParserFast.map2
            (\startType commentsAfter ->
                { comments =
                    startType.comments
                        |> Rope.prependTo commentsAfter
                , syntax = startType.syntax
                }
            )
            (ParserFast.lazy (\() -> typeAnnotationNotFunction))
            Elm.Parser.Layout.whitespaceAndComments
        )
        (ParserFast.symbolFollowedBy "->"
            (Elm.Parser.Layout.positivelyIndentedPlusFollowedBy 2
                (ParserFast.map3
                    (\commentsAfterArrow typeAnnotationResult commentsAfterType ->
                        { comments =
                            commentsAfterArrow
                                |> Rope.prependTo typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfterType
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.lazy (\() -> typeAnnotationNotFunction))
                    Elm.Parser.Layout.whitespaceAndComments
                )
            )
        )
        (\inType outType ->
            { comments =
                inType.comments
                    |> Rope.prependTo outType.comments
            , syntax =
                Elm.Syntax.Node.combine Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inType.syntax outType.syntax
            }
        )


typeNotSpaceSeparated : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeNotSpaceSeparated =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithoutArguments
        genericTypeAnnotation
        recordTypeAnnotation


typeAnnotationNotFunction : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeAnnotationNotFunction =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithArgumentsOptimisticLayout
        genericTypeAnnotation
        recordTypeAnnotation


parensTypeAnnotation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
parensTypeAnnotation =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = Rope.empty
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
                            |> Rope.prependTo firstPart.comments
                            |> Rope.prependTo commentsAfterFirstPart
                            |> Rope.prependTo lastToSecondPart.comments
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
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                type_
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                (ParserFast.oneOf2
                    (ParserFast.symbol ")"
                        { comments = Rope.empty, syntax = Nothing }
                    )
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore secondPartResult commentsAfter maybeThirdPartResult ->
                                { comments =
                                    commentsBefore
                                        |> Rope.prependTo secondPartResult.comments
                                        |> Rope.prependTo commentsAfter
                                , syntax = Just { maybeThirdPart = maybeThirdPartResult.syntax, secondPart = secondPartResult.syntax }
                                }
                            )
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                            type_
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")"
                                    { comments = Rope.empty, syntax = Nothing }
                                )
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore thirdPartResult commentsAfter ->
                                            { comments =
                                                commentsBefore
                                                    |> Rope.prependTo thirdPartResult.comments
                                                    |> Rope.prependTo commentsAfter
                                            , syntax = Just thirdPartResult.syntax
                                            }
                                        )
                                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                        type_
                                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


genericTypeAnnotation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
genericTypeAnnotation =
    functionNameMapWithRange
        (\range var ->
            { comments = Rope.empty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.TypeAnnotation.GenericType var)
            }
        )


recordTypeAnnotation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
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
                            |> Rope.prependTo afterCurlyResult.comments
                    , syntax =
                        Elm.Syntax.Node.Node range afterCurlyResult.syntax
                    }
        )
        (ParserFast.symbolFollowedBy "{" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\firstNameNode commentsAfterFirstName afterFirstName ->
                    Just
                        { comments =
                            commentsAfterFirstName
                                |> Rope.prependTo afterFirstName.comments
                        , syntax =
                            case afterFirstName.syntax of
                                RecordExtensionExpressionAfterName fields ->
                                    Elm.Syntax.TypeAnnotation.GenericRecord firstNameNode fields

                                FieldsAfterName fieldsAfterName ->
                                    Elm.Syntax.TypeAnnotation.Record (Elm.Syntax.Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                        }
                )
                functionNameNode
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithRange
                            (\range commentsBefore head tail ->
                                { comments =
                                    commentsBefore
                                        |> Rope.prependTo head.comments
                                        |> Rope.prependTo tail.comments
                                , syntax =
                                    RecordExtensionExpressionAfterName
                                        (Elm.Syntax.Node.Node range (head.syntax :: tail.syntax))
                                }
                            )
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                            recordFieldDefinition
                            (ParserWithComments.many
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore field ->
                                            { comments = commentsBefore |> Rope.prependTo field.comments
                                            , syntax = field.syntax
                                            }
                                        )
                                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                        recordFieldDefinition
                                    )
                                )
                            )
                        )
                    )
                    (ParserFast.symbolFollowedBy ":"
                        (ParserFast.map4
                            (\commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
                                { comments =
                                    commentsBeforeFirstFieldValue
                                        |> Rope.prependTo firstFieldValue.comments
                                        |> Rope.prependTo commentsAfterFirstFieldValue
                                        |> Rope.prependTo tailFields.comments
                                , syntax =
                                    FieldsAfterName
                                        { firstFieldValue = firstFieldValue.syntax
                                        , tailFields = tailFields.syntax
                                        }
                                }
                            )
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                            type_
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                                { comments = Rope.empty, syntax = [] }
                            )
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" Nothing)
        )


typeAnnotationRecordEmpty : Elm.Syntax.TypeAnnotation.TypeAnnotation
typeAnnotationRecordEmpty =
    Elm.Syntax.TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.RecordDefinition)
    | FieldsAfterName { firstFieldValue : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation, tailFields : List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.RecordField) }


recordFieldsTypeAnnotation : ParserFast.Parser (ParserWithComments.WithComments Elm.Syntax.TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserFast.map3
        (\commentsBefore head tail ->
            { comments =
                commentsBefore
                    |> Rope.prependTo head.comments
                    |> Rope.prependTo tail.comments
            , syntax = head.syntax :: tail.syntax
            }
        )
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        recordFieldDefinition
        (ParserWithComments.many
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map2
                    (\commentsBefore field ->
                        { comments = commentsBefore |> Rope.prependTo field.comments
                        , syntax = field.syntax
                        }
                    )
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    recordFieldDefinition
                )
            )
        )


recordFieldDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.RecordField))
recordFieldDefinition =
    ParserFast.map6WithRange
        (\range commentsBeforeFunctionName name commentsAfterFunctionName commentsAfterColon value commentsAfterValue ->
            { comments =
                commentsBeforeFunctionName
                    |> Rope.prependTo commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo value.comments
                    |> Rope.prependTo commentsAfterValue
            , syntax = Elm.Syntax.Node.Node range ( name, value.syntax )
            }
        )
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        functionNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        type_
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented


typedTypeAnnotationWithoutArguments : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
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
            { comments = Rope.empty
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node range name) [])
            }
        )
        typeName
        maybeDotTypeNamesTuple


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\firstName afterFirstName ->
            case afterFirstName of
                Nothing ->
                    Just ( [], firstName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( firstName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


typedTypeAnnotationWithArgumentsOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
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
                    |> Rope.prependTo argsReverse.comments
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
            typeName
            maybeDotTypeNamesTuple
        )
        Elm.Parser.Layout.whitespaceAndComments
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments =
                            typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    Elm.Parser.Layout.whitespaceAndComments
                )
            )
        )


subExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
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


referenceOrNumberExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
referenceOrNumberExpression =
    ParserFast.oneOf3
        qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
        numberExpression


followedByMultiRecordAccess : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)) -> ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
followedByMultiRecordAccess beforeRecordAccesses =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (ParserFast.symbolFollowedBy "." functionNameNode)
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


expression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
expression =
    expressionFollowedByOptimisticLayout


expressionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
expressionFollowedByOptimisticLayout =
    extendedSubExpressionOptimisticLayout
        { afterCommitting = .extensionRightParser
        , validateRightPrecedence = Just
        }


glslExpressionAfterOpeningSquareBracket : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                { comments = Rope.empty
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


listOrGlslExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
listOrGlslExpression =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.map2WithRange
            (\range commentsBefore elements ->
                { comments = commentsBefore |> Rope.prependTo elements.comments
                , syntax =
                    Elm.Syntax.Node.Node
                        { start = { row = range.start.row, column = range.start.column - 1 }
                        , end = range.end
                        }
                        elements.syntax
                }
            )
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = Rope.empty, syntax = Elm.Syntax.Expression.ListExpr [] })
                (ParserFast.map2
                    (\head tail ->
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                        , syntax = Elm.Syntax.Expression.ListExpr (head.syntax :: tail.syntax)
                        }
                    )
                    expressionFollowedByOptimisticLayout
                    (Elm.Parser.Layout.positivelyIndentedFollowedBy
                        (ParserWithComments.many
                            (ParserFast.symbolFollowedBy ","
                                (ParserFast.map2
                                    (\commentsBefore expressionResult ->
                                        { comments = commentsBefore |> Rope.prependTo expressionResult.comments
                                        , syntax = expressionResult.syntax
                                        }
                                    )
                                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                    expressionFollowedByOptimisticLayout
                                    |> Elm.Parser.Layout.endsPositivelyIndented
                                )
                            )
                        )
                    )
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )



-- recordExpression


recordExpressionFollowedByRecordAccess : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordExpressionFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2WithRange
            (\range commentsBefore afterCurly ->
                { comments =
                    commentsBefore
                        |> Rope.prependTo afterCurly.comments
                , syntax = Elm.Syntax.Node.Node (rangeMoveStartLeftByOneColumn range) afterCurly.syntax
                }
            )
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            recordContentsCurlyEnd
            |> followedByMultiRecordAccess
        )


recordContentsCurlyEnd : ParserFast.Parser (ParserWithComments.WithComments Elm.Syntax.Expression.Expression)
recordContentsCurlyEnd =
    ParserFast.oneOf2
        (ParserFast.map5
            (\nameNode commentsAfterFunctionName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
                { comments =
                    commentsAfterFunctionName
                        |> Rope.prependTo afterNameBeforeFields.comments
                        |> Rope.prependTo tailFields.comments
                        |> Rope.prependTo commentsBeforeClosingCurly
                , syntax =
                    case afterNameBeforeFields.syntax of
                        RecordUpdateFirstSetter firstField ->
                            Elm.Syntax.Expression.RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                        FieldsFirstValue firstFieldValue ->
                            Elm.Syntax.Expression.RecordExpr (Elm.Syntax.Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                }
            )
            functionNameNode
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore setterResult ->
                            { comments = commentsBefore |> Rope.prependTo setterResult.comments
                            , syntax = RecordUpdateFirstSetter setterResult.syntax
                            }
                        )
                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        recordSetterNodeWithLayout
                    )
                )
                (ParserFast.symbolFollowedBy "="
                    (ParserFast.map2
                        (\commentsBefore expressionResult ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo expressionResult.comments
                            , syntax = FieldsFirstValue expressionResult.syntax
                            }
                        )
                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        expressionFollowedByOptimisticLayout
                        |> Elm.Parser.Layout.endsPositivelyIndented
                    )
                )
            )
            recordFields
            (Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented |> ParserFast.followedBySymbol "}")
        )
        (ParserFast.symbol "}" { comments = Rope.empty, syntax = Elm.Syntax.Expression.RecordExpr [] })


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Elm.Syntax.Node.Node Elm.Syntax.Expression.RecordSetter)
    | FieldsFirstValue (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)


recordFields : ParserFast.Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Expression.RecordSetter)))
recordFields =
    ParserWithComments.many
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map2
                (\commentsBefore setterResult ->
                    { comments = commentsBefore |> Rope.prependTo setterResult.comments
                    , syntax = setterResult.syntax
                    }
                )
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                recordSetterNodeWithLayout
            )
        )


recordSetterNodeWithLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.RecordSetter))
recordSetterNodeWithLayout =
    ParserFast.map4WithRange
        (\range name commentsAfterFunctionName commentsAfterEquals expressionResult ->
            { comments =
                commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax = Elm.Syntax.Node.Node range ( name, expressionResult.syntax )
            }
        )
        functionNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        |> Elm.Parser.Layout.endsPositivelyIndented


literalExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
literalExpression =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string)
            }
        )


charLiteralExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
charLiteralExpression =
    characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.CharLiteral char)
            }
        )



-- lambda


lambdaExpressionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
lambdaExpressionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs commentsAfterArrow expressionResult ->
            let
                (Elm.Syntax.Node.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                commentsAfterBackslash
                    |> Rope.prependTo firstArg.comments
                    |> Rope.prependTo commentsAfterFirstArg
                    |> Rope.prependTo secondUpArgs.comments
                    |> Rope.prependTo commentsAfterArrow
                    |> Rope.prependTo expressionResult.comments
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
        (ParserFast.symbolFollowedBy "\\" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        patternNotSpaceSeparated
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserWithComments.until
            (ParserFast.symbol "->" ())
            (ParserFast.map2
                (\patternResult commentsAfter ->
                    { comments =
                        patternResult.comments
                            |> Rope.prependTo commentsAfter
                    , syntax = patternResult.syntax
                    }
                )
                patternNotSpaceSeparated
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            )
        )
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        expressionFollowedByOptimisticLayout



-- Case Expression


caseExpressionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
caseExpressionFollowedByOptimisticLayout =
    ParserFast.map4WithStartLocation
        (\start commentsAfterCase casedExpressionResult commentsAfterOf casesResult ->
            let
                ( firstCase, lastToSecondCase ) =
                    casesResult.syntax
            in
            { comments =
                commentsAfterCase
                    |> Rope.prependTo casedExpressionResult.comments
                    |> Rope.prependTo commentsAfterOf
                    |> Rope.prependTo casesResult.comments
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
        (ParserFast.keywordFollowedBy "case" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "of" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        )
        (ParserFast.withIndentSetToColumn caseStatementsFollowedByOptimisticLayout)


caseStatementsFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments ( Elm.Syntax.Expression.Case, List Elm.Syntax.Expression.Case ))
caseStatementsFollowedByOptimisticLayout =
    ParserFast.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> Rope.prependTo commentsAfterFirstCasePattern
                    |> Rope.prependTo commentsAfterFirstCaseArrowRight
                    |> Rope.prependTo firstCaseExpressionResult.comments
                    |> Rope.prependTo lastToSecondCase.comments
            , syntax =
                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                , lastToSecondCase.syntax
                )
            }
        )
        pattern
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "->" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        (ParserWithComments.manyWithoutReverse caseStatementFollowedByOptimisticLayout)


caseStatementFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments Elm.Syntax.Expression.Case)
caseStatementFollowedByOptimisticLayout =
    Elm.Parser.Layout.topIndentedFollowedBy
        (ParserFast.map4
            (\patternResult commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    patternResult.comments
                        |> Rope.prependTo commentsBeforeArrowRight
                        |> Rope.prependTo commentsAfterArrowRight
                        |> Rope.prependTo expr.comments
                , syntax = ( patternResult.syntax, expr.syntax )
                }
            )
            pattern
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            (ParserFast.symbolFollowedBy "->" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
            expressionFollowedByOptimisticLayout
        )



-- Let Expression


letExpressionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
letExpressionFollowedByOptimisticLayout =
    ParserFast.map3WithStartLocation
        (\start letDeclarationsResult commentsAfterIn expressionResult ->
            let
                (Elm.Syntax.Node.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                letDeclarationsResult.comments
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo expressionResult.comments
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
                                |> Rope.prependTo letDeclarationsResult.comments
                        , declarations = letDeclarationsResult.syntax
                        }
                    )
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
        )
        -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        (Elm.Parser.Layout.positivelyIndentedPlusFollowedBy 2
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        )
        expressionFollowedByOptimisticLayout


letDeclarationsIn : ParserFast.Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration)))
letDeclarationsIn =
    Elm.Parser.Layout.topIndentedFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                { comments =
                    headLetResult.comments
                        |> Rope.prependTo commentsAfter
                        |> Rope.prependTo tailLetResult.comments
                , syntax = headLetResult.syntax :: tailLetResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunctionFollowedByOptimisticLayout
                letDestructuringDeclarationFollowedByOptimisticLayout
            )
            Elm.Parser.Layout.whitespaceAndComments
            (ParserWithComments.until
                (ParserFast.keyword "in" ())
                letBlockElementFollowedByOptimisticLayout
            )
        )


letBlockElementFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letBlockElementFollowedByOptimisticLayout =
    Elm.Parser.Layout.topIndentedFollowedBy
        (ParserFast.oneOf2
            letFunctionFollowedByOptimisticLayout
            letDestructuringDeclarationFollowedByOptimisticLayout
        )


letDestructuringDeclarationFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
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
                    |> Rope.prependTo commentsAfterPattern
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node { start = patternRange.start, end = destructuredExpressionRange.end }
                    (Elm.Syntax.Expression.LetDestructuring patternResult.syntax expressionResult.syntax)
            }
        )
        patternNotSpaceSeparated
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout


letFunctionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letFunctionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
            case maybeSignature of
                Nothing ->
                    let
                        (Elm.Syntax.Node.Node expressionRange _) =
                            expressionResult.syntax
                    in
                    { comments =
                        commentsAfterStartName
                            |> Rope.prependTo arguments.comments
                            |> Rope.prependTo commentsAfterEqual
                            |> Rope.prependTo expressionResult.comments
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
                        (commentsAfterStartName |> Rope.prependTo signature.comments)
                            |> Rope.prependTo arguments.comments
                            |> Rope.prependTo commentsAfterEqual
                            |> Rope.prependTo expressionResult.comments
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
        functionNameNode
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            )
            (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
            type_
            (Elm.Parser.Layout.whitespaceAndCommentsEndsTopIndentedFollowedBy
                functionNameNode
            )
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            Nothing
        )
        parameterPatternsEqual
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
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


numberExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
numberExpression =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Floatable n)
            }
        )
        (\range n ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Integer n)
            }
        )
        (\range n ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Hex n)
            }
        )


ifBlockExpressionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
ifBlockExpressionFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterIf condition commentsAfterThen ifTrue commentsAfterElse ifFalse ->
            let
                (Elm.Syntax.Node.Node ifFalseRange _) =
                    ifFalse.syntax
            in
            { comments =
                commentsAfterIf
                    |> Rope.prependTo condition.comments
                    |> Rope.prependTo commentsAfterThen
                    |> Rope.prependTo ifTrue.comments
                    |> Rope.prependTo commentsAfterElse
                    |> Rope.prependTo ifFalse.comments
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
        (ParserFast.keywordFollowedBy "if" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "then" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        )
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "else" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        )
        expressionFollowedByOptimisticLayout


negationOperation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
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


negationAfterMinus : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
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


qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
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
        typeName
        maybeDotReferenceExpressionTuple
        |> followedByMultiRecordAccess


maybeDotReferenceExpressionTuple : ParserFast.Parser (Maybe ( List String, String ))
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
                    typeName
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\name -> Just ( [], name ))
                functionName
            )
        )
        Nothing


unqualifiedFunctionReferenceExpressionFollowedByRecordAccess : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
unqualifiedFunctionReferenceExpressionFollowedByRecordAccess =
    functionNameMapWithRange
        (\range unqualified ->
            { comments = Rope.empty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Expression.FunctionOrValue [] unqualified)
            }
        )
        |> followedByMultiRecordAccess


recordAccessFunctionExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordAccessFunctionExpression =
    ParserFast.symbolFollowedBy "."
        (functionNameMapWithRange
            (\range field ->
                { comments = Rope.empty
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


tupledExpressionIfNecessaryFollowedByRecordAccess : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
tupledExpressionIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf3
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = Rope.empty
                    , syntax =
                        Elm.Syntax.Node.Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            Elm.Syntax.Expression.UnitExpr
                    }
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            tupledExpressionInnerAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            { comments = Rope.empty
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


tupledExpressionInnerAfterOpeningParens : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.map3WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> Rope.prependTo firstPart.comments
                    |> Rope.prependTo tailParts.comments
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
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.oneOf2
                (ParserFast.symbol ")"
                    { comments = Rope.empty, syntax = TupledParenthesized () () }
                )
                (ParserFast.symbolFollowedBy ","
                    (ParserFast.map3
                        (\commentsBefore partResult maybeThirdPart ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo partResult.comments
                                    |> Rope.prependTo maybeThirdPart.comments
                            , syntax = TupledTwoOrThree partResult.syntax maybeThirdPart.syntax
                            }
                        )
                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        expressionFollowedByOptimisticLayout
                        (Elm.Parser.Layout.positivelyIndentedFollowedBy
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore partResult ->
                                            { comments =
                                                commentsBefore
                                                    |> Rope.prependTo partResult.comments
                                            , syntax = Just partResult.syntax
                                            }
                                        )
                                        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                        expressionFollowedByOptimisticLayout
                                        |> Elm.Parser.Layout.endsPositivelyIndented
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
        | afterCommitting : InfixOperatorInfo -> ParserFast.Parser (ParserWithComments.WithComments ExtensionRight)
        , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
extendedSubExpressionOptimisticLayout info =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (infixOperatorAndThen info)
        )
        subExpressionMaybeAppliedFollowedByOptimisticLayout
        (\extensionRightResult leftNodeWithComments ->
            { comments =
                leftNodeWithComments.comments
                    |> Rope.prependTo extensionRightResult.comments
            , syntax =
                leftNodeWithComments.syntax
                    |> applyExtensionRight extensionRightResult.syntax
            }
        )
        Basics.identity


extensionRightParser :
    { afterCommitting : InfixOperatorInfo -> ParserFast.Parser (ParserWithComments.WithComments ExtensionRight)
    , direction : Elm.Syntax.Infix.InfixDirection
    , symbol : String
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> ParserFast.Parser (ParserWithComments.WithComments ExtensionRight)
extensionRightParser extensionRightInfo =
    ParserFast.map2
        (\commentsBefore right ->
            { comments = commentsBefore |> Rope.prependTo right.comments
            , syntax =
                ExtendRightByOperation
                    { symbol = extensionRightInfo.symbol
                    , direction = extensionRightInfo.direction
                    , expression = right.syntax
                    }
            }
        )
        Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
        (ParserFast.lazy
            (\() -> extendedSubExpressionOptimisticLayout extensionRightInfo)
        )


infixOperatorAndThen :
    { info_
        | afterCommitting : InfixOperatorInfo -> ParserFast.Parser (ParserWithComments.WithComments ExtensionRight)
        , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> ParserFast.Parser (ParserWithComments.WithComments ExtensionRight)
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

                "++" ->
                    appendResult

                "<|" ->
                    apLResult

                ">>" ->
                    composeRResult

                "==" ->
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

                _ ->
                    Nothing
        )
        extensionRightConstraints.afterCommitting


subExpressionMaybeAppliedFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
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


negationOperationOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
negationOperationOptimisticLayout =
    negationOperation |> followedByOptimisticLayout


charLiteralExpressionOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
charLiteralExpressionOptimisticLayout =
    charLiteralExpression |> followedByOptimisticLayout


literalExpressionOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
literalExpressionOptimisticLayout =
    literalExpression |> followedByOptimisticLayout


listOrGlslExpressionOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
listOrGlslExpressionOptimisticLayout =
    listOrGlslExpression |> followedByOptimisticLayout


followedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments a) -> ParserFast.Parser (ParserWithComments.WithComments a)
followedByOptimisticLayout parser =
    ParserFast.map2
        (\result commentsAfter ->
            { comments = result.comments |> Rope.prependTo commentsAfter
            , syntax = result.syntax
            }
        )
        parser
        Elm.Parser.Layout.whitespaceAndComments


recordAccessFunctionExpressionMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordAccessFunctionExpressionMaybeApplied =
    recordAccessFunctionExpression |> followedByMultiArgumentApplication


recordExpressionFollowedByRecordAccessMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
recordExpressionFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if record access
    recordExpressionFollowedByRecordAccess
        |> followedByMultiArgumentApplication


tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if not parenthesized
    tupledExpressionIfNecessaryFollowedByRecordAccess
        |> followedByMultiArgumentApplication


caseOrUnqualifiedReferenceExpressionMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
caseOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        caseExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


letOrUnqualifiedReferenceExpressionMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
letOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        letExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


ifOrUnqualifiedReferenceExpressionMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
ifOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        ifBlockExpressionFollowedByOptimisticLayout
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


referenceOrNumberExpressionMaybeApplied : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
referenceOrNumberExpressionMaybeApplied =
    ParserFast.oneOf3
        (qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (numberExpression |> followedByOptimisticLayout)


followedByMultiArgumentApplication : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)) -> ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
followedByMultiArgumentApplication appliedExpressionParser =
    ParserFast.map3
        (\leftExpressionResult commentsBeforeExtension maybeArgsReverse ->
            { comments =
                leftExpressionResult.comments
                    |> Rope.prependTo commentsBeforeExtension
                    |> Rope.prependTo maybeArgsReverse.comments
            , syntax =
                case maybeArgsReverse.syntax of
                    [] ->
                        leftExpressionResult.syntax

                    ((Elm.Syntax.Node.Node lastArgRange _) :: _) as argsReverse ->
                        let
                            (Elm.Syntax.Node.Node leftRange _) =
                                leftExpressionResult.syntax
                        in
                        Elm.Syntax.Node.Node { start = leftRange.start, end = lastArgRange.end }
                            (Elm.Syntax.Expression.Application
                                (leftExpressionResult.syntax :: List.reverse argsReverse)
                            )
            }
        )
        appliedExpressionParser
        Elm.Parser.Layout.whitespaceAndComments
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfter ->
                        { comments = arg.comments |> Rope.prependTo commentsAfter
                        , syntax = arg.syntax
                        }
                    )
                    subExpression
                    Elm.Parser.Layout.whitespaceAndComments
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
    , extensionRightParser : ParserFast.Parser (ParserWithComments.WithComments ExtensionRight)
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


pattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
pattern =
    ParserFast.map2
        (\leftMaybeConsed maybeAsExtension ->
            case maybeAsExtension of
                Nothing ->
                    leftMaybeConsed

                Just asExtension ->
                    { comments =
                        leftMaybeConsed.comments
                            |> Rope.prependTo asExtension.comments
                    , syntax =
                        Elm.Syntax.Node.combine Elm.Syntax.Pattern.AsPattern leftMaybeConsed.syntax asExtension.syntax
                    }
        )
        (ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
            (ParserFast.map2
                (\startPatternResult commentsAfter ->
                    { comments = startPatternResult.comments |> Rope.prependTo commentsAfter
                    , syntax = startPatternResult.syntax
                    }
                )
                (ParserFast.lazy (\() -> composablePattern))
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            )
            (ParserFast.symbolFollowedBy "::"
                (ParserFast.map3
                    (\commentsAfterCons patternResult commentsAfterTailSubPattern ->
                        { comments =
                            commentsAfterCons
                                |> Rope.prependTo patternResult.comments
                                |> Rope.prependTo commentsAfterTailSubPattern
                        , syntax = patternResult.syntax
                        }
                    )
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.lazy (\() -> composablePattern))
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                )
            )
            (\consed afterCons ->
                { comments = consed.comments |> Rope.prependTo afterCons.comments
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
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    functionNameNode
                )
            )
            Nothing
        )


parensPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
parensPattern =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                { comments =
                    commentsBeforeHead
                        |> Rope.prependTo contentResult.comments
                , syntax =
                    Elm.Syntax.Node.Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                        contentResult.syntax
                }
            )
            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" { comments = Rope.empty, syntax = Elm.Syntax.Pattern.UnitPattern })
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        { comments =
                            headResult.comments
                                |> Rope.prependTo commentsAfterHead
                                |> Rope.prependTo tailResult.comments
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
                    Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo secondPart.comments
                                            |> Rope.prependTo commentsAfter
                                            |> Rope.prependTo maybeThirdPart.comments
                                    , syntax = Just { maybeThirdPart = maybeThirdPart.syntax, secondPart = secondPart.syntax }
                                    }
                                )
                                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                pattern
                                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                { comments =
                                                    commentsBefore
                                                        |> Rope.prependTo thirdPart.comments
                                                        |> Rope.prependTo commentsAfter
                                                , syntax = Just thirdPart.syntax
                                                }
                                            )
                                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                                            pattern
                                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
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


varPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
varPattern =
    functionNameMapWithRange
        (\range var ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.VarPattern var)
            }
        )


numberPart : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n -> { comments = Rope.empty, syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.IntPattern n) })
        (\range n -> { comments = Rope.empty, syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.HexPattern n) })


charPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
charPattern =
    characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty, syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.CharPattern char) }
        )


listPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = Elm.Syntax.Node.Node range patternListEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.ListPattern elements.syntax)
                    }
        )
        (ParserFast.symbolFollowedBy "[" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    Just
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                                |> Rope.prependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                pattern
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (Elm.Parser.Layout.surroundedByWhitespaceAndCommentsEndsPositivelyIndented pattern)
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : Elm.Syntax.Pattern.Pattern
patternListEmpty =
    Elm.Syntax.Pattern.ListPattern []


composablePattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
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


patternNotSpaceSeparated : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
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


allPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range Elm.Syntax.Pattern.AllPattern
            }
        )


stringPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
stringPattern =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.StringPattern string)
            }
        )


qualifiedPatternWithConsumeArgs : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
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
            { comments = afterStartName |> Rope.prependTo argsReverse.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Pattern.NamedPattern
                        name
                        (List.reverse argsReverse.syntax)
                    )
            }
        )
        qualifiedNameRefNode
        Elm.Parser.Layout.whitespaceAndComments
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfterArg ->
                        { comments = arg.comments |> Rope.prependTo commentsAfterArg
                        , syntax = arg.syntax
                        }
                    )
                    patternNotSpaceSeparated
                    Elm.Parser.Layout.whitespaceAndComments
                )
            )
        )


qualifiedNameRefNode : ParserFast.Parser (Elm.Syntax.Node.Node Elm.Syntax.Pattern.QualifiedNameRef)
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
        typeName
        maybeDotTypeNamesTuple


qualifiedPatternWithoutConsumeArgs : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
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
        typeName
        maybeDotTypeNamesTuple


recordPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            { comments = commentsBeforeElements |> Rope.prependTo elements.comments
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.RecordPattern elements.syntax)
            }
        )
        (ParserFast.symbolFollowedBy "{" Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    { comments =
                        commentsAfterHead
                            |> Rope.prependTo tail.comments
                    , syntax = head :: tail.syntax
                    }
                )
                functionNameNode
                Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map3
                            (\beforeName name afterName ->
                                { comments = beforeName |> Rope.prependTo afterName
                                , syntax = name
                                }
                            )
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                            functionNameNode
                            Elm.Parser.Layout.whitespaceAndCommentsEndsPositivelyIndented
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" { comments = Rope.empty, syntax = [] })
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


escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
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


characterLiteralMapWithRange : (Elm.Syntax.Range.Range -> Char -> res) -> ParserFast.Parser res
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


singleOrTripleQuotedStringLiteralMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> ParserFast.Parser res
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


singleQuotedStringLiteralAfterDoubleQuote : ParserFast.Parser String
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


tripleQuotedStringLiteralOfterTripleDoubleQuote : ParserFast.Parser String
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


functionName : ParserFast.Parser String
functionName =
    ParserFast.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNode : ParserFast.Parser (Elm.Syntax.Node.Node String)
functionNameNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> ParserFast.Parser res
functionNameMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak
        rangeAndNameToResult
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNotInfixNode : ParserFast.Parser (Elm.Syntax.Node.Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        (\name -> name /= "infix" && isNotReserved name)


typeName : ParserFast.Parser String
typeName =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


typeNameMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> ParserFast.Parser res
typeNameMapWithRange rangeAndNameToRes =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndNameToRes
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


typeNameNode : ParserFast.Parser (Elm.Syntax.Node.Node String)
typeNameNode =
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

        _ ->
            False
