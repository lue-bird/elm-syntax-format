module Elm.Parser.Declarations exposing (declaration)

import Elm.Parser.Comments
import Elm.Parser.Expression
import Elm.Parser.Layout
import Elm.Parser.Patterns
import Elm.Parser.Tokens
import Elm.Parser.TypeAnnotation
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation
import ParserFast
import ParserWithComments
import Rope


declaration : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
declaration =
    ParserFast.oneOf5
        functionDeclarationWithoutDocumentation
        declarationWithDocumentation
        typeOrTypeAliasDefinitionWithoutDocumentation
        portDeclarationWithoutDocumentation
        infixDeclaration


declarationWithDocumentation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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
                                                (Elm.Syntax.Node.combine (\name typeAnnotation -> { name = name, typeAnnotation = typeAnnotation })
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
        Elm.Parser.Comments.declarationDocumentation
        (Elm.Parser.Layout.layoutStrictFollowedByWithComments
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


functionAfterDocumentation : ParserFast.Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
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
        Elm.Parser.Tokens.functionNameNode
        Elm.Parser.Layout.maybeLayout
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
            (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.maybeLayout)
            Elm.Parser.TypeAnnotation.typeAnnotation
            (Elm.Parser.Layout.layoutStrictFollowedBy
                Elm.Parser.Tokens.functionNameNode
            )
            Elm.Parser.Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.Expression.expressionFollowedByOptimisticLayout


functionDeclarationWithoutDocumentation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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
        Elm.Parser.Tokens.functionNameNotInfixNode
        Elm.Parser.Layout.maybeLayout
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
            (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.maybeLayout)
            Elm.Parser.TypeAnnotation.typeAnnotation
            (Elm.Parser.Layout.layoutStrictFollowedBy
                Elm.Parser.Tokens.functionNameNode
            )
            Elm.Parser.Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.Expression.expressionFollowedByOptimisticLayout
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


parameterPatternsEqual : ParserFast.Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Elm.Parser.Tokens.equal
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            Elm.Parser.Patterns.patternNotDirectlyComposing
            Elm.Parser.Layout.maybeLayout
        )


infixDeclaration : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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
        (ParserFast.keywordFollowedBy "infix" Elm.Parser.Layout.maybeLayout)
        infixDirection
        Elm.Parser.Layout.maybeLayout
        (ParserFast.integerDecimalMapWithRange Elm.Syntax.Node.Node)
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy "("
            (ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                (\operatorRange operator ->
                    Elm.Syntax.Node.Node
                        { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                        , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                        }
                        operator
                )
                Elm.Parser.Tokens.isOperatorSymbolCharAsString
                Elm.Parser.Tokens.isAllowedOperatorToken
                ")"
            )
        )
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Tokens.functionNameNode


infixDirection : ParserFast.Parser (Elm.Syntax.Node.Node Elm.Syntax.Infix.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "right" Elm.Syntax.Infix.Right))
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "left" Elm.Syntax.Infix.Left))
        (ParserFast.mapWithRange Elm.Syntax.Node.Node (ParserFast.keyword "non" Elm.Syntax.Infix.Non))


portDeclarationAfterDocumentation : ParserFast.Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
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
        (ParserFast.keywordFollowedBy "port" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Tokens.functionNameNode
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.TypeAnnotation.typeAnnotation


portDeclarationWithoutDocumentation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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
        (ParserFast.keywordFollowedBy "port" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Tokens.functionNameNode
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.TypeAnnotation.typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : ParserFast.Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    ParserFast.map2
        (\commentsAfterType declarationAfterDocumentation ->
            { comments = commentsAfterType |> Rope.prependTo declarationAfterDocumentation.comments
            , syntax = declarationAfterDocumentation.syntax
            }
        )
        (ParserFast.keywordFollowedBy "type" Elm.Parser.Layout.maybeLayout)
        (ParserFast.oneOf2
            typeAliasDefinitionAfterDocumentationAfterTypePrefix
            customTypeDefinitionAfterDocumentationAfterTypePrefix
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : ParserFast.Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
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
        (ParserFast.keywordFollowedBy "alias" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Tokens.typeNameNode
        Elm.Parser.Layout.maybeLayout
        typeGenericListEquals
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.TypeAnnotation.typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : ParserFast.Parser (ParserWithComments.WithComments DeclarationAfterDocumentation)
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
        Elm.Parser.Tokens.typeNameNode
        Elm.Parser.Layout.maybeLayout
        typeGenericListEquals
        Elm.Parser.Layout.maybeLayout
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
                        Elm.Parser.Layout.maybeLayout
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration))
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
            Elm.Parser.Layout.maybeLayout
        )
        (ParserFast.oneOf2
            typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            customTypeDefinitionWithoutDocumentationAfterTypePrefix
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : ParserFast.Parser (ParserWithComments.WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
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
        (ParserFast.keywordFollowedBy "alias" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Tokens.typeNameNode
        Elm.Parser.Layout.maybeLayout
        typeGenericListEquals
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.TypeAnnotation.typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : ParserFast.Parser (ParserWithComments.WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
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
        Elm.Parser.Tokens.typeNameNode
        Elm.Parser.Layout.maybeLayout
        typeGenericListEquals
        Elm.Parser.Layout.maybeLayout
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
                        Elm.Parser.Layout.maybeLayout
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


valueConstructorOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Type.ValueConstructor))
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
        Elm.Parser.Tokens.typeNameNode
        Elm.Parser.Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments = typeAnnotationResult.comments |> Rope.prependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    Elm.Parser.TypeAnnotation.typeAnnotationNoFnExcludingTypedWithArguments
                    Elm.Parser.Layout.optimisticLayout
                )
            )
        )


typeGenericListEquals : ParserFast.Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node String)))
typeGenericListEquals =
    ParserWithComments.until Elm.Parser.Tokens.equal
        (ParserFast.map2
            (\name commentsAfterName ->
                { comments = commentsAfterName
                , syntax = name
                }
            )
            Elm.Parser.Tokens.functionNameNode
            Elm.Parser.Layout.maybeLayout
        )