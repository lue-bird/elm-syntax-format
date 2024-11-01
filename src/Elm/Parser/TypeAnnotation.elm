module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout
import Elm.Parser.Tokens
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import ParserFast
import ParserWithComments
import Rope


typeAnnotation : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeAnnotation =
    ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
        (ParserFast.map2
            (\startType commentsAfter ->
                { comments =
                    startType.comments
                        |> Rope.prependTo commentsAfter
                , syntax = startType.syntax
                }
            )
            (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
            Elm.Parser.Layout.optimisticLayout
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
                    Elm.Parser.Layout.maybeLayout
                    (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
                    Elm.Parser.Layout.optimisticLayout
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


typeAnnotationNoFnExcludingTypedWithArguments : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithoutArguments
        genericTypeAnnotation
        recordTypeAnnotation


typeAnnotationNoFnIncludingTypedWithArguments : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
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
                Elm.Parser.Layout.maybeLayout
                typeAnnotation
                Elm.Parser.Layout.maybeLayout
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
                            Elm.Parser.Layout.maybeLayout
                            typeAnnotation
                            Elm.Parser.Layout.maybeLayout
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
                                        Elm.Parser.Layout.maybeLayout
                                        typeAnnotation
                                        Elm.Parser.Layout.maybeLayout
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
    Elm.Parser.Tokens.functionNameMapWithRange
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
        (ParserFast.symbolFollowedBy "{" Elm.Parser.Layout.maybeLayout)
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
                Elm.Parser.Tokens.functionNameNode
                Elm.Parser.Layout.maybeLayout
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
                            Elm.Parser.Layout.maybeLayout
                            recordFieldDefinition
                            (ParserWithComments.many
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore field ->
                                            { comments = commentsBefore |> Rope.prependTo field.comments
                                            , syntax = field.syntax
                                            }
                                        )
                                        Elm.Parser.Layout.maybeLayout
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
                            Elm.Parser.Layout.maybeLayout
                            typeAnnotation
                            Elm.Parser.Layout.maybeLayout
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
        Elm.Parser.Layout.maybeLayout
        recordFieldDefinition
        (ParserWithComments.many
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map2
                    (\commentsBefore field ->
                        { comments = commentsBefore |> Rope.prependTo field.comments
                        , syntax = field.syntax
                        }
                    )
                    Elm.Parser.Layout.maybeLayout
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
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.Tokens.functionNameNode
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Elm.Parser.Layout.maybeLayout)
        typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Elm.Parser.Layout.maybeLayout


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
        Elm.Parser.Tokens.typeName
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
        (ParserFast.symbolFollowedBy "." Elm.Parser.Tokens.typeName)
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
            Elm.Parser.Tokens.typeName
            maybeDotTypeNamesTuple
        )
        Elm.Parser.Layout.optimisticLayout
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
                    typeAnnotationNoFnExcludingTypedWithArguments
                    Elm.Parser.Layout.optimisticLayout
                )
            )
        )
