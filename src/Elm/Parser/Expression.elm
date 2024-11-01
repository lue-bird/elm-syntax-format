module Elm.Parser.Expression exposing (expressionFollowedByOptimisticLayout)

import Elm.Parser.Layout
import Elm.Parser.Patterns
import Elm.Parser.Tokens
import Elm.Parser.TypeAnnotation
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import ParserFast
import ParserWithComments
import Rope


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
        (ParserFast.symbolFollowedBy "." Elm.Parser.Tokens.functionNameNode)
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
            Elm.Parser.Layout.maybeLayout
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
                                    Elm.Parser.Layout.maybeLayout
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
            Elm.Parser.Layout.maybeLayout
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
            Elm.Parser.Tokens.functionNameNode
            Elm.Parser.Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore setterResult ->
                            { comments = commentsBefore |> Rope.prependTo setterResult.comments
                            , syntax = RecordUpdateFirstSetter setterResult.syntax
                            }
                        )
                        Elm.Parser.Layout.maybeLayout
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
                        Elm.Parser.Layout.maybeLayout
                        expressionFollowedByOptimisticLayout
                        |> Elm.Parser.Layout.endsPositivelyIndented
                    )
                )
            )
            recordFields
            (Elm.Parser.Layout.maybeLayout |> ParserFast.followedBySymbol "}")
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
                Elm.Parser.Layout.maybeLayout
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
        Elm.Parser.Tokens.functionNameNode
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        |> Elm.Parser.Layout.endsPositivelyIndented


literalExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
literalExpression =
    Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string)
            }
        )


charLiteralExpression : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
charLiteralExpression =
    Elm.Parser.Tokens.characterLiteralMapWithRange
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
        (ParserFast.symbolFollowedBy "\\" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Patterns.patternNotDirectlyComposing
        Elm.Parser.Layout.maybeLayout
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
                Elm.Parser.Patterns.patternNotDirectlyComposing
                Elm.Parser.Layout.maybeLayout
            )
        )
        Elm.Parser.Layout.maybeLayout
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
        (ParserFast.keywordFollowedBy "case" Elm.Parser.Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "of" Elm.Parser.Layout.maybeLayout)
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
        Elm.Parser.Patterns.pattern
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy "->" Elm.Parser.Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        (ParserWithComments.manyWithoutReverse caseStatementFollowedByOptimisticLayout)


caseStatementFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments Elm.Syntax.Expression.Case)
caseStatementFollowedByOptimisticLayout =
    Elm.Parser.Layout.onTopIndentationFollowedBy
        (ParserFast.map4
            (\pattern commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    pattern.comments
                        |> Rope.prependTo commentsBeforeArrowRight
                        |> Rope.prependTo commentsAfterArrowRight
                        |> Rope.prependTo expr.comments
                , syntax = ( pattern.syntax, expr.syntax )
                }
            )
            Elm.Parser.Patterns.pattern
            Elm.Parser.Layout.maybeLayout
            (ParserFast.symbolFollowedBy "->" Elm.Parser.Layout.maybeLayout)
            expressionFollowedByOptimisticLayout
        )



-- Let Expression


letExpressionFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
letExpressionFollowedByOptimisticLayout =
    ParserFast.map3WithStartLocation
        (\start declarations commentsAfterIn expressionResult ->
            let
                (Elm.Syntax.Node.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                declarations.comments
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (Elm.Syntax.Expression.LetExpression
                        { declarations = declarations.declarations
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.withIndentSetToColumn
            (ParserFast.keywordFollowedBy "let"
                (ParserFast.map2
                    (\commentsAfterLet declarations ->
                        { comments =
                            commentsAfterLet
                                |> Rope.prependTo declarations.comments
                        , declarations = declarations.syntax
                        }
                    )
                    Elm.Parser.Layout.maybeLayout
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
        )
        -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        (Elm.Parser.Layout.positivelyIndentedPlusFollowedBy 2
            Elm.Parser.Layout.maybeLayout
        )
        expressionFollowedByOptimisticLayout


letDeclarationsIn : ParserFast.Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration)))
letDeclarationsIn =
    Elm.Parser.Layout.onTopIndentationFollowedBy
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
            Elm.Parser.Layout.optimisticLayout
            (ParserWithComments.until Elm.Parser.Tokens.inToken letBlockElementFollowedByOptimisticLayout)
        )


letBlockElementFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letBlockElementFollowedByOptimisticLayout =
    Elm.Parser.Layout.onTopIndentationFollowedBy
        (ParserFast.oneOf2
            letFunctionFollowedByOptimisticLayout
            letDestructuringDeclarationFollowedByOptimisticLayout
        )


letDestructuringDeclarationFollowedByOptimisticLayout : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration))
letDestructuringDeclarationFollowedByOptimisticLayout =
    ParserFast.map4
        (\pattern commentsAfterPattern commentsAfterEquals expressionResult ->
            let
                (Elm.Syntax.Node.Node patternRange _) =
                    pattern.syntax

                (Elm.Syntax.Node.Node destructuredExpressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                pattern.comments
                    |> Rope.prependTo commentsAfterPattern
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Elm.Syntax.Node.Node { start = patternRange.start, end = destructuredExpressionRange.end }
                    (Elm.Syntax.Expression.LetDestructuring pattern.syntax expressionResult.syntax)
            }
        )
        Elm.Parser.Patterns.patternNotDirectlyComposing
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.maybeLayout)
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
                                        (Elm.Syntax.Node.combine (\name typeAnnotation -> { name = name, typeAnnotation = typeAnnotation })
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
        (ParserFast.keywordFollowedBy "if" Elm.Parser.Layout.maybeLayout)
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "then" Elm.Parser.Layout.maybeLayout)
        )
        expressionFollowedByOptimisticLayout
        (Elm.Parser.Layout.positivelyIndentedFollowedBy
            (ParserFast.keywordFollowedBy "else" Elm.Parser.Layout.maybeLayout)
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
        Elm.Parser.Tokens.typeName
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
                    Elm.Parser.Tokens.typeName
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\name -> Just ( [], name ))
                Elm.Parser.Tokens.functionName
            )
        )
        Nothing


unqualifiedFunctionReferenceExpressionFollowedByRecordAccess : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression))
unqualifiedFunctionReferenceExpressionFollowedByRecordAccess =
    Elm.Parser.Tokens.functionNameMapWithRange
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
        (Elm.Parser.Tokens.functionNameMapWithRange
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
        Elm.Parser.Tokens.isOperatorSymbolCharAsString
        Elm.Parser.Tokens.isAllowedOperatorToken
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
        Elm.Parser.Layout.maybeLayout
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
                        Elm.Parser.Layout.maybeLayout
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
                                        Elm.Parser.Layout.maybeLayout
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
        Elm.Parser.Layout.maybeLayout
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
        Elm.Parser.Tokens.isOperatorSymbolCharAsString
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
        Elm.Parser.Layout.optimisticLayout


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
        Elm.Parser.Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfter ->
                        { comments = arg.comments |> Rope.prependTo commentsAfter
                        , syntax = arg.syntax
                        }
                    )
                    subExpression
                    Elm.Parser.Layout.optimisticLayout
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
