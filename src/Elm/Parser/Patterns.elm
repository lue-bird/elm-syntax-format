module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import Elm.Parser.Layout
import Elm.Parser.Tokens
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import ParserFast
import ParserWithComments
import Rope


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
                Elm.Parser.Layout.maybeLayout
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
                    Elm.Parser.Layout.maybeLayout
                    (ParserFast.lazy (\() -> composablePattern))
                    Elm.Parser.Layout.maybeLayout
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
                    Elm.Parser.Layout.maybeLayout
                    Elm.Parser.Tokens.functionNameNode
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
            Elm.Parser.Layout.maybeLayout
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
                    Elm.Parser.Layout.maybeLayout
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
                                Elm.Parser.Layout.maybeLayout
                                pattern
                                Elm.Parser.Layout.maybeLayout
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
                                            Elm.Parser.Layout.maybeLayout
                                            pattern
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
        )


varPattern : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
varPattern =
    Elm.Parser.Tokens.functionNameMapWithRange
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
    Elm.Parser.Tokens.characterLiteralMapWithRange
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
        (ParserFast.symbolFollowedBy "[" Elm.Parser.Layout.maybeLayout)
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
                Elm.Parser.Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (Elm.Parser.Layout.maybeAroundBothSides pattern)
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


patternNotDirectlyComposing : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern))
patternNotDirectlyComposing =
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
    Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Elm.Syntax.Node.Node range (Elm.Syntax.Pattern.StringPattern string)
            }
        )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\startName afterStartName ->
            case afterStartName of
                Nothing ->
                    Just ( [], startName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( startName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." Elm.Parser.Tokens.typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


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
        Elm.Parser.Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Elm.Parser.Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfterArg ->
                        { comments = arg.comments |> Rope.prependTo commentsAfterArg
                        , syntax = arg.syntax
                        }
                    )
                    patternNotDirectlyComposing
                    Elm.Parser.Layout.optimisticLayout
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
        Elm.Parser.Tokens.typeName
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
        Elm.Parser.Tokens.typeName
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
        (ParserFast.symbolFollowedBy "{" Elm.Parser.Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    { comments =
                        commentsAfterHead
                            |> Rope.prependTo tail.comments
                    , syntax = head :: tail.syntax
                    }
                )
                Elm.Parser.Tokens.functionNameNode
                Elm.Parser.Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map3
                            (\beforeName name afterName ->
                                { comments = beforeName |> Rope.prependTo afterName
                                , syntax = name
                                }
                            )
                            Elm.Parser.Layout.maybeLayout
                            Elm.Parser.Tokens.functionNameNode
                            Elm.Parser.Layout.maybeLayout
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" { comments = Rope.empty, syntax = [] })
        )
