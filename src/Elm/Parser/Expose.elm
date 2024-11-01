module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout
import Elm.Parser.Tokens
import Elm.Syntax.Exposing
import Elm.Syntax.Node
import ParserFast
import ParserWithComments
import Rope


exposeDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing))
exposeDefinition =
    ParserFast.map3WithRange
        (\range commentsAfterExposing commentsBefore exposingListInnerResult ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo commentsBefore
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = Elm.Syntax.Node.Node range exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing" Elm.Parser.Layout.maybeLayout)
        (ParserFast.symbolFollowedBy "(" Elm.Parser.Layout.optimisticLayout)
        (exposingListInner
            |> ParserFast.followedBySymbol ")"
        )


exposingListInner : ParserFast.Parser (ParserWithComments.WithComments Elm.Syntax.Exposing.Exposing)
exposingListInner =
    ParserFast.oneOf2
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
            exposable
            Elm.Parser.Layout.maybeLayout
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy ","
                    (Elm.Parser.Layout.maybeAroundBothSides exposable)
                )
            )
        )
        (ParserFast.mapWithRange
            (\range commentsAfterDotDot ->
                { comments = commentsAfterDotDot
                , syntax = Elm.Syntax.Exposing.All range
                }
            )
            (ParserFast.symbolFollowedBy ".." Elm.Parser.Layout.maybeLayout)
        )


exposable : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
exposable =
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
        Elm.Parser.Tokens.parensEnd


typeExpose : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
typeExpose =
    ParserFast.map3
        (\(Elm.Syntax.Node.Node typeNameRange typeName) commentsBeforeMaybeOpen maybeOpen ->
            case maybeOpen of
                Nothing ->
                    { comments = commentsBeforeMaybeOpen
                    , syntax =
                        Elm.Syntax.Node.Node typeNameRange (Elm.Syntax.Exposing.TypeOrAliasExpose typeName)
                    }

                Just open ->
                    { comments = commentsBeforeMaybeOpen |> Rope.prependTo open.comments
                    , syntax =
                        Elm.Syntax.Node.Node
                            { start = typeNameRange.start
                            , end = open.syntax.end
                            }
                            (Elm.Syntax.Exposing.TypeExpose { name = typeName, open = Just open.syntax })
                    }
        )
        Elm.Parser.Tokens.typeNameNode
        Elm.Parser.Layout.optimisticLayout
        (ParserFast.map2WithRangeOrSucceed
            (\range left right ->
                Just { comments = left |> Rope.prependTo right, syntax = range }
            )
            (ParserFast.symbolFollowedBy "(" Elm.Parser.Layout.maybeLayout)
            (ParserFast.symbolFollowedBy ".." Elm.Parser.Layout.maybeLayout
                |> ParserFast.followedBySymbol ")"
            )
            Nothing
        )


functionExpose : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose))
functionExpose =
    Elm.Parser.Tokens.functionNameMapWithRange
        (\range name ->
            { comments = Rope.empty
            , syntax =
                Elm.Syntax.Node.Node range (Elm.Syntax.Exposing.FunctionExpose name)
            }
        )
