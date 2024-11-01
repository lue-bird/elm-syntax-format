module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout
import Elm.Parser.Tokens
import Elm.Syntax.Module
import Elm.Syntax.Node
import List.Extra
import ParserFast
import ParserWithComments
import Rope


moduleDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
moduleDefinition =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : ParserFast.Parser (ParserWithComments.WithComments ( String, Elm.Syntax.Node.Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual typeName ->
            { comments = commentsAfterFnName |> Rope.prependTo commentsAfterEqual
            , syntax = ( fnName, typeName )
            }
        )
        Elm.Parser.Tokens.functionName
        Elm.Parser.Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Tokens.typeNameNode


whereBlock : ParserFast.Parser (ParserWithComments.WithComments { command : Maybe (Elm.Syntax.Node.Node String), subscription : Maybe (Elm.Syntax.Node.Node String) })
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
            Elm.Parser.Layout.maybeLayout
            effectWhereClause
            Elm.Parser.Layout.maybeLayout
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy "," (Elm.Parser.Layout.maybeAroundBothSides effectWhereClause))
            )
        )
        |> ParserFast.followedBySymbol "}"


effectWhereClauses : ParserFast.Parser (ParserWithComments.WithComments { command : Maybe (Elm.Syntax.Node.Node String), subscription : Maybe (Elm.Syntax.Node.Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            { comments = commentsBefore |> Rope.prependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" Elm.Parser.Layout.maybeLayout)
        whereBlock


effectModuleDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
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
        (ParserFast.keywordFollowedBy "effect" Elm.Parser.Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Elm.Parser.Layout.maybeLayout
        effectWhereClauses
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition


normalModuleDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
normalModuleDefinition =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.NormalModule
                        { moduleName = moduleName
                        , exposingList = exposingList.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "module" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition


portModuleDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Module.Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                Elm.Syntax.Node.Node range
                    (Elm.Syntax.Module.PortModule { moduleName = moduleName, exposingList = exposingList.syntax })
            }
        )
        (ParserFast.keywordFollowedBy "port" Elm.Parser.Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Elm.Parser.Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition
