module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout
import Elm.Parser.Tokens
import Elm.Syntax.Import
import Elm.Syntax.Node
import ParserFast
import ParserWithComments
import Rope


importDefinition : ParserFast.Parser (ParserWithComments.WithComments (Elm.Syntax.Node.Node Elm.Syntax.Import.Import))
importDefinition =
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
        (ParserFast.keywordFollowedBy "import" Elm.Parser.Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Elm.Parser.Layout.optimisticLayout
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    { comments = commentsBefore |> Rope.prependTo commentsAfter
                    , syntax = moduleAliasNode
                    }
            )
            (ParserFast.keywordFollowedBy "as" Elm.Parser.Layout.maybeLayout)
            (Elm.Parser.Tokens.typeNameMapWithRange
                (\range moduleAlias ->
                    Elm.Syntax.Node.Node range [ moduleAlias ]
                )
            )
            Elm.Parser.Layout.optimisticLayout
            Nothing
        )
        (ParserFast.map2OrSucceed
            (\exposingResult commentsAfter ->
                Just
                    { comments = exposingResult.comments |> Rope.prependTo commentsAfter
                    , syntax = exposingResult.syntax
                    }
            )
            Elm.Parser.Expose.exposeDefinition
            Elm.Parser.Layout.optimisticLayout
            Nothing
        )
