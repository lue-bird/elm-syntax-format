module Elm.Parser.Layout exposing
    ( endsPositivelyIndented
    , layoutStrict
    , layoutStrictFollowedBy
    , layoutStrictFollowedByComments
    , layoutStrictFollowedByWithComments
    , maybeAroundBothSides
    , maybeLayout
    , moduleLevelIndentationFollowedBy
    , onTopIndentationFollowedBy
    , optimisticLayout
    , positivelyIndentedFollowedBy
    , positivelyIndentedPlusFollowedBy
    )

import Elm.Parser.Comments
import ParserFast
import ParserWithComments
import Rope


whitespaceAndCommentsOrEmpty : ParserFast.Parser ParserWithComments.Comments
whitespaceAndCommentsOrEmpty =
    ParserFast.skipWhileWhitespaceBacktrackableFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserFast.offsetSourceAndThenOrSucceed
            (\offset source ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to Rope.empty
                        Just fromSingleLineCommentNode

                    "{-" ->
                        Just fromMultilineCommentNodeOrEmptyOnProblem

                    _ ->
                        Nothing
            )
            Rope.empty
        )


fromMultilineCommentNodeOrEmptyOnProblem : ParserFast.Parser ParserWithComments.Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserFast.map2OrSucceed
        (\comment commentsAfter ->
            Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        (Elm.Parser.Comments.multilineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        Rope.empty


fromSingleLineCommentNode : ParserFast.Parser ParserWithComments.Comments
fromSingleLineCommentNode =
    ParserFast.map2
        (\content commentsAfter ->
            Rope.one content |> Rope.filledPrependTo commentsAfter
        )
        (Elm.Parser.Comments.singleLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : ParserFast.Parser ParserWithComments.Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            Elm.Parser.Comments.singleLineComment
            Elm.Parser.Comments.multilineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        Rope.empty
        (\right soFar -> soFar |> Rope.prependToFilled (Rope.one right))
        identity


maybeLayout : ParserFast.Parser ParserWithComments.Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty |> endsPositivelyIndented


endsPositivelyIndented : ParserFast.Parser a -> ParserFast.Parser a
endsPositivelyIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent -> column > indent)
        parser


{-| Check that the indentation of an already parsed token
would be valid after [`maybeLayout`](#maybeLayout)
-}
positivelyIndentedPlusFollowedBy : Int -> ParserFast.Parser a -> ParserFast.Parser a
positivelyIndentedPlusFollowedBy extraIndent nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column > indent + extraIndent then
                nextParser

            else
                ParserFast.problem
        )


positivelyIndentedFollowedBy : ParserFast.Parser a -> ParserFast.Parser a
positivelyIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column > indent then
                nextParser

            else
                ParserFast.problem
        )


optimisticLayout : ParserFast.Parser ParserWithComments.Comments
optimisticLayout =
    whitespaceAndCommentsOrEmpty


layoutStrictFollowedByComments : ParserFast.Parser ParserWithComments.Comments -> ParserFast.Parser ParserWithComments.Comments
layoutStrictFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> Rope.prependTo afterComments
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrictFollowedByWithComments : ParserFast.Parser (ParserWithComments.WithComments syntax) -> ParserFast.Parser (ParserWithComments.WithComments syntax)
layoutStrictFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore |> Rope.prependTo after.comments
            , syntax = after.syntax
            }
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrictFollowedBy : ParserFast.Parser syntax -> ParserFast.Parser (ParserWithComments.WithComments syntax)
layoutStrictFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore, syntax = after }
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrict : ParserFast.Parser ParserWithComments.Comments
layoutStrict =
    optimisticLayout |> endsTopIndented


moduleLevelIndentationFollowedBy : ParserFast.Parser a -> ParserFast.Parser a
moduleLevelIndentationFollowedBy nextParser =
    ParserFast.columnAndThen
        (\column ->
            if column == 1 then
                nextParser

            else
                ParserFast.problem
        )


endsTopIndented : ParserFast.Parser a -> ParserFast.Parser a
endsTopIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent -> column - indent == 0)
        parser


onTopIndentationFollowedBy : ParserFast.Parser a -> ParserFast.Parser a
onTopIndentationFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column - indent == 0 then
                nextParser

            else
                ParserFast.problem
        )


maybeAroundBothSides : ParserFast.Parser (ParserWithComments.WithComments b) -> ParserFast.Parser (ParserWithComments.WithComments b)
maybeAroundBothSides x =
    ParserFast.map3
        (\before v after ->
            { comments =
                before
                    |> Rope.prependTo v.comments
                    |> Rope.prependTo after
            , syntax = v.syntax
            }
        )
        maybeLayout
        x
        maybeLayout
