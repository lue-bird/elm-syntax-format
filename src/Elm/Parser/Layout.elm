module Elm.Parser.Layout exposing
    ( endsPositivelyIndented
    , moduleLevelIndentedFollowedBy
    , multilineComment
    , positivelyIndentedFollowedBy
    , positivelyIndentedPlusFollowedBy
    , singleLineComment
    , surroundedByWhitespaceAndCommentsEndsPositivelyIndented
    , topIndentedFollowedBy
    , whitespaceAndComments
    , whitespaceAndCommentsEndsPositivelyIndented
    , whitespaceAndCommentsEndsTopIndented
    , whitespaceAndCommentsEndsTopIndentedFollowedBy
    , whitespaceAndCommentsEndsTopIndentedFollowedByComments
    , whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
    )

import Char.Extra
import Elm.Syntax.Node
import ParserFast
import ParserWithComments
import Rope


singleLineComment : ParserFast.Parser (Elm.Syntax.Node.Node String)
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


multilineComment : ParserFast.Parser (Elm.Syntax.Node.Node String)
multilineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice (offset + 2) (offset + 3) source of
                "|" ->
                    ParserFast.problem

                _ ->
                    multiLineCommentNoCheck
        )


multiLineCommentNoCheck : ParserFast.Parser (Elm.Syntax.Node.Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiCommentMapWithRange Elm.Syntax.Node.Node
        ( '{', "-" )
        ( '-', "}" )


whitespaceAndComments : ParserFast.Parser ParserWithComments.Comments
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
        (multilineComment
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
        (singleLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : ParserFast.Parser ParserWithComments.Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            singleLineComment
            multilineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        Rope.empty
        (\right soFar -> soFar |> Rope.prependToFilled (Rope.one right))
        identity


whitespaceAndCommentsEndsPositivelyIndented : ParserFast.Parser ParserWithComments.Comments
whitespaceAndCommentsEndsPositivelyIndented =
    whitespaceAndComments |> endsPositivelyIndented


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


whitespaceAndCommentsEndsTopIndentedFollowedByComments : ParserFast.Parser ParserWithComments.Comments -> ParserFast.Parser ParserWithComments.Comments
whitespaceAndCommentsEndsTopIndentedFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> Rope.prependTo afterComments
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedByWithComments : ParserFast.Parser (ParserWithComments.WithComments syntax) -> ParserFast.Parser (ParserWithComments.WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore |> Rope.prependTo after.comments
            , syntax = after.syntax
            }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedBy : ParserFast.Parser syntax -> ParserFast.Parser (ParserWithComments.WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore, syntax = after }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndented : ParserFast.Parser ParserWithComments.Comments
whitespaceAndCommentsEndsTopIndented =
    whitespaceAndComments |> endsTopIndented


moduleLevelIndentedFollowedBy : ParserFast.Parser a -> ParserFast.Parser a
moduleLevelIndentedFollowedBy nextParser =
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


topIndentedFollowedBy : ParserFast.Parser a -> ParserFast.Parser a
topIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column - indent == 0 then
                nextParser

            else
                ParserFast.problem
        )


surroundedByWhitespaceAndCommentsEndsPositivelyIndented : ParserFast.Parser (ParserWithComments.WithComments b) -> ParserFast.Parser (ParserWithComments.WithComments b)
surroundedByWhitespaceAndCommentsEndsPositivelyIndented x =
    ParserFast.map3
        (\before v after ->
            { comments =
                before
                    |> Rope.prependTo v.comments
                    |> Rope.prependTo after
            , syntax = v.syntax
            }
        )
        whitespaceAndCommentsEndsPositivelyIndented
        x
        whitespaceAndCommentsEndsPositivelyIndented
