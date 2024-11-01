module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Char.Extra
import Elm.Syntax.Documentation
import Elm.Syntax.Node
import ParserFast


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


moduleDocumentation : ParserFast.Parser (Elm.Syntax.Node.Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : ParserFast.Parser (Elm.Syntax.Node.Node Elm.Syntax.Documentation.Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    multiLineCommentNoCheck
