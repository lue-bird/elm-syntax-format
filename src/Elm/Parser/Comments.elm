module Elm.Parser.Comments exposing (documentationComment)

import Elm.Syntax.Documentation
import Elm.Syntax.Node
import ParserFast


documentationComment : ParserFast.Parser (Elm.Syntax.Node.Node Elm.Syntax.Documentation.Documentation)
documentationComment =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiCommentMapWithRange Elm.Syntax.Node.Node
        ( '{', "-" )
        ( '-', "}" )
