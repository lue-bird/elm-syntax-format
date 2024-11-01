module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Tokens
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import ParserFast


moduleName : ParserFast.Parser (Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName)
moduleName =
    ParserFast.map2WithRange
        (\range head tail ->
            Elm.Syntax.Node.Node range (head :: tail)
        )
        Elm.Parser.Tokens.typeName
        (ParserFast.loopWhileSucceedsRightToLeftStackUnsafe
            (ParserFast.symbolFollowedBy "." Elm.Parser.Tokens.typeName)
            []
            (::)
        )
