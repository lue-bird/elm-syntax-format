module Elm.Parser.TestUtil exposing (parse, parseToResult)

import ParserFast


parse : String -> ParserFast.Parser a -> Maybe a
parse source p =
    ParserFast.run p source


parseToResult : String -> ParserFast.Parser a -> Maybe a
parseToResult source p =
    ParserFast.run p source
