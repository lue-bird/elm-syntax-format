module Elm.Parser.TestUtil exposing (parse)

import ParserFast


parse : String -> ParserFast.Parser a -> Maybe a
parse source p =
    ParserFast.run p source
