module Elm.Parser.Tokens exposing
    ( isAllowedOperatorToken, isOperatorSymbolCharAsString
    , characterLiteralMapWithRange, singleOrTripleQuotedStringLiteralMapWithRange
    , functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode, typeNameMapWithRange
    )

{-|

@docs isAllowedOperatorToken, isOperatorSymbolCharAsString

@docs characterLiteralMapWithRange, singleOrTripleQuotedStringLiteralMapWithRange
@docs functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode, typeNameMapWithRange

-}

import Char
import Char.Extra
import Elm.Syntax.Node
import Elm.Syntax.Range
import ParserFast


isNotReserved : String -> Bool
isNotReserved name =
    case name of
        "module" ->
            False

        "exposing" ->
            False

        "import" ->
            False

        "as" ->
            False

        "if" ->
            False

        "then" ->
            False

        "else" ->
            False

        "let" ->
            False

        "in" ->
            False

        "case" ->
            False

        "of" ->
            False

        "port" ->
            False

        --"infixr"
        --"infixl"
        "type" ->
            False

        -- "infix" Apparently this is not a reserved keyword
        -- "alias" Apparently this is not a reserved keyword
        "where" ->
            False

        _ ->
            True


escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf7
        (ParserFast.symbol "'" (charToRes '\''))
        (ParserFast.symbol "\"" (charToRes '"'))
        (ParserFast.symbol "n" (charToRes '\n'))
        (ParserFast.symbol "t" (charToRes '\t'))
        -- even though elm-format will change \r to a unicode version. When you don't use elm-format, this will not happen.
        (ParserFast.symbol "r" (charToRes '\u{000D}'))
        (ParserFast.symbol "\\" (charToRes '\\'))
        (ParserFast.symbolFollowedBy "u{"
            (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    charToRes (Char.fromCode (hexStringToInt hex))
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserFast.followedBySymbol "}"
            )
        )


hexStringToInt : String -> Int
hexStringToInt string =
    String.foldr
        (\c soFar ->
            { exponent = soFar.exponent + 1
            , result = soFar.result + 16 ^ soFar.exponent * charToHex c
            }
        )
        { exponent = 0, result = 0 }
        string
        |> .result


charToHex : Char -> Int
charToHex c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        -- 'F'
        _ ->
            15


characterLiteralMapWithRange : (Elm.Syntax.Range.Range -> Char -> res) -> ParserFast.Parser res
characterLiteralMapWithRange rangeAndCharToRes =
    ParserFast.symbolFollowedBy "'"
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity))
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            ParserFast.anyChar
            |> ParserFast.followedBySymbol "'"
        )


singleOrTripleQuotedStringLiteralMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> ParserFast.Parser res
singleOrTripleQuotedStringLiteralMapWithRange rangeAndStringToRes =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            (ParserFast.symbolFollowedBy "\"\""
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : ParserFast.Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"" ())
        (ParserFast.oneOf2
            (ParserFast.whileAtLeast1WithoutLinebreak (\c -> c /= '"' && c /= '\\' && not (Char.Extra.isUtf16Surrogate c)))
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : ParserFast.Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"\"\"" ())
        (ParserFast.oneOf3
            (ParserFast.symbol "\"" "\"")
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.while (\c -> c /= '"' && c /= '\\' && not (Char.Extra.isUtf16Surrogate c)))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


functionName : ParserFast.Parser String
functionName =
    ParserFast.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNode : ParserFast.Parser (Elm.Syntax.Node.Node String)
functionNameNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> ParserFast.Parser res
functionNameMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak
        rangeAndNameToResult
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNotInfixNode : ParserFast.Parser (Elm.Syntax.Node.Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        (\name -> name /= "infix" && isNotReserved name)


typeName : ParserFast.Parser String
typeName =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


typeNameMapWithRange : (Elm.Syntax.Range.Range -> String -> res) -> ParserFast.Parser res
typeNameMapWithRange rangeAndNameToRes =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndNameToRes
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


typeNameNode : ParserFast.Parser (Elm.Syntax.Node.Node String)
typeNameNode =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak Elm.Syntax.Node.Node
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


isAllowedOperatorToken : String -> Bool
isAllowedOperatorToken operatorCandidateToValidate =
    case operatorCandidateToValidate of
        "==" ->
            True

        "/=" ->
            True

        "::" ->
            True

        "++" ->
            True

        "+" ->
            True

        "*" ->
            True

        "<|" ->
            True

        "|>" ->
            True

        "||" ->
            True

        "<=" ->
            True

        ">=" ->
            True

        "|=" ->
            True

        "|." ->
            True

        "//" ->
            True

        "</>" ->
            True

        "<?>" ->
            True

        "^" ->
            True

        "<<" ->
            True

        ">>" ->
            True

        "<" ->
            True

        ">" ->
            True

        "/" ->
            True

        "&&" ->
            True

        "-" ->
            True

        _ ->
            False


isOperatorSymbolCharAsString : String -> Bool
isOperatorSymbolCharAsString c =
    case c of
        "|" ->
            True

        "+" ->
            True

        "<" ->
            True

        ">" ->
            True

        "=" ->
            True

        "*" ->
            True

        ":" ->
            True

        "-" ->
            True

        "/" ->
            True

        "&" ->
            True

        "." ->
            True

        "?" ->
            True

        "^" ->
            True

        _ ->
            False
