module Elm.Parser.NumbersTests exposing (all)

import Elm.Syntax.Expression
import Elm.Syntax.Node
import ElmSyntaxParserLenient
import Expect
import ParserFast
import Test


all : Test.Test
all =
    Test.describe "number"
        [ Test.test "long hex"
            (\() ->
                "0x03FFFFFF"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Hex 67108863))
            )
        , Test.test "hex FF"
            (\() ->
                "0xFF"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Hex 255))
            )
        , Test.test "hex"
            (\() ->
                "0x2A"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Hex 42))
            )
        , Test.test "float"
            (\() ->
                "2.0"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0))
            )
        , Test.test "integer with negative exponent"
            (\() ->
                "2e-2"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
            )
        , Test.test "integer with negative exponent (uppercase E)"
            (\() ->
                "2E-2"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
            )
        , Test.test "integer with positive exponent"
            (\() ->
                "2e+2"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0e2))
            )
        , Test.test "float with negative exponent"
            (\() ->
                "2.0e-2"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
            )
        , Test.test "float with negative exponent (uppercase E)"
            (\() ->
                "2.0E-2"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0e-2))
            )
        , Test.test "float with positive exponent"
            (\() ->
                "2.0e+2"
                    |> ParserFast.run ElmSyntaxParserLenient.expression
                    |> Maybe.map (\result -> result.syntax |> Elm.Syntax.Node.value)
                    |> Expect.equal
                        (Just (Elm.Syntax.Expression.Floatable 2.0e2))
            )
        ]
