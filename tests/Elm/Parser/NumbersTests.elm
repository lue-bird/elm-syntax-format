module Elm.Parser.NumbersTests exposing (all)

import Elm.Parser.TestUtil
import Expect
import ParserFast
import Test


all : Test.Test
all =
    Test.describe "NumbersTests"
        [ Test.describe "integerDecimalOrHexadecimalMapWithRange"
            [ Test.test "hex"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "0x03FFFFFF" (ParserFast.integerDecimalOrHexadecimalMapWithRange (\_ _ -> -1) (\_ n -> n))
                        |> Expect.equal
                            (Just 67108863)
                )
            , Test.test "hex - 2"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "0xFF" (ParserFast.integerDecimalOrHexadecimalMapWithRange (\_ _ -> -1) (\_ n -> n))
                        |> Expect.equal
                            (Just 255)
                )
            ]
        , Test.describe "floatOrIntegerDecimalOrHexadecimalMapWithRange"
            [ Test.test "hex"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "0x2A"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                            (\_ n -> n)
                        )
                        |> Expect.equal
                            (Just 42)
                )
            , Test.test "float"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2.0"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0)
                )
            , Test.test "integer with negative exponent"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2e-2"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0e-2)
                )
            , Test.test "integer with negative exponent (uppercase E)"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2E-2"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0e-2)
                )
            , Test.test "integer with positive exponent"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2e+2"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0e2)
                )
            , Test.test "float with negative exponent"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2.0e-2"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0e-2)
                )
            , Test.test "float with negative exponent (uppercase E)"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2.0E-2"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0e-2)
                )
            , Test.test "float with positive exponent"
                (\() ->
                    Elm.Parser.TestUtil.parseToResult "2.0e+2"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ n -> n)
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                        )
                        |> Expect.equal
                            (Just 2.0e2)
                )
            ]
        ]
