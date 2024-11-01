module Elm.Parser.TokenTests exposing (all)

import Elm.Parser.Declarations
import Elm.Parser.TestUtil
import Elm.Parser.Tokens
import Expect
import Test


longString : String
longString =
    "\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\""


longMultiLineString : String
longMultiLineString =
    "\"\"\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\"\"\""


all : Test.Test
all =
    Test.describe "TokenTests"
        [ Test.test "functionName"
            (\() ->
                Elm.Parser.TestUtil.parse "foo" Elm.Parser.Tokens.functionName
                    |> Expect.equal (Just "foo")
            )
        , Test.test "functionName may not be a keyword"
            (\() ->
                Elm.Parser.TestUtil.parse "type" Elm.Parser.Tokens.functionName
                    |> Expect.equal Nothing
            )
        , Test.test "functionName may be a keyword suffixed with an underscore"
            (\() ->
                Elm.Parser.TestUtil.parse "type_" Elm.Parser.Tokens.functionName
                    |> Expect.equal (Just "type_")
            )
        , Test.test "functionName not empty"
            (\() ->
                Elm.Parser.TestUtil.parse "" Elm.Parser.Tokens.functionName
                    |> Expect.equal Nothing
            )
        , Test.test "functionName with number"
            (\() ->
                Elm.Parser.TestUtil.parse "n1" Elm.Parser.Tokens.functionName
                    |> Expect.equal (Just "n1")
            )
        , Test.test "alias can be a functionName (it is not reserved)"
            (\() ->
                Elm.Parser.TestUtil.parse "alias" Elm.Parser.Tokens.functionName
                    |> Expect.equal (Just "alias")
            )
        , Test.test "infix can be a functionName (it is not reserved)"
            (\() ->
                Elm.Parser.TestUtil.parse "infix" Elm.Parser.Tokens.functionName
                    |> Expect.equal (Just "infix")
            )
        , Test.test "functionName is not matched with 'if'"
            (\() ->
                Elm.Parser.TestUtil.parse "if" Elm.Parser.Tokens.functionName
                    |> Expect.equal Nothing
            )
        , Test.test "functionName with _"
            (\() ->
                Elm.Parser.TestUtil.parse "foo_" Elm.Parser.Tokens.functionName
                    |> Expect.equal (Just "foo_")
            )
        , Test.test "typeName"
            (\() ->
                Elm.Parser.TestUtil.parse "MyCmd" Elm.Parser.Tokens.typeName
                    |> Expect.equal (Just "MyCmd")
            )
        , Test.test "typeName not empty"
            (\() ->
                Elm.Parser.TestUtil.parse "" Elm.Parser.Tokens.typeName
                    |> Expect.equal Nothing
            )
        , Test.test "typeName with number"
            (\() ->
                Elm.Parser.TestUtil.parse "T1" Elm.Parser.Tokens.typeName
                    |> Expect.equal (Just "T1")
            )
        , Test.test "operatorToken 11 -- is not an operator"
            (\() ->
                Elm.Parser.TestUtil.parse "a = (--)" Elm.Parser.Declarations.declaration
                    |> Expect.equal Nothing
            )
        , Test.test "operatorToken 14"
            (\() ->
                Elm.Parser.TestUtil.parse "a = (=)" Elm.Parser.Declarations.declaration
                    |> Expect.equal Nothing
            )
        , Test.test "operatorToken 15"
            (\() ->
                Elm.Parser.TestUtil.parse "a = (?)" Elm.Parser.Declarations.declaration
                    |> Expect.equal Nothing
            )
        , Test.test "multiline string"
            (\() ->
                Elm.Parser.TestUtil.parse "\"\"\"Bar foo \n a\"\"\"" (Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange (\_ s -> s))
                    |> Expect.equal (Just "Bar foo \n a")
            )
        , Test.test "multiline string escape"
            (\() ->
                Elm.Parser.TestUtil.parse """\"\"\" \\\"\"\" \"\"\"""" (Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange (\_ s -> s))
                    |> Expect.equal (Just """ \"\"\" """)
            )
        , Test.test "character escaped"
            (\() ->
                Elm.Parser.TestUtil.parse "'\\''" (Elm.Parser.Tokens.characterLiteralMapWithRange (\_ c -> c))
                    |> Expect.equal (Just '\'')
            )
        , Test.test "character escaped - 2"
            (\() ->
                Elm.Parser.TestUtil.parse "'\\r'" (Elm.Parser.Tokens.characterLiteralMapWithRange (\_ c -> c))
                    |> Expect.equal (Just '\u{000D}')
            )
        , Test.test "unicode char"
            (\() ->
                Elm.Parser.TestUtil.parse "'\\u{000D}'" (Elm.Parser.Tokens.characterLiteralMapWithRange (\_ c -> c))
                    |> Expect.equal (Just '\u{000D}')
            )
        , Test.test "unicode char with lowercase hex"
            (\() ->
                Elm.Parser.TestUtil.parse "'\\u{000d}'" (Elm.Parser.Tokens.characterLiteralMapWithRange (\_ c -> c))
                    |> Expect.equal (Just '\u{000D}')
            )
        , Test.test "string escaped 3"
            (\() ->
                Elm.Parser.TestUtil.parse "\"\\\"\"" (Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange (\_ s -> s))
                    |> Expect.equal (Just "\"")
            )
        , Test.test "string escaped"
            (\() ->
                Elm.Parser.TestUtil.parse "\"foo\\\\\"" (Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange (\_ s -> s))
                    |> Expect.equal (Just "foo\\")
            )
        , Test.test "character escaped 3"
            (\() ->
                Elm.Parser.TestUtil.parse "'\\n'" (Elm.Parser.Tokens.characterLiteralMapWithRange (\_ c -> c))
                    |> Expect.equal (Just '\n')
            )
        , Test.test "long string"
            (\() ->
                Elm.Parser.TestUtil.parse longString (Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange (\_ s -> s))
                    |> Expect.notEqual Nothing
            )
        , Test.test "long multi line string"
            (\() ->
                Elm.Parser.TestUtil.parse longMultiLineString (Elm.Parser.Tokens.singleOrTripleQuotedStringLiteralMapWithRange (\_ s -> s))
                    |> Expect.notEqual Nothing
            )
        , Test.test "ρ function"
            (\() ->
                Elm.Parser.TestUtil.parse "ρ" Elm.Parser.Tokens.functionName
                    |> Expect.notEqual Nothing
            )
        , Test.test "ε2 function"
            (\() ->
                Elm.Parser.TestUtil.parse "ε2" Elm.Parser.Tokens.functionName
                    |> Expect.notEqual Nothing
            )
        , Test.test "εε function"
            (\() ->
                Elm.Parser.TestUtil.parse "εε" Elm.Parser.Tokens.functionName
                    |> Expect.notEqual Nothing
            )
        , Test.test "ρ uppercase function"
            (\() ->
                Elm.Parser.TestUtil.parse (String.toUpper "ρ") Elm.Parser.Tokens.functionName
                    |> Expect.equal Nothing
            )
        , Test.test "ε uppercase function"
            (\() ->
                Elm.Parser.TestUtil.parse (String.toUpper "ε") Elm.Parser.Tokens.functionName
                    |> Expect.equal Nothing
            )
        , Test.test "ρ type name"
            (\() ->
                Elm.Parser.TestUtil.parse "ρ" Elm.Parser.Tokens.typeName
                    |> Expect.equal Nothing
            )
        , Test.test "ε2 type name"
            (\() ->
                Elm.Parser.TestUtil.parse "ε2" Elm.Parser.Tokens.typeName
                    |> Expect.equal Nothing
            )
        , Test.test "εε type name"
            (\() ->
                Elm.Parser.TestUtil.parse "εε" Elm.Parser.Tokens.typeName
                    |> Expect.equal Nothing
            )
        , Test.test "ρ uppercase type name"
            (\() ->
                Elm.Parser.TestUtil.parse (String.toUpper "ρ") Elm.Parser.Tokens.typeName
                    |> Expect.notEqual Nothing
            )
        , Test.test "ε uppercase type name"
            (\() ->
                Elm.Parser.TestUtil.parse (String.toUpper "ε") Elm.Parser.Tokens.typeName
                    |> Expect.notEqual Nothing
            )
        ]
