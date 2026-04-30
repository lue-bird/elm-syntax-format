module ElmFormatParserTest exposing (all)

import ElmFormatParser
import ElmFullModules
import Expect
import Test


all : Test.Test
all =
    Test.concat
        [ Test.describe "FileTests"
            (List.map
                (\( n, s ) ->
                    Test.test ("sample " ++ String.fromInt n)
                        (\() ->
                            case ElmFormatParser.run ElmFormatParser.module_ s of
                                Nothing ->
                                    Expect.fail "failed to parse"

                                Just _ ->
                                    Expect.pass
                        )
                )
                ElmFullModules.allSamples
            )
        , Test.describe "layout"
            [ Test.test "positively indented across multiple linebreaks and comments"
                (\() ->
                    """a =
        --x
        {- foo 
-}

    f 0"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "positively indented, too few spaces"
                (\() ->
                    """a = f
0"""
                        |> ElmFormatParser.run ElmFormatParser.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "top indented across multiple linebreaks and comments"
                (\() ->
                    """a =
    let
        b = 0

        --x
        {- foo 
-}


        c = 0
    in
    b"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "top indented, too many spaces"
                (\() ->
                    """a =
    let
        b = 0
         c = 0
    in
    b"""
                        |> ElmFormatParser.run ElmFormatParser.declaration
                        |> Expect.equal Nothing
                )
            ]
        , Test.describe "module header"
            [ Test.test "formatted moduleDefinition"
                (\() ->
                    "module Foo exposing (Bar)"
                        |> moduleExpectToParse
                )
            , Test.test "port moduleDefinition"
                (\() ->
                    "port module Foo exposing (Bar)"
                        |> moduleExpectToParse
                )
            , Test.test "port moduleDefinition with spacing"
                (\() ->
                    "port module Foo exposing ( Bar )"
                        |> moduleExpectToParse
                )
            , Test.test "effect moduleDefinition"
                (\() ->
                    "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                        |> moduleExpectToParse
                )
            , Test.test "unformatted"
                (\() ->
                    "module \n Foo \n exposing  (..)"
                        |> moduleExpectToParse
                )
            , Test.test "allow module name without indentation"
                (\() ->
                    """module 
Foo 
 exposing  (..)"""
                        |> moduleExpectToParse
                )
            , Test.test "exposing .."
                (\() ->
                    "module Foo exposing (..)"
                        |> moduleExpectToParse
                )
            , Test.test "exposing ..."
                (\() ->
                    "module Foo exposing (...)"
                        |> moduleExpectToParse
                )
            , Test.test "module name with _"
                (\() ->
                    "module I_en_gb exposing (..)"
                        |> moduleExpectToParse
                )
            , Test.test "File with multiple declarations"
                (\() ->
                    """module TestModule exposing (..)
type A = B | C
a = 1
type alias B = A
b : Int
b = 2
"""
                        |> moduleExpectToParse
                )
            , Test.test "should fail to parse two signatures in a row"
                (\() ->
                    """module TestModule exposing (..)
a : Int
b : Int
b = 2
"""
                        |> moduleExpectFailsToParse
                )
            , Test.test "should allow different signature name and implementation name"
                (\() ->
                    """module TestModule exposing (..)
a : Int
b = 2
"""
                        |> moduleExpectToParse
                )
            , Test.test "trailing comments at the end of declarations"
                (\() ->
                    """module A exposing (fun1, fun2)

fun1 n =
  fun2 n
  + fun2 n  -- a

fun2 n =
  fun1 n    -- b
"""
                        |> moduleExpectToParse
                )
            ]
        , Test.test "declarations with comments"
            (\() ->
                """module Foo exposing (b, fn)

fn a =
    case a of
        X ->
            1
                -- 1
                + 2

        Y ->
            1

-- 2

b =
    1

"""
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "function declaration with a case and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

caseWhitespace f = case f   of
  True     -> 
    1   
  False   
     -> 
     
     
         2    

   --some comment

    
    """
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "function declaration with lambda and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

lambdaWhitespace =   \\ a b ->    a    

       + 

    b 


--some comment

    """
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "function declaration with let and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

letWhitespace = let
                  b =   1

 in
 b


--some comment

    """
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "type declaration with documentation after imports"
            (\() ->
                """
module Foo exposing (..)

import Dict

{-| Config goes here
-}
type Configuration
    = Configuration
"""
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "module documentation formatted like a type documentation"
            (\() ->
                """
module Foo exposing (..)

{-| actually module doc
-}
type Configuration
    = Configuration
"""
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "documentation on a port declaration is treated as a normal comment"
            (\() ->
                """
port module Foo exposing (..)

import String

{-| foo
-}
port sendResponse : String -> Cmd msg
"""
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "port module without port gets parsed to normal module"
            (\() ->
                """
port module Foo exposing (..)

sendResponse =
    Cmd.none
"""
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "normal module with port gets parsed to port module"
            (\() ->
                """
module Foo exposing (..)

port sendResponse : String -> Cmd msg
"""
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.test "A file with a large number of comments should not create a stack overflow"
            (\() ->
                let
                    comments : String
                    comments =
                        String.repeat 3000 "-- a\n"
                in
                ("""module Foo exposing (..)
a = 1
"""
                    ++ comments
                )
                    |> expectToParse ElmFormatParser.module_
            )
        , Test.describe "declaration"
            [ Test.test "value/function declaration"
                (\() ->
                    "foo = bar"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function declaration with documentation"
                (\() ->
                    """{-| Foo does bar -}
foo = bar"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function declaration with empty record"
                (\() ->
                    "foo = {}"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function with case in let"
                (\() ->
                    """inc x =
  let
    y =
      case x of
        True -> z
    a = b
  in a"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function declaration with args"
                (\() ->
                    "inc x = x + 1"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function declaration with let"
                (\() ->
                    """foo =
 let
  b = 1
 in
  b"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "documentation comment inside a let is invalid"
                (\() ->
                    expectFailsToParse ElmFormatParser.declaration
                        """foo =
 let
  {-| b is one -}
  b = 1
 in
  b"""
                )
            , Test.test "let destructuring with no spaces around '='"
                (\() ->
                    """foo =
 let
  (b, c)=(1, 2)
 in
  b"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "declaration with record"
                (\() ->
                    """main =
  beginnerProgram { model = 0, view = view, update = update }"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "update function"
                (\() ->
                    """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "port declaration for command"
                (\() ->
                    "port parseResponse : ( String, String ) -> Cmd msg"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "port declaration for subscription"
                (\() ->
                    "port scroll : (Move -> msg) -> Sub msg"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "should fail to parse destructuring declaration at the top-level"
                (\() ->
                    "_ = b"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "simple main declaration"
                (\() ->
                    """main =
  text "Hello, World!\""""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "value/function declaration with signature"
                (\() ->
                    """main : Html msg
main =
  text "Hello, World!\""""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function with -> instead of ="
                (\() ->
                    """main ->
  text "Hello, World!\""""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function starting with multi line comment"
                (\() ->
                    """main =
  {- -} x
"""
                        |> moduleExpectToParse
                )
            , Test.test "function with a lot of symbols"
                (\() ->
                    "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "Some function"
                (\() ->
                    """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "some other function"
                (\() ->
                    """update : Model
update msg model =
    msg"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type alias"
                (\() ->
                    "type alias Foo = { color : String }"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type alias with documentation"
                (\() ->
                    """{-| Foo is colorful -}
type alias Foo = {color: String }"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type alias without spacings around '='"
                (\() ->
                    "type alias Foo={color: String }"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type alias with GenericType "
                (\() ->
                    "type alias Foo a = {some : a }"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type"
                (\() ->
                    "type Color = Blue String | Red | Green"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type with leading | before first variant"
                (\() ->
                    "type Color=| Blue String | Red | Green"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type with extra | between variants"
                (\() ->
                    "type Color = Blue String ||Red | Green"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type with documentation"
                (\() ->
                    """{-| Classic RGB -}
type Color = Blue String | Red | Green"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type with multiple args"
                (\() ->
                    "type D = C a B"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type with multiple args and correct distribution of args"
                (\() ->
                    "type D = C B a"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "type args should not continue on next line"
                (\() ->
                    """type D = C B
a"""
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "type with GenericType"
                (\() ->
                    "type Maybe a = Just a | Nothing"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow type with variant name without indentation"
                (\() ->
                    """type Maybe a = Just a |
Nothing"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "fail if declarations not on module-level"
                (\() ->
                    """a = f
    3
    b = 4"""
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "fail if function declaration argument is `as` without parenthesis"
                (\() ->
                    """a foo as bar = f3"""
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "regression test for disallowing ( +)"
                (\() ->
                    "a = ( +)"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "regression test for disallowing (+ )"
                (\() ->
                    "a = (+ )"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "right infix"
                (\() ->
                    "infix right 7 (</>) = slash"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "left infix"
                (\() ->
                    "infix left  8 (<?>) = questionMark"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "non infix"
                (\() ->
                    "infix non   4 (==) = eq"
                        |> expectToParse ElmFormatParser.declaration
                )
            ]
        , Test.describe "type"
            [ Test.test "unitTypeReference"
                (\() ->
                    "()"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "unitTypeReference with spaces"
                (\() ->
                    "( )"
                        |> expectFailsToParse ElmFormatParser.type_
                )
            , Test.test "tupledTypeReference"
                (\() ->
                    "( (), ())"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "4-tuple type annotation is invalid"
                (\() ->
                    "(Int,String,(),a)"
                        |> expectFailsToParse ElmFormatParser.type_
                )
            , Test.test "tupledTypeReference 2"
                (\() ->
                    "( () )"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "tupledTypeReference 3"
                (\() ->
                    "( () , Maybe m )"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "qualified type reference"
                (\() ->
                    "Foo.Bar"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "typedTypeReference 2"
                (\() ->
                    "Foo () a Bar"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "recordTypeReference empty"
                (\() ->
                    "{}"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "recordTypeReference one field with name-value separator ="
                (\() ->
                    "{color= String }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "type record with prefixed comma"
                (\() ->
                    "{ , a : Int, b : Int }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "type record with extra comma between fields"
                (\() ->
                    "{   a : Int,,b : Int }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "record with generic"
                (\() ->
                    "{ attr | position : Vec2, texture : Vec2 }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "record with generic with extra comma between fields"
                (\() ->
                    "{ attr | position : Vec2,,texture : Vec2 }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "allow generic record with no fields"
                (\() ->
                    "{ attr |}"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "recordTypeReference nested record"
                (\() ->
                    "{color: {r : Int, g :Int, b: Int } }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "record field ranges"
                (\() ->
                    "{ foo : Int, bar : Int, baz : Int }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "recordTypeReference with generic"
                (\() ->
                    "{color: s }"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "function type reference"
                (\() ->
                    "Foo -> Bar"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "function type reference multiple"
                (\() ->
                    "Foo -> Bar -> baz"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "function type reference multiple with consecutive ->"
                (\() ->
                    "Foo->->Bar -> baz"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "function type reference generics"
                (\() ->
                    "(cMsg -> cModel -> a)"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "annotation with parens"
                (\() ->
                    "Msg -> Model -> (Model, Cmd Msg)"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "function as argument"
                (\() ->
                    "( cMsg -> cModel -> a ) -> b"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "type with params"
                (\() ->
                    "(Foo -> Bar)"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "function type reference multiple and parens"
                (\() ->
                    "(Foo -> Bar) -> baz"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "parseTypeWith wrong indent"
                (\() ->
                    "Maybe\na"
                        |> expectFailsToParse ElmFormatParser.type_
                )
            , Test.test "parseTypeWith good indent"
                (\() ->
                    "Maybe\n a"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "issue #5 - no spaces between type and generic with parens"
                (\() ->
                    "List(String)"
                        |> expectToParse ElmFormatParser.type_
                )
            , Test.test "parse type with multiple params"
                (\() ->
                    "Dict String Int"
                        |> expectToParse ElmFormatParser.type_
                )
            ]
        , Test.describe "expression"
            [ Test.test "operatorToken 11 -- is not an operator"
                (\() ->
                    "a = (--)"
                        |> ElmFormatParser.run ElmFormatParser.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 14"
                (\() ->
                    "a = (=)"
                        |> ElmFormatParser.run ElmFormatParser.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 15"
                (\() ->
                    "a = (?)"
                        |> ElmFormatParser.run ElmFormatParser.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "empty"
                (\() ->
                    "a = "
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.describe "number"
                [ Test.test "long hex"
                    (\() ->
                        "0x03FFFFFF"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "hex FF"
                    (\() ->
                        "0xFF"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "hex"
                    (\() ->
                        "0x2A"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "Hex integer literal"
                    (\() ->
                        "0x56"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "Integer literal"
                    (\() ->
                        "101"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "float"
                    (\() ->
                        "2.0"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "integer with negative exponent"
                    (\() ->
                        "2e-2"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "literal e is not a number"
                    (\() ->
                        "e"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "integer with negative exponent (uppercase E)"
                    (\() ->
                        "2E-2"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "integer with positive exponent"
                    (\() ->
                        "2e+2"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "float with negative exponent"
                    (\() ->
                        "2.0e-2"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "float with negative exponent (uppercase E)"
                    (\() ->
                        "2.0E-2"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "float with positive exponent"
                    (\() ->
                        "2.0e+2"
                            |> expectToParse ElmFormatParser.expression
                    )
                ]
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "multiline string"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "multiline string escape"
                (\() ->
                    """\"\"\" \\\"\"\" \"\"\""""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "character escaped"
                (\() ->
                    "'\\''"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "character escaped - 2"
                (\() ->
                    "'\\r'"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "unicode char"
                (\() ->
                    "'\\u{000D}'"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "unicode char with lowercase hex"
                (\() ->
                    "'\\u{000d}'"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "string escaped 3"
                (\() ->
                    "\"\\\"\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "string escaped"
                (\() ->
                    "\"foo\\\\\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "character escaped 3"
                (\() ->
                    "'\\n'"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "long string"
                (\() ->
                    longString
                        |> ElmFormatParser.run ElmFormatParser.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "long multi line string"
                (\() ->
                    longMultiLineString
                        |> ElmFormatParser.run ElmFormatParser.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "character literal"
                (\() ->
                    "'c'"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "tuple expression"
                (\() ->
                    "(1,2)"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "triple expression"
                (\() ->
                    "(1,2,3)"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "tuple expression with spaces"
                (\() ->
                    "( 1  ,  2 )"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "4-tuple expression is invalid"
                (\() ->
                    "a = (1,2,3,4)"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "String literal multiline"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "application expression"
                (\() ->
                    "List.concat []"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Binary operation"
                (\() ->
                    "model + 1"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Nested binary operations (+ and ==)"
                (\() ->
                    "count + 1 == 1"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Nested binary operations (+ and /=)"
                (\() ->
                    "count + 1 /= 1"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Nested binary operations (+ and //)"
                (\() ->
                    "count + 1 // 2"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Nested binary operations (&& and <|)"
                (\() ->
                    "condition && condition <| f"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "application expression 2"
                (\() ->
                    "(\"\", always (List.concat [ [ fileName ], [] ]))"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "expressionNotApplication simple"
                (\() ->
                    "foo"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Function call"
                (\() ->
                    "foo bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "Function call with argument badly indented"
                (\() ->
                    "a = foo\nbar"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "ifBlockExpression"
                (\() ->
                    "if True then foo else bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "if-then-else with of instead of then"
                (\() ->
                    "if True of   foo else bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "nestedIfExpression"
                (\() ->
                    "if True then if False then foo else baz else bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "recordExpression"
                (\() ->
                    "{ model = 0, view = view, update = update }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "recordExpression with comment"
                (\() ->
                    "{ foo = 1 -- bar\n , baz = 2 }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "listExpression"
                (\() ->
                    "[ class \"a\", text \"Foo\"]"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[ class \"a\",,text \"Foo\"]"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "listExpression singleton with comment"
                (\() ->
                    "[ 1 {- Foo-} ]"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "list with extra prefix comma and comment"
                (\() ->
                    "[,1 {- Foo-} ]"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "listExpression empty with comment"
                (\() ->
                    "[{- Foo -}]"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "qualified expression with name colliding with keyword"
                (\() ->
                    "Html.Attributes.type"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record access with field name colliding with keyword"
                (\() ->
                    "foo.exposing"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "multiple record access operations"
                (\() ->
                    "foo.bar.baz"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "multiple record access operations with module name"
                (\() ->
                    "A.B.foo.bar.baz"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record with prefix comma"
                (\() ->
                    "{ , a = 1, b = 2 }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record with extra comma between fields"
                (\() ->
                    "{   a = 1,,b = 2 }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record update"
                (\() ->
                    "{ model | count = 1, loading = True }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record update with extra comma between fields"
                (\() ->
                    "{ model | count = 1,,loading = True }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record update no spacing"
                (\() ->
                    "{model| count = 1, loading = True }"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record access as function"
                (\() ->
                    "List.map .name people"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "record access function call"
                (\() ->
                    "(.spaceEvenly Internal.Style.classes)"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "positive integer should be invalid"
                (\() ->
                    "a = +1"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "expression ending with an operator should not be valid"
                (\() ->
                    "a = 1++"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "multiple < in a row should not be valid"
                (\() ->
                    "z = a < b < c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "multiple > in a row should not be valid"
                (\() ->
                    "z = a > b > c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "multiple == in a row should not be valid"
                (\() ->
                    "z = a == b == c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "multiple /= in a row should not be valid"
                (\() ->
                    "z = a /= b /= c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "multiple >= in a row should not be valid"
                (\() ->
                    "z = a >= b >= c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "multiple <= in a row should not be valid"
                (\() ->
                    "z = a <= b <= c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "mixing comparison operators without parenthesis should not be valid"
                (\() ->
                    "z = a < b == c"
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , -- these are currently allowed but I'm not sure they should be
              -- , test "<| followed by |> operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a <| b |> c"
              --             |> ParserWithCommentsUtil.expectInvalid ElmFormatParser.declaration
              -- , test "|> followed by <| operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a |> b <| c"
              --             |> ParserWithCommentsUtil.expectInvalid ElmFormatParser.declaration
              -- , test "<< followed by >> operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a << b >> c"
              --             |> ParserWithCommentsUtil.expectInvalid ElmFormatParser.declaration
              -- , test ">> followed by << operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a >> b << c"
              --             |> ParserWithCommentsUtil.expectInvalid ElmFormatParser.declaration
              Test.test "operator function with one argument"
                (\() ->
                    "(::) x"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "subtraction without spaces"
                (\() ->
                    "2-1"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression after comma"
                (\() ->
                    "a = (0,-x)"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "negated expression after = in record"
                (\() ->
                    "{a=-x}"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression after list"
                (\() ->
                    "List.sum [a,b]-x"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression after = in let declaration"
                (\() ->
                    "let a=-x in a"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression after -> in case branch"
                (\() ->
                    "case 1 of\n    _->-a"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression after `in` in let body"
                (\() ->
                    "let a=-x in-a"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression for value"
                (\() ->
                    "a = -x"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "function declaration with negation sign after ="
                (\() ->
                    "foo=-1"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "negated expression in application"
                (\() ->
                    "toFloat -5"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negated expression for parenthesized"
                (\() ->
                    "a = -(x - y)"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "negated expression with other operations"
                (\() ->
                    "a = -1 + -10 * -100^2 == -100001"
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "plus and minus in the same expression"
                (\() ->
                    "1 + 2 - 3"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "pipe operation"
                (\() ->
                    "a |> b"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "function with higher order"
                (\() ->
                    "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "application should be lower-priority than field access"
                (\() ->
                    "foo { d | b = f x y }.b"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "should not consider a negative number parameter as the start of a new application"
                (\() ->
                    "Random.list -1 generator"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.test "negation can be applied on record access"
                (\() ->
                    "1 + -{x = 10}.x"
                        |> expectToParse ElmFormatParser.expression
                )
            , Test.describe "lambda"
                [ Test.test "unit lambda"
                    (\() ->
                        "\\() -> foo"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "record lambda"
                    (\() ->
                        "\\{foo} -> foo"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "empty record lambda"
                    (\() ->
                        "\\{} -> foo"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "args lambda"
                    (\() ->
                        "\\a b -> a + b"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "tuple lambda"
                    (\() ->
                        "\\(a,b) -> a + b"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "lambda with => instead of ->"
                    (\() ->
                        "\\() => foo"
                            |> expectToParse ElmFormatParser.expression
                    )
                ]
            , Test.describe "let-in"
                [ Test.test "let expression with multiple declarations"
                    (\() ->
                        """let
  foo = bar

  john n = n in 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "Let with `in` indented more than the body and let declarations"
                    (\() ->
                        """let
  bar = 1
            in
  bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should fail to parse if declaration is indented as much as `let`"
                    (\() ->
                        """  let
  bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "should fail to parse if declarations are not indented the same way"
                    (\() ->
                        """  let
    bar = 1
      foo = 2
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "let with deindented expression in in"
                    (\() ->
                        """let
  bar = 1
 in
   bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "Let function with type annotation"
                    (\() ->
                        """let
    bar : Int
    bar = 1
  in
  bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "Let function with type annotation (separated by a few lines)"
                    (\() ->
                        """let
    bar : Int


    bar = 1
  in
  bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation earlier)"
                    (\() ->
                        """let
    bar : Int
      bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation later)"
                    (\() ->
                        """let
       bar : Int
    bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "should fail to parse `as` pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar n as m = 1
        in
        bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "correctly parse variant + args pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar Bar m = 1
        in
        bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should not parse let destructuring with a type annotation"
                    (\() ->
                        """let
    bar : Int
    (bar) = 1
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "should not parse let destructuring with `as` not surrounded by parentheses"
                    (\() ->
                        """let
    foo as bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "should not parse let destructuring with variant + arguments not surrounded by parentheses"
                    (\() ->
                        """let
    Foo bar = 1
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "allow let destructuring = to be top indented"
                    (\() ->
                        """let
    (bar)
    =     1
  in
  bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "allow let destructuring with top indented expression"
                    (\() ->
                        """let
    (bar) =
    1
  in
  bar"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should not parse let type annotation without a declaration"
                    (\() ->
                        """let
    bar : Int
  in
  bar"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "Using destructuring"
                    (\() ->
                        """let
    _ = b
    {a} = b
    (c, d) = e
    (Node _ f) = g
 in
    1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "On one line"
                    (\() ->
                        "let indent = String.length s in indent"
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "let with list after in without space"
                    (\() ->
                        """let
        a = 1
    in[]"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "let with record after in without space"
                    (\() ->
                        """let
        a = 1
    in{}"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "let with lambda after in without space"
                    (\() ->
                        """let
        a = 1
    in\\_ -> 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "let is not confused by a variable name starting with let"
                    (\() ->
                        "letterbox"
                            |> expectToParse ElmFormatParser.expression
                    )
                ]
            , Test.describe "case-of"
                [ Test.test "with then instead of of"
                    (\() ->
                        """case
True
  then
    A -> 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "allow the matched expression to be top indented"
                    (\() ->
                        """case
True
  of
    A -> 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "allow the `of` keyword to be top indented"
                    (\() ->
                        """case True
of
               A -> 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "disallow case first branch pattern to be top indented"
                    (\() ->
                        """case True of
    True -> 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "allow case branch result to be top indented"
                    (\() ->
                        """case f of
  True ->
1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "case expression"
                    (\() ->
                        """case f of
  True -> 1
  False -> 2"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "case expression with qualified imports"
                    (\() ->
                        """case f of
  Foo.Bar -> 1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "case expression with no space between pattern and value"
                    (\() ->
                        """case f of
  x->1"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should parse case expression with first branch on the same line as case of"
                    (\() ->
                        """case x of True -> 1
          False -> 2"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should parse case expression with a multiline pattern"
                    (\() ->
                        """case x of
        \"\"\"single line triple quote\"\"\" ->
            1
        \"\"\"multi line
            triple quote\"\"\" ->
            2
        _ -> 3"""
                            |> expectToParse ElmFormatParser.expression
                    )
                , Test.test "should fail to parse case expression with second branch indented differently than the first line (before)"
                    (\() ->
                        expectFailsToParse ElmFormatParser.expression
                            """case f of
  True -> 1
 False -> 2"""
                    )
                , Test.test "should fail to parse case expression with second branch indented differently than the first line (after)"
                    (\() ->
                        """case f of
  True -> 1
   False -> 2
"""
                            |> expectFailsToParse ElmFormatParser.expression
                    )
                , Test.test "should parse case expression when "
                    (\() ->
                        """case msg of
  Increment ->
    1

  Decrement ->
    2"""
                            |> expectToParse ElmFormatParser.expression
                    )
                ]
            , Test.test "allow if condition to be top indented"
                (\() ->
                    """a =
    let
        x =
            if
        f y then  1 else 0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow `then` indentation to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True
        then  1 else 0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow if `then` to not be positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """a =
    let
        x =
            if True
        then  1 else 0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow if-true-branch to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then
        1   else 0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow if `else` to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then 1
        else 0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow if `else` to not positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """a =
    let
        x =
            if True then 1
        else 0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow if if-false-branch to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then 1 else
        0
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow record closing curly to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b = 1
        }
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow record field value to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b =
        1 }
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow record field name to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0,
        b       = 1 }
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow record field `=` to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b
        =         1 }
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow tuple closing parens to be top indented"
                (\() ->
                    """a =
    let
        x =
            ( 0, 1
        )
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow first tuple part to be top indented"
                (\() ->
                    """a =
    let
        x =
            (
        0   , 1
            )
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow second tuple part to be top indented"
                (\() ->
                    """a =
    let
        x =
            ( 0,
        1   )
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow infix operator to be top indented"
                (\() ->
                    """a =
    let
        x =
            0
        + 1
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "fail if function call argument is top indented"
                (\() ->
                    """a =
    let
        x =
            f 0
        1
    in
    x"""
                        |> expectFailsToParse ElmFormatParser.declaration
                )
            , Test.test "allow lambda result to be top indented"
                (\() ->
                    """a =
    let
        x =
            \\y ->
        y
    in
    x"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow case branch result call argument to be top indented"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a <|
        \\_ -> ()"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "allow if case branch result call argument to not positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a
  b"""
                        |> expectToParse ElmFormatParser.declaration
                )
            , Test.test "glsl block"
                (\() ->
                    "[glsl| precision mediump float; |]"
                        |> expectToParse ElmFormatParser.expression
                )
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Float should not be valid" (\() -> expectFailsToParse ElmFormatParser.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Uncons with parens"
                (\() ->
                    "(X x) :: xs"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Empty list"
                (\() ->
                    "[]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "list with prefix extra comma"
                (\() ->
                    "[,1]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[1,,2]"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Record pattern with whitespace"
                (\() ->
                    "{a , b}"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Record pattern with extra comma between fields"
                (\() ->
                    "{a,, b}"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Record pattern with prefixed comma"
                (\() ->
                    "{ , a , b }"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Record pattern with trailing whitespace"
                (\() ->
                    "{a }"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Named pattern without and with spacing should parse to the same"
                (\() ->
                    "Bar "
                        |> ElmFormatParser.run ElmFormatParser.pattern
                        |> Expect.equal
                            ("Bar"
                                |> ElmFormatParser.run ElmFormatParser.pattern
                            )
                )
            , Test.test "Qualified named"
                (\() ->
                    "Basics.True"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Qualified named pattern with data"
                (\() ->
                    "Set.Set x"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Tuple"
                (\() ->
                    "(model, cmd)"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "4-tuple pattern is invalid"
                (\() ->
                    "(1,2,3,4)"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "Nested tuple"
                (\() ->
                    "(a,{b,c},())"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "As pattern"
                (\() ->
                    "x as y"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "should fail to parse when right side is not a direct variable name"
                (\() ->
                    "x as (y)"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "should fail to parse consecutive as"
                (\() ->
                    "x as y as z"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "should fail to parse :: after as"
                (\() ->
                    "x as y :: z"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "should fail to parse :: after as even when :: was already used before"
                (\() ->
                    "w :: x as y :: z"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "should fail to parse when right side is an invalid variable name"
                (\() ->
                    "x as _y"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "should fail to parse when right side is not a variable name"
                (\() ->
                    "x as 1"
                        |> expectFailsToParse ElmFormatParser.pattern
                )
            , Test.test "Record as"
                (\() ->
                    "{model,context} as appState"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Complex"
                (\() ->
                    "(Index irec as index, docVector)"
                        |> expectToParse ElmFormatParser.pattern
                )
            , Test.test "Complex pattern 2"
                (\() ->
                    "RBNode_elm_builtin col (RBNode_elm_builtin Red  (RBNode_elm_builtin Red xv))"
                        |> expectToParse ElmFormatParser.pattern
                )
            ]
        , Test.describe "misc comments and operators"
            [ Test.test "function with documentation comment, without signature"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The docs
-}
bar = 1
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "function with documentation and signature"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The docs
-}
bar : Int
bar = 1
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "function with single line comment before"
                (\() ->
                    """
module Bar exposing (..)

--The Doc
bar = 1
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "file with multiple comments"
                (\() ->
                    """
-- comment 1
module Bar exposing (..)

-- comment 2
bar = {- comment 3 -} 1 -- comment 4
 -- comment 5
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "function with multi-line comment before"
                (\() ->
                    """
module Bar exposing (..)

{- The Doc -}
bar = 1
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "type alias with documentation"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The Doc -}
type alias Foo
  = { name : String }
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "choice type with documentation comment"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The Doc -}
type Foo
   = Red
   | Blue
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "max call stack size failure"
                (\() ->
                    """module Simplify.AstHelpers exposing (log)


log : Int -> Int
log a =
    Debug.log "ok" a
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "parenthesized infix operations"
                (\() ->
                    """
module Bar exposing (..)

bar = (x + 1) * (2 * y)
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "infix operators consecutive with different associativity loose then tight"
                (\() ->
                    """
module Bar exposing (..)

bar = x + 1 * 2
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "infix operators consecutive with different associativity tight then loose"
                (\() ->
                    """
module Bar exposing (..)

bar = x * 1 + 2
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "negated infix operation"
                (\() ->
                    """
module Bar exposing (..)

bar = -(1 * 2)
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "infix operation into record access"
                (\() ->
                    """
module Bar exposing (..)

bar = (1 * 2).x
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "infix operators regression https://github.com/stil4m/elm-syntax/issues/41"
                (\() ->
                    """
module A exposing (..)

bool1 = True && True || True
bool2 = True || True && True

numeric1 = 1 ^ 2 * 3 + 4
numeric2 = 1 + 2 * 3 ^ 4
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "infix operators associativity https://github.com/stil4m/elm-syntax/issues/87"
                (\() ->
                    """
module A exposing (..)

numeric1 = 1 + 2 - 3

pipeline1 = 1 |> 2 |> 3
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "| is equivalent to |>"
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 | 2 | 3

pipeline1 = 1 |> 2 |> 3
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "!= is equivalent to /="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 != 2

pipeline1 = 1 /= 2
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "!== is equivalent to /="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 !== 2

pipeline1 = 1 /= 2
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "=== is equivalent to =="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 === 2

pipeline1 = 1 == 2
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            , Test.test "** is equivalent to ^"
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 ** 2

pipeline1 = 1 ^ 2
"""
                        |> String.trim
                        |> expectToParse ElmFormatParser.module_
                )
            ]
        , -- https://github.com/stil4m/elm-syntax/issues/273
          Test.test "single double quote, not closed before end of file"
            (\() ->
                """
module A exposing (..)
a = "
"""
                    |> ElmFormatParser.run ElmFormatParser.module_
                    |> Expect.equal Nothing
            )
        , Test.test "triple double quote, not closed before end of file"
            (\() ->
                """
module A exposing (..)
a = \"\"\"
"""
                    |> ElmFormatParser.run ElmFormatParser.module_
                    |> Expect.equal Nothing
            )
        , Test.test "single quote, not closed before end of file"
            (\() ->
                """
module A exposing (..)
a = '
"""
                    |> ElmFormatParser.run ElmFormatParser.module_
                    |> Expect.equal Nothing
            )
        , Test.test "[glsl|, not closed before end of file"
            (\() ->
                """
module A exposing (..)
a = [glsl|
"""
                    |> ElmFormatParser.run ElmFormatParser.module_
                    |> Expect.equal Nothing
            )
        ]


expectFailsToParse :
    ElmFormatParser.Parser a_
    -> String
    -> Expect.Expectation
expectFailsToParse parser source =
    case source |> ElmFormatParser.run parser of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)


expectToParse : ElmFormatParser.Parser a_ -> String -> Expect.Expectation
expectToParse parse source =
    case ElmFormatParser.run parse source of
        Nothing ->
            Expect.fail "Expected the source to be parsed"

        Just _ ->
            Expect.pass


moduleExpectFailsToParse : String -> Expect.Expectation
moduleExpectFailsToParse source =
    case ElmFormatParser.run ElmFormatParser.module_ source of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)


moduleExpectToParse : String -> Expect.Expectation
moduleExpectToParse source =
    case ElmFormatParser.run ElmFormatParser.module_ source of
        Nothing ->
            Expect.fail "Expected the source to be parsed"

        Just _ ->
            Expect.pass


longString : String
longString =
    "\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\""


longMultiLineString : String
longMultiLineString =
    "\"\"\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\"\"\""
