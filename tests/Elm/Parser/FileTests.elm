module Elm.Parser.FileTests exposing (all)

import Elm.Parser
import Elm.Parser.Samples
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.TypeAnnotation
import ElmSyntaxParserLenient
import Expect
import ParserFast
import Test


all : Test.Test
all =
    Test.concat
        [ Test.describe "FileTests"
            (List.map
                (\( n, s ) ->
                    Test.test ("sample " ++ String.fromInt n)
                        (\() ->
                            case ParserFast.run ElmSyntaxParserLenient.module_ s of
                                Nothing ->
                                    Expect.fail "failed to parse"

                                Just _ ->
                                    Expect.pass
                        )
                )
                Elm.Parser.Samples.allSamples
            )

        -- , describe "Error messages" <|
        --     [ test "failure on module name" <|
        --         \() ->
        --             Parser.parse "module foo exposing (..)\nx = 1"
        --                 |> Result.toMaybe
        --                 |> Expect.equal Nothing
        --     , test "failure on declaration" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..)\n\ntype x = \n  1"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,0)" ])
        --     , test "failure on declaration expression" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..) \nx = \n  x + _"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,6)" ])
        --     ]
        , Test.test "Comments ordering"
            (\() ->
                let
                    input : String
                    input =
                        """
module Foo exposing (..)

{-| Module documentation
-}

import A

-- 1
{- 2 -}
-- 3

{-| Function declaration
-}
f =
    -- 4
    identity

-- 5
{- 6 -}
"""
                in
                Elm.Parser.parseToFile input
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| Module documentation\n-}"
                            , Elm.Syntax.Node.Node { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } } "-- 1"
                            , Elm.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } } "{- 2 -}"
                            , Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } } "-- 3"
                            , Elm.Syntax.Node.Node { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } } "-- 4"
                            , Elm.Syntax.Node.Node { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } } "-- 5"
                            , Elm.Syntax.Node.Node { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } } "{- 6 -}"
                            ]
                        )
            )
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
                    |> Elm.Parser.parseToFile
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Elm.Syntax.Node.Node { start = { row = 7, column = 17 }, end = { row = 7, column = 21 } } "-- 1"
                            , Elm.Syntax.Node.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 5 } } "-- 2"
                            ]
                        )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } } "caseWhitespace"
                                                , arguments = [ Elm.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } (Elm.Syntax.Pattern.VarPattern "f") ]
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        (Elm.Syntax.Expression.CaseExpression
                                                            { expression = Elm.Syntax.Node.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } } (Elm.Syntax.Expression.FunctionOrValue [] "f")
                                                            , cases =
                                                                [ ( Elm.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                                                  , Elm.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Elm.Syntax.Expression.Integer 1)
                                                                  )
                                                                , ( Elm.Syntax.Node.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } } (Elm.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                                                  , Elm.Syntax.Node.Node { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } } (Elm.Syntax.Expression.Integer 2)
                                                                  )
                                                                ]
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } } "--some comment" ]
                            }
                        )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } "lambdaWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        (Elm.Syntax.Expression.LambdaExpression
                                                            { args =
                                                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } } (Elm.Syntax.Pattern.VarPattern "a")
                                                                , Elm.Syntax.Node.Node { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } } (Elm.Syntax.Pattern.VarPattern "b")
                                                                ]
                                                            , expression =
                                                                Elm.Syntax.Node.Node { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    (Elm.Syntax.Expression.OperatorApplication "+"
                                                                        Elm.Syntax.Infix.Left
                                                                        (Elm.Syntax.Node.Node { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } } (Elm.Syntax.Expression.FunctionOrValue [] "a"))
                                                                        (Elm.Syntax.Node.Node { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } } (Elm.Syntax.Expression.FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                    (Elm.Syntax.Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } } "letWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Elm.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        (Elm.Syntax.Expression.LetExpression
                                                            { declarations =
                                                                [ Elm.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                    (Elm.Syntax.Expression.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            Elm.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                { name = Elm.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } } "b"
                                                                                , arguments = []
                                                                                , expression = Elm.Syntax.Node.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } } (Elm.Syntax.Expression.Integer 1)
                                                                                }
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = Elm.Syntax.Node.Node { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } } (Elm.Syntax.Expression.FunctionOrValue [] "b")
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } } [ "Dict" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                    (Elm.Syntax.Declaration.CustomTypeDeclaration
                                        { documentation = Just (Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| Config goes here\n-}")
                                        , name = Elm.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ Elm.Syntax.Node.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = []
                            }
                        )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (Elm.Syntax.Module.NormalModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                    (Elm.Syntax.Declaration.CustomTypeDeclaration
                                        { documentation = Nothing
                                        , name = Elm.Syntax.Node.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                { name = Elm.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| actually module doc\n-}" ]
                            }
                        )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Elm.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 30 } }
                                    (Elm.Syntax.Module.PortModule
                                        { moduleName = Elm.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 16 } } [ "Foo" ]
                                        , exposingList =
                                            Elm.Syntax.Node.Node { start = { row = 2, column = 17 }, end = { row = 2, column = 30 } }
                                                (Elm.Syntax.Exposing.All { start = { row = 2, column = 27 }, end = { row = 2, column = 29 } })
                                        }
                                    )
                            , imports =
                                [ Elm.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                    { moduleName = Elm.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } } [ "String" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Elm.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 38 } }
                                    (Elm.Syntax.Declaration.PortDeclaration
                                        { name = Elm.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 18 } } "sendResponse"
                                        , typeAnnotation =
                                            Elm.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 38 } }
                                                (Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Elm.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } } ( [], "String" )) [])
                                                    )
                                                    (Elm.Syntax.Node.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 38 } }
                                                        (Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 34 } } ( [], "Cmd" ))
                                                            [ Elm.Syntax.Node.Node { start = { row = 8, column = 35 }, end = { row = 8, column = 38 } } (Elm.Syntax.TypeAnnotation.GenericType "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , comments = [ Elm.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| foo\n-}" ]
                            }
                        )
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
""" ++ comments)
                    |> Elm.Parser.parseToFile
                    |> Result.map (\ast -> List.length ast.comments)
                    |> Expect.equal (Ok 3000)
            )
        ]
