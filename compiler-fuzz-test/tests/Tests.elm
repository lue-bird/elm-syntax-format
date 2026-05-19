module Tests exposing (suite)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (DefaultModuleData, Module(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Location, Range)
import ElmMake
import ElmSyntaxPrint
import Expect
import Fuzz exposing (Fuzzer)
import Instrument
import ListExtra
import Set exposing (Set)
import Test exposing (Test)
import Test.Distribution


toString : File -> String
toString file =
    file
        |> ElmSyntaxPrint.module_
        |> ElmSyntaxPrint.toString


suite : Test
suite =
    Test.fuzzWith
        { runs = 100000
        , distribution =
            Test.expectDistribution
                [ ( Test.Distribution.atLeast 80, "original compiles", ElmMake.compiles << toString )
                ]
        }
        fileFuzzer
        "if a File compiles when stringified, it compiles when instrumented and stringified"
    <|
        \file ->
            if ElmMake.compiles (toString file) then
                file
                    |> Instrument.instrument
                    |> toString
                    |> ElmMake.compiles
                    |> Expect.equal True
                    |> Expect.onFail "Compiled on its own but doesn't compile when instrumented"

            else
                -- We don't care about this input - it didn't compile on its own, so it likely won't compile when instrumented.
                Expect.pass


fileFuzzer : Fuzzer File
fileFuzzer =
    declarationFuzzer
        |> Fuzz.map
            (\declaration ->
                { moduleDefinition =
                    -- module FuzzedModule exposing (..)
                    toNode
                        (NormalModule
                            { moduleName = toNode [ "FuzzedModule" ]
                            , exposingList = toNode (All Elm.Syntax.Range.empty)
                            }
                        )
                , imports = []
                , declarations = [ declaration ]
                , comments = []
                }
            )


declarationFuzzer : Fuzzer (Node Declaration)
declarationFuzzer =
    functionFuzzer
        |> Fuzz.map (\fn -> toNode (FunctionDeclaration fn))


topLevelDeclarationName : String
topLevelDeclarationName =
    "x"


functionFuzzer : Fuzzer Function
functionFuzzer =
    let
        initialUsedNames =
            Set.singleton topLevelDeclarationName
    in
    expressionFuzzer initialUsedNames 15
        |> Fuzz.map
            (\expr ->
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    toNode
                        { name = toNode topLevelDeclarationName
                        , arguments = []
                        , expression = expr
                        }
                }
            )


expressionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
expressionFuzzer usedNames depth =
    if depth <= 0 then
        literalExpressionFuzzer

    else
        Fuzz.frequency
            [ ( 1, literalExpressionFuzzer )
            , ( 3, simpleOperationFuzzer usedNames depth )
            , ( 1, unitExpressionFuzzer )
            , ( 2, listExpressionFuzzer usedNames depth )
            , ( 3, tupleExpressionFuzzer usedNames depth )
            , ( 5, ifExpressionFuzzer usedNames depth )
            , ( 6, caseExpressionFuzzer usedNames depth )
            ]


type ExprType
    = -- TODO: functions? *gulp*
      TInt
    | TFloat
    | TString
    | TChar
    | TBool
    | TUnit


exprTypeFuzzer : Fuzzer ExprType
exprTypeFuzzer =
    Fuzz.oneOfValues
        [ TInt
        , TFloat
        , TString
        , TChar
        , TBool
        , TUnit
        ]


exprTypeFuzzerWithoutFloat : Fuzzer ExprType
exprTypeFuzzerWithoutFloat =
    Fuzz.oneOfValues
        [ TInt
        , TString
        , TChar
        , TBool
        , TUnit
        ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


rawIntExprFuzzer : Fuzzer (Node Expression)
rawIntExprFuzzer =
    Fuzz.intRange -100 100 |> Fuzz.map (\i -> toNode (Integer i))


rawFloatExprFuzzer : Fuzzer (Node Expression)
rawFloatExprFuzzer =
    Fuzz.floatRange -100.0 100.0 |> Fuzz.map (\f -> toNode (Floatable f))


rawStringExprFuzzer : Fuzzer (Node Expression)
rawStringExprFuzzer =
    Fuzz.string |> Fuzz.map (String.slice 0 20) |> Fuzz.map (\s -> toNode (Literal s))


rawCharExprFuzzer : Fuzzer (Node Expression)
rawCharExprFuzzer =
    charFuzzer |> Fuzz.map (\c -> toNode (CharLiteral c))


rawBoolExprFuzzer : Fuzzer (Node Expression)
rawBoolExprFuzzer =
    Fuzz.bool |> Fuzz.map (\b -> toNode (FunctionOrValue [] (boolToString b)))


expressionFuzzerOfType : Set String -> Int -> ExprType -> Fuzzer (Node Expression)
expressionFuzzerOfType usedNames depth exprType =
    if depth <= 0 then
        case exprType of
            TInt ->
                rawIntExprFuzzer
                    |> withOptionalNoOps

            TFloat ->
                rawFloatExprFuzzer
                    |> withOptionalNoOps

            TString ->
                rawStringExprFuzzer
                    |> withOptionalNoOps

            TChar ->
                rawCharExprFuzzer
                    |> withOptionalNoOps

            TBool ->
                rawBoolExprFuzzer
                    |> withOptionalNoOps

            TUnit ->
                Fuzz.constant (toNode UnitExpr)

    else
        withOptionalNoOps <|
            case exprType of
                TInt ->
                    rawIntExprFuzzer

                TFloat ->
                    rawFloatExprFuzzer

                TString ->
                    rawStringExprFuzzer

                TChar ->
                    rawCharExprFuzzer

                TBool ->
                    Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames depth)

                TUnit ->
                    Fuzz.constant (toNode UnitExpr)


withOptionalNoOps : Fuzzer (Node Expression) -> Fuzzer (Node Expression)
withOptionalNoOps baseFuzzer =
    baseFuzzer
        |> Fuzz.andThen
            (\expr ->
                Fuzz.frequency
                    [ ( 7, Fuzz.constant expr )
                    , ( 3, noOpWrap expr ) -- recurse!
                    ]
            )


noOpWrap : Node Expression -> Fuzzer (Node Expression)
noOpWrap expr =
    Fuzz.oneOfValues
        [ -- expr |> identity
          toNode
            (OperatorApplication "|>"
                Right
                (parenthesize expr)
                (toNode (FunctionOrValue [] "identity"))
            )
        , -- identity <| expr
          toNode
            (OperatorApplication "<|"
                Left
                (toNode (FunctionOrValue [] "identity"))
                (parenthesize expr)
            )
        , -- identity expr
          toNode
            (Application
                [ toNode (FunctionOrValue [] "identity")
                , expr
                ]
            )
        , -- (expr)
          parenthesize expr
        ]
        |> Fuzz.andThen
            (\wrappedExpr ->
                Fuzz.frequency
                    [ ( 7, Fuzz.constant wrappedExpr )
                    , ( 3, noOpWrap wrappedExpr ) -- recurse!
                    ]
            )


literalExpressionFuzzer : Fuzzer (Node Expression)
literalExpressionFuzzer =
    withOptionalNoOps
        (Fuzz.oneOf
            [ rawIntExprFuzzer
            , rawFloatExprFuzzer
            , rawStringExprFuzzer
            , rawCharExprFuzzer
            , rawBoolExprFuzzer
            ]
        )


charRange : Char -> Char -> Fuzzer Char
charRange start end =
    Fuzz.intRange (Char.toCode start) (Char.toCode end)
        |> Fuzz.map Char.fromCode


charFuzzer : Fuzzer Char
charFuzzer =
    Fuzz.oneOf
        [ charRange 'a' 'z'
        , charRange 'A' 'Z'
        , charRange '0' '9'
        ]


removeNewlines : String -> String
removeNewlines str =
    str
        |> String.toList
        |> List.filter (\c -> c /= '\n' && c /= '\u{000D}')
        |> String.fromList


stringForPatternFuzzer : Fuzzer String
stringForPatternFuzzer =
    Fuzz.string |> Fuzz.map (String.slice 0 20 >> removeNewlines)


simpleOperationFuzzer : Set String -> Int -> Fuzzer (Node Expression)
simpleOperationFuzzer usedNames depth =
    if depth <= 0 then
        withOptionalNoOps literalExpressionFuzzer

    else
        withOptionalNoOps
            (operatorFuzzer
                |> Fuzz.andThen
                    (\op ->
                        case op of
                            OrderingOp orderingOp ->
                                orderingOperationFuzzer usedNames depth orderingOp

                            EqualityOp equalityOp ->
                                equalityOperationFuzzer usedNames depth equalityOp

                            ArithmeticOp arithmeticOp ->
                                arithmeticOperationFuzzer usedNames depth arithmeticOp

                            BooleanOp booleanOp ->
                                booleanOperationFuzzer usedNames depth booleanOp
                    )
            )


orderingOperationFuzzer : Set String -> Int -> OrderingOperator -> Fuzzer (Node Expression)
orderingOperationFuzzer usedNames depth op =
    if depth <= 0 then
        numberExpressionFuzzer

    else
        numberExpressionFuzzer
            |> Fuzz.andThen
                (\left ->
                    Fuzz.map2
                        (\dir right ->
                            toNode
                                (OperatorApplication (operatorToString (OrderingOp op))
                                    dir
                                    (parenthesize left)
                                    (parenthesize right)
                                )
                        )
                        infixDirectionFuzzer
                        (Fuzz.lazy (\_ -> numberExpressionFuzzer))
                )


equalityOperationFuzzer : Set String -> Int -> EqualityOperator -> Fuzzer (Node Expression)
equalityOperationFuzzer usedNames depth op =
    if depth <= 0 then
        literalExpressionFuzzer

    else
        exprTypeFuzzer
            |> Fuzz.andThen
                (\exprType ->
                    Fuzz.map3
                        (\dir left right ->
                            toNode
                                (OperatorApplication (operatorToString (EqualityOp op))
                                    dir
                                    (parenthesize left)
                                    (parenthesize right)
                                )
                        )
                        infixDirectionFuzzer
                        (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                        (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                )


arithmeticOperationFuzzer : Set String -> Int -> ArithmeticOperator -> Fuzzer (Node Expression)
arithmeticOperationFuzzer usedNames depth op =
    if depth <= 0 then
        if op == IntDivide then
            rawIntExprFuzzer

        else
            numberExpressionFuzzer

    else
        let
            operandFuzzer =
                if op == IntDivide then
                    rawIntExprFuzzer

                else
                    numberExpressionFuzzer
        in
        Fuzz.map3
            (\dir left right ->
                toNode
                    (OperatorApplication (operatorToString (ArithmeticOp op))
                        dir
                        (parenthesize left)
                        (parenthesize right)
                    )
            )
            infixDirectionFuzzer
            (Fuzz.lazy (\_ -> operandFuzzer))
            (Fuzz.lazy (\_ -> operandFuzzer))


booleanOperationFuzzer : Set String -> Int -> BooleanOperator -> Fuzzer (Node Expression)
booleanOperationFuzzer usedNames depth op =
    if depth <= 0 then
        rawBoolExprFuzzer

    else
        Fuzz.map3
            (\dir left right ->
                toNode
                    (OperatorApplication (operatorToString (BooleanOp op))
                        dir
                        (parenthesize left)
                        (parenthesize right)
                    )
            )
            infixDirectionFuzzer
            (Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames (depth - 1)))
            (Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames (depth - 1)))


numberExpressionFuzzer : Fuzzer (Node Expression)
numberExpressionFuzzer =
    Fuzz.oneOf
        [ rawIntExprFuzzer
        , rawFloatExprFuzzer
        ]


infixDirectionFuzzer : Fuzzer InfixDirection
infixDirectionFuzzer =
    Fuzz.oneOfValues
        [ Left
        , Right
        , Non
        ]


type Operator
    = OrderingOp OrderingOperator
    | EqualityOp EqualityOperator
    | ArithmeticOp ArithmeticOperator
    | BooleanOp BooleanOperator


type OrderingOperator
    = LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual


type EqualityOperator
    = Equal
    | NotEqual


type ArithmeticOperator
    = Plus
    | Minus
    | Multiply
    | IntDivide


type BooleanOperator
    = Or
    | And


operatorToString : Operator -> String
operatorToString op =
    case op of
        OrderingOp LessThan ->
            "<"

        OrderingOp LessThanOrEqual ->
            "<="

        OrderingOp GreaterThan ->
            ">"

        OrderingOp GreaterThanOrEqual ->
            ">="

        EqualityOp Equal ->
            "=="

        EqualityOp NotEqual ->
            "/="

        ArithmeticOp Plus ->
            "+"

        ArithmeticOp Minus ->
            "-"

        ArithmeticOp Multiply ->
            "*"

        ArithmeticOp IntDivide ->
            "//"

        BooleanOp Or ->
            "||"

        BooleanOp And ->
            "&&"


operatorFuzzer : Fuzzer Operator
operatorFuzzer =
    Fuzz.oneOf
        [ orderingOperatorFuzzer |> Fuzz.map OrderingOp
        , equalityOperatorFuzzer |> Fuzz.map EqualityOp
        , arithmeticOperatorFuzzer |> Fuzz.map ArithmeticOp
        , booleanOperatorFuzzer |> Fuzz.map BooleanOp
        ]


orderingOperatorFuzzer : Fuzzer OrderingOperator
orderingOperatorFuzzer =
    Fuzz.oneOfValues
        [ LessThan
        , LessThanOrEqual
        , GreaterThan
        , GreaterThanOrEqual
        ]


equalityOperatorFuzzer : Fuzzer EqualityOperator
equalityOperatorFuzzer =
    Fuzz.oneOfValues
        [ Equal
        , NotEqual
        ]


arithmeticOperatorFuzzer : Fuzzer ArithmeticOperator
arithmeticOperatorFuzzer =
    Fuzz.oneOfValues
        [ Plus
        , Minus
        , Multiply
        , IntDivide
        ]


booleanOperatorFuzzer : Fuzzer BooleanOperator
booleanOperatorFuzzer =
    Fuzz.oneOfValues
        [ Or
        , And
        ]


unitExpressionFuzzer : Fuzzer (Node Expression)
unitExpressionFuzzer =
    withOptionalNoOps (Fuzz.constant (toNode UnitExpr))


listExpressionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
listExpressionFuzzer usedNames depth =
    if depth <= 0 then
        withOptionalNoOps literalExpressionFuzzer

    else
        withOptionalNoOps
            (exprTypeFuzzer
                |> Fuzz.andThen
                    (\exprType ->
                        Fuzz.list (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                            |> Fuzz.map (\exprs -> toNode (ListExpr exprs))
                    )
            )


tupleExpressionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
tupleExpressionFuzzer usedNames depth =
    if depth <= 0 then
        withOptionalNoOps literalExpressionFuzzer

    else
        withOptionalNoOps
            (Fuzz.map2
                (\left right ->
                    toNode (TupledExpression [ left, right ])
                )
                (Fuzz.lazy (\_ -> expressionFuzzer usedNames (depth - 1)))
                (Fuzz.lazy (\_ -> expressionFuzzer usedNames (depth - 1)))
            )


boolExpressionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
boolExpressionFuzzer usedNames depth =
    if depth <= 0 then
        -- Base case: only literal booleans
        rawBoolExprFuzzer

    else
        Fuzz.frequency
            [ -- Literal booleans (low weight)
              ( 1
              , rawBoolExprFuzzer
              )
            , -- Boolean operations: || and && (higher weight for nesting)
              ( 3
              , Fuzz.map2
                    (\left right ->
                        toNode (OperatorApplication "||" Left (parenthesize left) (parenthesize right))
                    )
                    (Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames (depth - 1)))
                    (Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames (depth - 1)))
              )
            , ( 3
              , Fuzz.map2
                    (\left right ->
                        toNode (OperatorApplication "&&" Left (parenthesize left) (parenthesize right))
                    )
                    (Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames (depth - 1)))
                    (Fuzz.lazy (\_ -> boolExpressionFuzzer usedNames (depth - 1)))
              )
            , -- Comparisons: == and /= for same-type (non-function) expressions (medium weight)
              ( 2
              , Fuzz.andThen
                    (\exprType ->
                        Fuzz.map2
                            (\left right ->
                                toNode (OperatorApplication "==" Non (parenthesize left) (parenthesize right))
                            )
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                    )
                    exprTypeFuzzer
              )
            , ( 2
              , Fuzz.andThen
                    (\exprType ->
                        Fuzz.map2
                            (\left right ->
                                toNode (OperatorApplication "/=" Non (parenthesize left) (parenthesize right))
                            )
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                    )
                    exprTypeFuzzer
              )
            ]


conditionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
conditionFuzzer usedNames depth =
    boolExpressionFuzzer usedNames depth


ifExpressionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
ifExpressionFuzzer usedNames depth =
    if depth <= 0 then
        withOptionalNoOps literalExpressionFuzzer

    else
        withOptionalNoOps
            (exprTypeFuzzer
                |> Fuzz.andThen
                    (\exprType ->
                        Fuzz.map3
                            (\condition thenBranch elseBranch ->
                                toNode (IfBlock condition thenBranch elseBranch)
                            )
                            (Fuzz.lazy (\_ -> conditionFuzzer usedNames (depth - 1)))
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                    )
            )


patternVarNameFuzzer : Set String -> Fuzzer String
patternVarNameFuzzer usedNames =
    let
        availableNames =
            [ "a", "b", "c", "y", "z" ]
                |> List.filter (\name -> not (Set.member name usedNames))
    in
    if List.isEmpty availableNames then
        -- Fallback: generate a unique name by appending numbers
        Fuzz.intRange 0 9999 |> Fuzz.map (\n -> "var" ++ String.fromInt n)

    else
        Fuzz.oneOfValues availableNames


intPatternFuzzer : Fuzzer (Node Pattern)
intPatternFuzzer =
    Fuzz.intRange 0 100 |> Fuzz.map (\i -> toNode (IntPattern i))


stringPatternFuzzer : Fuzzer (Node Pattern)
stringPatternFuzzer =
    stringForPatternFuzzer |> Fuzz.map (\s -> toNode (StringPattern s))


charPatternFuzzer : Fuzzer (Node Pattern)
charPatternFuzzer =
    charFuzzer |> Fuzz.map (\c -> toNode (CharPattern c))


varPatternFuzzer : Set String -> Fuzzer (Node Pattern)
varPatternFuzzer usedNames =
    patternVarNameFuzzer usedNames |> Fuzz.map (\name -> toNode (VarPattern name))


patternFuzzer : Set String -> Fuzzer (Node Pattern)
patternFuzzer usedNames =
    Fuzz.oneOf
        [ varPatternFuzzer usedNames
        , intPatternFuzzer -- don't use negative ints as patterns

        -- don't use floats as patterns
        , stringPatternFuzzer
        , charPatternFuzzer
        , Fuzz.bool |> Fuzz.map (\b -> toNode (VarPattern (boolToString b)))
        , Fuzz.constant (toNode UnitPattern)
        ]


nonVarPatternOfTypeFuzzer : ExprType -> Fuzzer (Node Pattern)
nonVarPatternOfTypeFuzzer subjectType =
    case subjectType of
        TInt ->
            intPatternFuzzer

        TFloat ->
            -- Floats should not be used as case expression subjects
            Fuzz.invalid "Floats should not be used as case expression subjects"

        TString ->
            stringPatternFuzzer

        TChar ->
            charPatternFuzzer

        TBool ->
            Fuzz.oneOfValues
                [ toNode (VarPattern "True")
                , toNode (VarPattern "False")
                ]

        TUnit ->
            Fuzz.constant (toNode UnitPattern)


patternFuzzerOfType : Set String -> ExprType -> Fuzzer (Node Pattern)
patternFuzzerOfType usedNames subjectType =
    case subjectType of
        TInt ->
            Fuzz.oneOf
                [ intPatternFuzzer
                , varPatternFuzzer usedNames
                ]

        TFloat ->
            varPatternFuzzer usedNames

        TString ->
            Fuzz.oneOf
                [ stringPatternFuzzer
                , varPatternFuzzer usedNames
                ]

        TChar ->
            Fuzz.oneOf
                [ charPatternFuzzer
                , varPatternFuzzer usedNames
                ]

        TBool ->
            Fuzz.oneOf
                [ Fuzz.constant (toNode (VarPattern "True"))
                , Fuzz.constant (toNode (VarPattern "False"))
                , varPatternFuzzer usedNames
                ]

        TUnit ->
            Fuzz.constant (toNode UnitPattern)


isVarPattern : Pattern -> Bool
isVarPattern pattern =
    case pattern of
        VarPattern _ ->
            True

        _ ->
            False


caseExpressionFuzzer : Set String -> Int -> Fuzzer (Node Expression)
caseExpressionFuzzer usedNames depth =
    if depth <= 0 then
        withOptionalNoOps literalExpressionFuzzer

    else
        withOptionalNoOps
            (Fuzz.map2
                (\subjectType exprType ->
                    if subjectType == TUnit then
                        -- Unit type only has one value (), so we must have exactly one case with UnitPattern
                        Fuzz.map2
                            (\caseExpr expr ->
                                Fuzz.constant
                                    (toNode
                                        (CaseExpression
                                            { expression = caseExpr
                                            , cases = [ ( toNode UnitPattern, expr ) ]
                                            }
                                        )
                                    )
                            )
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) subjectType))
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))

                    else if subjectType == TBool then
                        -- For boolean subjects, use Fuzz.oneOf to choose one of the four valid patterns:
                        -- 1. Just a var pattern
                        -- 2. True pattern and var pattern
                        -- 3. False pattern and var pattern
                        -- 4. True pattern and False pattern
                        Fuzz.map2
                            (\caseExpr cases ->
                                Fuzz.constant <|
                                    toNode
                                        (CaseExpression
                                            { expression = caseExpr
                                            , cases = cases
                                            }
                                        )
                            )
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) subjectType))
                            (Fuzz.oneOf
                                [ -- 1. Just a var pattern
                                  patternVarNameFuzzer usedNames
                                    |> Fuzz.andThen
                                        (\varName ->
                                            expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType
                                                |> Fuzz.map
                                                    (\expr ->
                                                        [ ( toNode (VarPattern varName), expr ) ]
                                                    )
                                        )
                                , -- 2. True pattern and var pattern
                                  Fuzz.map2
                                    (\trueExpr varCase ->
                                        [ ( toNode (VarPattern "True"), trueExpr ), varCase ]
                                    )
                                    (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                                    (patternVarNameFuzzer usedNames
                                        |> Fuzz.andThen
                                            (\varName ->
                                                expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType
                                                    |> Fuzz.map
                                                        (\expr ->
                                                            ( toNode (VarPattern varName), expr )
                                                        )
                                            )
                                    )
                                , -- 3. False pattern and var pattern
                                  Fuzz.map2
                                    (\falseExpr varCase ->
                                        [ ( toNode (VarPattern "False"), falseExpr ), varCase ]
                                    )
                                    (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                                    (patternVarNameFuzzer usedNames
                                        |> Fuzz.andThen
                                            (\varName ->
                                                expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType
                                                    |> Fuzz.map
                                                        (\expr ->
                                                            ( toNode (VarPattern varName), expr )
                                                        )
                                            )
                                    )
                                , -- 4. True pattern and False pattern
                                  Fuzz.map2
                                    (\trueExpr falseExpr ->
                                        [ ( toNode (VarPattern "True"), trueExpr )
                                        , ( toNode (VarPattern "False"), falseExpr )
                                        ]
                                    )
                                    (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                                    (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                                ]
                            )

                    else
                        Fuzz.map2
                            (\caseExpr nonVarCases ->
                                Fuzz.andThen
                                    (\maybeVarCase ->
                                        let
                                            allCases =
                                                case maybeVarCase of
                                                    Just varCase ->
                                                        nonVarCases ++ [ varCase ]

                                                    Nothing ->
                                                        nonVarCases

                                            deduplicatedCases =
                                                allCases
                                                    |> ListExtra.uniqueBy (\( pattern, _ ) -> Elm.Syntax.Node.value pattern)
                                        in
                                        if List.isEmpty deduplicatedCases then
                                            patternVarNameFuzzer usedNames
                                                |> Fuzz.andThen
                                                    (\varName ->
                                                        expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType
                                                            |> Fuzz.map
                                                                (\expr ->
                                                                    toNode
                                                                        (CaseExpression
                                                                            { expression = caseExpr
                                                                            , cases = [ ( toNode (VarPattern varName), expr ) ]
                                                                            }
                                                                        )
                                                                )
                                                    )

                                        else
                                            Fuzz.constant
                                                (toNode
                                                    (CaseExpression
                                                        { expression = caseExpr
                                                        , cases = deduplicatedCases
                                                        }
                                                    )
                                                )
                                    )
                                    (if subjectType == TInt || subjectType == TFloat || subjectType == TString || subjectType == TChar then
                                        -- These types as subjects must have a var pattern at the end (they're infinite)
                                        Fuzz.map Just
                                            (patternVarNameFuzzer usedNames
                                                |> Fuzz.andThen
                                                    (\varName ->
                                                        Fuzz.map
                                                            (\expr ->
                                                                ( toNode (VarPattern varName)
                                                                , expr
                                                                )
                                                            )
                                                            (expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType)
                                                    )
                                            )

                                     else
                                     -- For other types, ensure we have at least one pattern
                                     if
                                        List.isEmpty nonVarCases
                                     then
                                        -- If no non-var cases, we must generate at least one pattern (var pattern)
                                        Fuzz.map Just
                                            (patternVarNameFuzzer usedNames
                                                |> Fuzz.andThen
                                                    (\varName ->
                                                        Fuzz.map
                                                            (\expr ->
                                                                ( toNode (VarPattern varName)
                                                                , expr
                                                                )
                                                            )
                                                            (expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType)
                                                    )
                                            )

                                     else
                                        -- Optionally add a var pattern to the non-empty list of patterns.
                                        Fuzz.maybe
                                            (patternVarNameFuzzer usedNames
                                                |> Fuzz.andThen
                                                    (\varName ->
                                                        Fuzz.map
                                                            (\expr ->
                                                                ( toNode (VarPattern varName)
                                                                , expr
                                                                )
                                                            )
                                                            (expressionFuzzerOfType (Set.insert varName usedNames) (depth - 1) exprType)
                                                    )
                                            )
                                    )
                            )
                            (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) subjectType))
                            (Fuzz.list
                                (Fuzz.map2
                                    (\pattern expr ->
                                        ( pattern, expr )
                                    )
                                    (Fuzz.lazy (\_ -> nonVarPatternOfTypeFuzzer subjectType))
                                    (Fuzz.lazy (\_ -> expressionFuzzerOfType usedNames (depth - 1) exprType))
                                )
                            )
                )
                exprTypeFuzzerWithoutFloat
                exprTypeFuzzer
                |> Fuzz.andThen identity
                |> Fuzz.andThen identity
            )


toNode : a -> Node a
toNode value =
    Node Elm.Syntax.Range.empty value


parenthesize : Node Expression -> Node Expression
parenthesize expr =
    toNode (ParenthesizedExpression expr)
