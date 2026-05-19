module Instrument exposing (instrument)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)


instrument : File -> File
instrument file =
    { file | declarations = List.map instrumentDecl file.declarations }


{-|

     expr
     -->
     let _ = () in expr

-}
wrapExpr : Range -> Node Elm.Syntax.Expression.Expression -> Node Elm.Syntax.Expression.Expression
wrapExpr exprRange exprNode =
    let
        nodify : a -> Node a
        nodify a =
            Node exprRange a
    in
    nodify <|
        Elm.Syntax.Expression.LetExpression
            { declarations =
                [ nodify <|
                    Elm.Syntax.Expression.LetDestructuring
                        (nodify Elm.Syntax.Pattern.AllPattern)
                        (nodify Elm.Syntax.Expression.UnitExpr)
                ]
            , expression = exprNode
            }


instrumentDecl : Node Declaration -> Node Declaration
instrumentDecl declNode =
    let
        declRange : Range
        declRange =
            Elm.Syntax.Node.range declNode
    in
    case Elm.Syntax.Node.value declNode of
        Elm.Syntax.Declaration.FunctionDeclaration fn ->
            instrumentFnDecl fn declRange

        Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
            declNode

        Elm.Syntax.Declaration.CustomTypeDeclaration type_ ->
            declNode

        Elm.Syntax.Declaration.PortDeclaration _ ->
            declNode

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            declNode

        Elm.Syntax.Declaration.Destructuring patternNode exprNode ->
            instrumentDestructuringDecl patternNode exprNode declRange


instrumentFnDecl : Elm.Syntax.Expression.Function -> Range -> Node Declaration
instrumentFnDecl fn declRange =
    let
        fnImpl : Elm.Syntax.Expression.FunctionImplementation
        fnImpl =
            Elm.Syntax.Node.value fn.declaration

        declarationName : String
        declarationName =
            Elm.Syntax.Node.value fnImpl.name

        instrumentedExpr =
            instrumentExprWithCategory fnImpl.expression declarationName

        newFnImpl : Elm.Syntax.Expression.FunctionImplementation
        newFnImpl =
            { fnImpl | expression = instrumentedExpr }

        newFn : Elm.Syntax.Expression.Function
        newFn =
            { fn | declaration = Elm.Syntax.Node.map (always newFnImpl) fn.declaration }
    in
    Node declRange (Elm.Syntax.Declaration.FunctionDeclaration newFn)


instrumentDestructuringDecl : Node Pattern -> Node Expression -> Range -> Node Declaration
instrumentDestructuringDecl patternNode exprNode declRange =
    let
        instrumentedExpr =
            instrumentExprWithCategory exprNode "_destructuring"
    in
    Node declRange (Elm.Syntax.Declaration.Destructuring patternNode instrumentedExpr)


instrumentExpr : Node Elm.Syntax.Expression.Expression -> String -> Node Elm.Syntax.Expression.Expression
instrumentExpr exprNode declarationName =
    instrumentExprRecurse exprNode declarationName


instrumentExprWithCategory : Node Elm.Syntax.Expression.Expression -> String -> Node Elm.Syntax.Expression.Expression
instrumentExprWithCategory exprNode declarationName =
    let
        exprRange : Range
        exprRange =
            Elm.Syntax.Node.range exprNode
    in
    case Elm.Syntax.Node.value exprNode of
        Elm.Syntax.Expression.IfBlock condition thenBranch elseBranch ->
            let
                instCondition =
                    instrumentExpr condition declarationName

                instThen =
                    instrumentExprWithCategory thenBranch declarationName

                instElse =
                    instrumentExprWithCategory elseBranch declarationName
            in
            Node exprRange (Elm.Syntax.Expression.IfBlock instCondition instThen instElse)

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            let
                instExpr =
                    instrumentExprWithCategory caseBlock.expression declarationName

                instrumentedCases =
                    List.map
                        (\( pattern, caseExpr ) ->
                            ( pattern, instrumentExprWithCategory caseExpr declarationName )
                        )
                        caseBlock.cases

                newCaseBlock : Elm.Syntax.Expression.CaseBlock
                newCaseBlock =
                    { expression = instExpr
                    , cases = instrumentedCases
                    }
            in
            Node exprRange (Elm.Syntax.Expression.CaseExpression newCaseBlock)

        _ ->
            let
                instrumentedInnerExpr =
                    instrumentExprRecurse exprNode declarationName

                wrappedExpr =
                    wrapExpr exprRange instrumentedInnerExpr
            in
            wrappedExpr


instrumentExprRecurse :
    Node Elm.Syntax.Expression.Expression
    -> String
    -> Node Elm.Syntax.Expression.Expression
instrumentExprRecurse exprNode declarationName =
    let
        exprRange : Range
        exprRange =
            Elm.Syntax.Node.range exprNode
    in
    case Elm.Syntax.Node.value exprNode of
        Elm.Syntax.Expression.Application exprs ->
            let
                instrumentedExprs =
                    instrumentExprList exprs declarationName
            in
            Node exprRange (Elm.Syntax.Expression.Application instrumentedExprs)

        Elm.Syntax.Expression.OperatorApplication op dir left right ->
            let
                instLeft =
                    if op == "&&" || op == "||" then
                        instrumentExprWithCategory left declarationName

                    else
                        instrumentExpr left declarationName

                instRight =
                    if op == "&&" || op == "||" then
                        instrumentExprWithCategory right declarationName

                    else
                        instrumentExpr right declarationName
            in
            Node exprRange (Elm.Syntax.Expression.OperatorApplication op dir instLeft instRight)

        Elm.Syntax.Expression.IfBlock condition thenBranch elseBranch ->
            let
                instCondition =
                    instrumentExpr condition declarationName

                instThen =
                    instrumentExprWithCategory thenBranch declarationName

                instElse =
                    instrumentExprWithCategory elseBranch declarationName
            in
            Node exprRange (Elm.Syntax.Expression.IfBlock instCondition instThen instElse)

        Elm.Syntax.Expression.Negation inner ->
            let
                instInner =
                    instrumentExpr inner declarationName
            in
            Node exprRange (Elm.Syntax.Expression.Negation instInner)

        Elm.Syntax.Expression.TupledExpression exprs ->
            let
                instrumentedExprs =
                    instrumentExprList exprs declarationName
            in
            Node exprRange (Elm.Syntax.Expression.TupledExpression instrumentedExprs)

        Elm.Syntax.Expression.ParenthesizedExpression inner ->
            let
                instInner =
                    instrumentExpr inner declarationName
            in
            Node exprRange (Elm.Syntax.Expression.ParenthesizedExpression instInner)

        Elm.Syntax.Expression.LetExpression letBlock ->
            let
                instrumentedDecls =
                    List.map
                        (\declNode ->
                            instrumentLetDeclaration declNode declarationName
                        )
                        letBlock.declarations

                instLetExpr =
                    instrumentExpr letBlock.expression declarationName

                newLetBlock : Elm.Syntax.Expression.LetBlock
                newLetBlock =
                    { declarations = instrumentedDecls
                    , expression = instLetExpr
                    }
            in
            Node exprRange (Elm.Syntax.Expression.LetExpression newLetBlock)

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            let
                instExpr =
                    instrumentExprWithCategory caseBlock.expression declarationName

                instrumentedCases =
                    List.map
                        (\( pattern, caseExpr ) ->
                            ( pattern, instrumentExprWithCategory caseExpr declarationName )
                        )
                        caseBlock.cases

                newCaseBlock : Elm.Syntax.Expression.CaseBlock
                newCaseBlock =
                    { expression = instExpr
                    , cases = instrumentedCases
                    }
            in
            Node exprRange (Elm.Syntax.Expression.CaseExpression newCaseBlock)

        Elm.Syntax.Expression.LambdaExpression lambda ->
            let
                instExpr =
                    instrumentExpr lambda.expression declarationName

                bodyRange : Range
                bodyRange =
                    Elm.Syntax.Node.range lambda.expression

                wrappedBody =
                    wrapExpr bodyRange instExpr

                newLambda : Elm.Syntax.Expression.Lambda
                newLambda =
                    { lambda | expression = wrappedBody }
            in
            Node exprRange (Elm.Syntax.Expression.LambdaExpression newLambda)

        Elm.Syntax.Expression.RecordExpr setters ->
            let
                instrumentedSetters =
                    List.map
                        (\setterNode ->
                            let
                                ( fieldNameNode, fieldExprNode ) =
                                    Elm.Syntax.Node.value setterNode

                                instExpr =
                                    instrumentExpr fieldExprNode declarationName

                                newSetter : Elm.Syntax.Expression.RecordSetter
                                newSetter =
                                    ( fieldNameNode, instExpr )
                            in
                            Node (Elm.Syntax.Node.range setterNode) newSetter
                        )
                        setters
            in
            Node exprRange (Elm.Syntax.Expression.RecordExpr instrumentedSetters)

        Elm.Syntax.Expression.ListExpr exprs ->
            let
                instrumentedExprs =
                    instrumentExprList exprs declarationName
            in
            Node exprRange (Elm.Syntax.Expression.ListExpr instrumentedExprs)

        Elm.Syntax.Expression.RecordAccess record field ->
            let
                instRecord =
                    instrumentExpr record declarationName
            in
            Node exprRange (Elm.Syntax.Expression.RecordAccess instRecord field)

        Elm.Syntax.Expression.RecordUpdateExpression name setters ->
            let
                instrumentedSetters =
                    List.map
                        (\setterNode ->
                            let
                                ( fieldNameNode, fieldExprNode ) =
                                    Elm.Syntax.Node.value setterNode

                                instExpr =
                                    instrumentExpr fieldExprNode declarationName

                                newSetter : Elm.Syntax.Expression.RecordSetter
                                newSetter =
                                    ( fieldNameNode, instExpr )
                            in
                            Node (Elm.Syntax.Node.range setterNode) newSetter
                        )
                        setters
            in
            Node exprRange (Elm.Syntax.Expression.RecordUpdateExpression name instrumentedSetters)

        Elm.Syntax.Expression.UnitExpr ->
            exprNode

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            exprNode

        Elm.Syntax.Expression.PrefixOperator _ ->
            exprNode

        Elm.Syntax.Expression.Operator _ ->
            exprNode

        Elm.Syntax.Expression.Integer _ ->
            exprNode

        Elm.Syntax.Expression.Hex _ ->
            exprNode

        Elm.Syntax.Expression.Floatable _ ->
            exprNode

        Elm.Syntax.Expression.Literal _ ->
            exprNode

        Elm.Syntax.Expression.CharLiteral _ ->
            exprNode

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            exprNode

        Elm.Syntax.Expression.GLSLExpression _ ->
            exprNode


instrumentExprList : List (Node Elm.Syntax.Expression.Expression) -> String -> List (Node Elm.Syntax.Expression.Expression)
instrumentExprList exprs declarationName =
    List.map
        (\exprNode_ ->
            instrumentExpr exprNode_ declarationName
        )
        exprs


instrumentLetDeclaration : Node Elm.Syntax.Expression.LetDeclaration -> String -> Node Elm.Syntax.Expression.LetDeclaration
instrumentLetDeclaration declNode declarationName =
    let
        declRange : Range
        declRange =
            Elm.Syntax.Node.range declNode
    in
    case Elm.Syntax.Node.value declNode of
        Elm.Syntax.Expression.LetFunction fn ->
            let
                fnImpl : Elm.Syntax.Expression.FunctionImplementation
                fnImpl =
                    Elm.Syntax.Node.value fn.declaration

                instExpr =
                    if List.isEmpty fnImpl.arguments then
                        instrumentExpr fnImpl.expression declarationName

                    else
                        instrumentExprWithCategory fnImpl.expression declarationName

                newFnImpl : Elm.Syntax.Expression.FunctionImplementation
                newFnImpl =
                    { fnImpl | expression = instExpr }

                newFn : Elm.Syntax.Expression.Function
                newFn =
                    { fn | declaration = Elm.Syntax.Node.map (always newFnImpl) fn.declaration }
            in
            Node declRange (Elm.Syntax.Expression.LetFunction newFn)

        Elm.Syntax.Expression.LetDestructuring pattern destrExpr ->
            let
                instDestrExpr =
                    instrumentExpr destrExpr declarationName
            in
            Node declRange (Elm.Syntax.Expression.LetDestructuring pattern instDestrExpr)
