module ElmSyntaxPrint exposing
    ( toString, Print
    , module_
    , moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
    , moduleName, qualifiedReference
    )

{-| Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

@docs toString, Print
@docs module_

That's all you'll need most of the time.

Sometimes it's useful to print only some part of the syntax,
to, say, display only an expression in an article
or reformat only the touched declarations on save.

@docs moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
@docs moduleName, qualifiedReference

If you need other syntax printing like for collapsible comments to be exposed,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import ElmSyntaxPrintDefunctionalized
import Print


{-| Pretty printable intermediate representation.
See [`toString`](#toString)
-}
type alias Print =
    { indent : Int } -> String


{-| All other helpers in this module produce a [`Print`](#Print)
which you can in the end convert to a String with [`toString`](#toString)
-}
toString : Print -> String
toString print =
    print { indent = 0 }


{-| Print an [`Elm.Syntax.File.File`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-File#File)
-}
module_ : Elm.Syntax.File.File -> Print
module_ syntaxModule state =
    ElmSyntaxPrintDefunctionalized.module_ syntaxModule
        |> Print.toStringWithIndent state.indent


{-| Print the stuff after `exposing` in a module header.
For import exposing: [`importExposing`](#importExposing)
-}
moduleExposing :
    { atDocsLines : List (List String), comments : List (Elm.Syntax.Node.Node String) }
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
moduleExposing context moduleExposingNode state =
    ElmSyntaxPrintDefunctionalized.moduleExposing context moduleExposingNode
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Module.Module`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader :
    { atDocsLines : List (List String), comments : List (Elm.Syntax.Node.Node String) }
    -> Elm.Syntax.Module.Module
    -> Print
moduleHeader context syntaxModuleHeader state =
    ElmSyntaxPrintDefunctionalized.moduleHeader context syntaxModuleHeader
        |> Print.toStringWithIndent state.indent


{-| Print a set of [`Elm.Syntax.Import.Import`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import#Import)s
-}
imports :
    List (Elm.Syntax.Node.Node String)
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> Print
imports syntaxComments syntaxImports state =
    ElmSyntaxPrintDefunctionalized.imports syntaxComments syntaxImports
        |> Print.toStringWithIndent state.indent


{-| Print a single [`Elm.Syntax.Import.Import`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import#Import)
-}
import_ :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Import.Import
    -> Print
import_ syntaxComments importNode state =
    ElmSyntaxPrintDefunctionalized.import_ syntaxComments importNode
        |> Print.toStringWithIndent state.indent


{-| Print the stuff after `exposing` in an import.
For module header exposing: [`moduleExposing`](#moduleExposing)
-}
importExposing :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
    -> Print
importExposing syntaxComments importExposingNode state =
    ElmSyntaxPrintDefunctionalized.importExposing syntaxComments importExposingNode
        |> Print.toStringWithIndent state.indent


{-| Print `--` or `{- -}` comments placed _within a declaration_.
For top-level comments: [`moduleLevelComments`](#moduleLevelComments)
-}
comments : List String -> Print
comments syntaxComments state =
    ElmSyntaxPrintDefunctionalized.comments syntaxComments
        |> Print.toStringWithIndent state.indent


{-| Print `--` or `{- -}` comments placed outside of declarations at the top level.
For comments within a declaration: [`comments`](#comments)
-}
moduleLevelComments : List String -> Print
moduleLevelComments syntaxComments state =
    ElmSyntaxPrintDefunctionalized.moduleLevelComments syntaxComments
        |> Print.toStringWithIndent state.indent


{-| Print a single `--` or `{- -}` comment.
-}
comment : String -> Print
comment syntaxComment state =
    ElmSyntaxPrintDefunctionalized.comment syntaxComment
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.ModuleName.ModuleName`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
-}
moduleName : Elm.Syntax.ModuleName.ModuleName -> Print
moduleName syntaxModuleName _ =
    ElmSyntaxPrintDefunctionalized.moduleName syntaxModuleName


{-| Print a single [`Elm.Syntax.Exposing.TopLevelExpose`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#TopLevelExpose)
-}
expose : Elm.Syntax.Exposing.TopLevelExpose -> Print
expose syntaxExpose _ =
    ElmSyntaxPrintDefunctionalized.expose syntaxExpose


{-| Print an [`Elm.Syntax.Pattern.Pattern`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Print
patternNotParenthesized syntaxComments patternNode state =
    ElmSyntaxPrintDefunctionalized.patternNotParenthesized syntaxComments patternNode
        |> Print.toStringWithIndent state.indent


{-| Print a name with its qualification (`[]` for no qualification)
-}
qualifiedReference : { qualification : List String, unqualified : String } -> Print
qualifiedReference syntaxReference _ =
    ElmSyntaxPrintDefunctionalized.qualifiedReference syntaxReference


{-| Print an [`Elm.Syntax.TypeAnnotation.TypeAnnotation`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments typeNode state =
    ElmSyntaxPrintDefunctionalized.typeNotParenthesized syntaxComments typeNode
        |> Print.toStringWithIndent state.indent


{-| Print a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
and comments in between
-}
declarations :
    { portDocumentationComments : List (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    , previousEnd : Elm.Syntax.Range.Location
    }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)
    -> Print
declarations context syntaxDeclarations state =
    ElmSyntaxPrintDefunctionalized.declarations context syntaxDeclarations
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)
-}
declaration :
    { portDocumentationComment : Maybe (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Declaration.Declaration
    -> Print
declaration syntaxComments syntaxDeclaration state =
    ElmSyntaxPrintDefunctionalized.declaration syntaxComments syntaxDeclaration
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Signature.Signature`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Signature#Signature)
as `name : Type`
-}
declarationSignature :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Signature.Signature
    -> Print
declarationSignature syntaxComments signature state =
    ElmSyntaxPrintDefunctionalized.declarationSignature syntaxComments signature
        |> Print.toStringWithIndent state.indent


{-| Print a `port` [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)
-}
declarationPort :
    { documentationComment : Maybe (Elm.Syntax.Node.Node String)
    , comments : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Signature.Signature
    -> Print
declarationPort syntaxComments signature state =
    ElmSyntaxPrintDefunctionalized.declarationPort syntaxComments signature
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.TypeAlias.TypeAlias`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration state =
    ElmSyntaxPrintDefunctionalized.declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Type.Type`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Type#Type) declaration
-}
declarationChoiceType :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Type.Type
    -> Print
declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration state =
    ElmSyntaxPrintDefunctionalized.declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Infix.Infix`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Infix#Infix) declaration
-}
declarationInfix : Elm.Syntax.Infix.Infix -> Print
declarationInfix syntaxInfixDeclaration state =
    ElmSyntaxPrintDefunctionalized.declarationInfix syntaxInfixDeclaration
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Expression.Function`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.Function
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration state =
    ElmSyntaxPrintDefunctionalized.declarationExpression syntaxComments syntaxExpressionDeclaration
        |> Print.toStringWithIndent state.indent


{-| Print an [`Elm.Syntax.Expression.Expression`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Expression)
-}
expressionNotParenthesized :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Print
expressionNotParenthesized syntaxComments expressionNode state =
    ElmSyntaxPrintDefunctionalized.expressionNotParenthesized syntaxComments expressionNode
        |> Print.toStringWithIndent state.indent


{-| Print a single [`Elm.Syntax.Expression.Case`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Case)
-}
case_ :
    List (Elm.Syntax.Node.Node String)
    -> Elm.Syntax.Expression.Case
    -> Print
case_ syntaxComments syntaxCase state =
    ElmSyntaxPrintDefunctionalized.case_ syntaxComments syntaxCase
        |> Print.toStringWithIndent state.indent
