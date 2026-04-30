module ElmFormatSyntax exposing
    ( Comment, CommentStyle(..)
    , Exposing, Exposed(..), Expose(..)
    , Module, ModuleHeader(..), EffectEntry, EffectsEntries(..), Import
    , Declaration, UndocumentedDeclaration(..), VariableDeclarationInfo, InfixDirection(..)
    , Type(..), TypeRecordStartAfterFirstName(..)
    , StringQuotingStyle(..), Pattern(..), Expression(..), ExpressionRecordStartAfterFirstName(..), LetDeclaration(..)
    , CommaSeparated, TailingCommaSeparated, StartAndCommentsAfter, WithStartAndCommentsAfter
    )

{-| Like the `elm-syntax` tree but containing all missing the information necessary for formatting
like `elm-format`. As a treat, it also embeds comments directly after

Note that all sub-syntax elements will include potential comments after.

@docs Comment, CommentStyle
@docs Exposing, Exposed, Expose
@docs Module, ModuleHeader, EffectEntry, EffectsEntries, Import
@docs Declaration, UndocumentedDeclaration, VariableDeclarationInfo, InfixDirection
@docs Type, TypeRecordStartAfterFirstName
@docs StringQuotingStyle, Pattern, Expression, ExpressionRecordStartAfterFirstName, LetDeclaration

@docs CommaSeparated, TailingCommaSeparated, StartAndCommentsAfter, WithStartAndCommentsAfter

-}

import Elm.Syntax.Range


{-| For example

    -- for example




    {- For example

       {- For example


       -}

    -}

See [`CommentStyle`](#CommentStyle)

-}
type alias Comment =
    { start : Elm.Syntax.Range.Location
    , style : CommentStyle
    , content : String
    , end : Elm.Syntax.Range.Location
    }


{-| block `{- ... -}` or line `-- ...`
-}
type CommentStyle
    = CommentLine
    | CommentBlock


{-| as it says on the package. Used for keywords, symbols and regular word symbols
-}
type alias StartAndCommentsAfter =
    { start : Elm.Syntax.Range.Location
    , commentsAfter : List Comment
    }


{-| as it says on the package
-}
type alias WithStartAndCommentsAfter value =
    { start : Elm.Syntax.Range.Location
    , value : value
    , commentsAfter : List Comment
    }


{-| member exported within an [`exposing (here)`](#Exposing)
-}
type Expose
    = ExposeVariable (WithStartAndCommentsAfter String)
    | ExposeType
        { name : WithStartAndCommentsAfter String
        , exposingAllVariants :
            Maybe
                { openParen : StartAndCommentsAfter
                , dotDot : StartAndCommentsAfter
                , closedParen : StartAndCommentsAfter
                }
        }
    | ExposeOperator
        -- whitespace between parens and symbol do not compile.
        -- start is for the open paren, symbol String does not include ()
        (WithStartAndCommentsAfter String)


{-| For example:

    import Html.Attributes as HA exposing (style)

-}
type alias Import =
    { importKeyword : StartAndCommentsAfter
    , name : WithStartAndCommentsAfter String
    , as_ :
        Maybe
            { asKeyword : StartAndCommentsAfter
            , name : WithStartAndCommentsAfter String
            }
    , exposing_ : Maybe Exposing
    }


{-| For example:

    exposing (some, Type(..))

-}
type alias Exposing =
    { exposingKeyword : StartAndCommentsAfter
    , openParen : StartAndCommentsAfter
    , exposed : Exposed
    , closedParen : StartAndCommentsAfter
    }


{-| For example:

    ..

    some, Type(..)

used in [`Exposing`](#Exposing)

-}
type Exposed
    = ExposedAll
        -- ..
        StartAndCommentsAfter
    | ExposedExplicit (CommaSeparated Expose)


{-| `,`-separated syntax elements. Think list, fields, exposes.
Be aware: comments before are not included and must be tracked separately.
Also, the element must contain potential comments after
-}
type alias CommaSeparated element =
    Maybe
        { head : element
        , tail : TailingCommaSeparated element
        }


{-| [`CommaSeparated`](#CommaSeparated) after a known first element.
-}
type alias TailingCommaSeparated element =
    List
        { comma : StartAndCommentsAfter
        , element : element
        }


{-| `"""string"""` or `"string"`
-}
type StringQuotingStyle
    = StringSingleDoubleQuoted
    | StringTripleDoubleQuoted


{-| module header. For example:

    port module Main

    module Html.Attributes

Required at the top of every [`Module`](#Module).
Not including the module [exposing](#Exposing)
and the potential module documentation comment.

-}
type ModuleHeader
    = ModuleHeaderPure
        { moduleKeyword : StartAndCommentsAfter
        , moduleName : WithStartAndCommentsAfter String
        }
    | ModuleHeaderPort
        { portKeyword : StartAndCommentsAfter
        , moduleKeyword : StartAndCommentsAfter
        , moduleName : WithStartAndCommentsAfter String
        }
    | ModuleHeaderEffect
        { effectKeyword : StartAndCommentsAfter
        , moduleKeyword : StartAndCommentsAfter
        , moduleName : WithStartAndCommentsAfter String
        , whereKeyword : StartAndCommentsAfter
        , openCurly : StartAndCommentsAfter
        , effectEntries : EffectsEntries
        , closedCurly : StartAndCommentsAfter
        }


{-| A list of named entries like `{ subscription = MySub }` after `where` in an `effect module` [header](#ModuleHeader)
-}
type EffectsEntries
    = OnlySubscription EffectEntry
    | CommandAnd
        { command : EffectEntry
        , subscription :
            Maybe
                { comma : StartAndCommentsAfter
                , entry : EffectEntry
                }
        }


{-| A field like `subscription = MySub` in [`EffectsEntries`](#EffectsEntries) after `where` in an `effect module` [header](#ModuleHeader)
-}
type alias EffectEntry =
    { keyword : StartAndCommentsAfter
    , equals : StartAndCommentsAfter
    , name : WithStartAndCommentsAfter String
    }


{-| A full file, see [`ModuleHeader`](#ModuleHeader), [`Exposing`](#Exposing), [`Import`](#Import), [`Declaration`](#Declaration)
-}
type alias Module =
    { commentsBeforeHeader : List Comment
    , header : Maybe ModuleHeader
    , exposing_ : Maybe Exposing
    , moduleDocumentation : Maybe (WithStartAndCommentsAfter String)
    , imports : List Import
    , declarations : List Declaration
    }


{-| Module-level declaration. For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }

see [`UndocumentedDeclaration`](#UndocumentedDeclaration)

-}
type alias Declaration =
    { documentation : Maybe (WithStartAndCommentsAfter String)
    , declaration : UndocumentedDeclaration
    }


{-| Module-level declaration. Can be one of the following:

  - Function/value declaration: `add x y = x + y`
  - Custom type declaration: `type Color = Blue | Red`
  - Type alias declaration: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declaration. You will probably not need this, while only core packages can define these.

-}
type UndocumentedDeclaration
    = DeclarationVariable VariableDeclarationInfo
    | DeclarationTypeAlias
        { typeKeyword : StartAndCommentsAfter
        , aliasKeyword : StartAndCommentsAfter
        , name : WithStartAndCommentsAfter String
        , parameters : List (WithStartAndCommentsAfter String)
        , equals : StartAndCommentsAfter
        , type_ : Type
        }
    | DeclarationChoiceType
        { typeKeyword : StartAndCommentsAfter
        , name : WithStartAndCommentsAfter String
        , parameters : List (WithStartAndCommentsAfter String)
        , equals : StartAndCommentsAfter
        , headVariantName : WithStartAndCommentsAfter String
        , headVariantValues : List Type
        , tailVariants :
            List
                { bar : StartAndCommentsAfter
                , name : WithStartAndCommentsAfter String
                , values : List Type
                }
        }
    | DeclarationPort
        { portKeyword : StartAndCommentsAfter
        , name : WithStartAndCommentsAfter String
        , colon : StartAndCommentsAfter
        , type_ : Type
        }
    | DeclarationInfix
        { infixKeyword : StartAndCommentsAfter
        , direction : WithStartAndCommentsAfter InfixDirection
        , precedence : WithStartAndCommentsAfter String
        , operator :
            -- whitespace between parens and symbol do not compile.
            -- start is for the open paren, symbol string does not include ()
            WithStartAndCommentsAfter String
        , equals : StartAndCommentsAfter
        , variable : WithStartAndCommentsAfter String
        }


{-| Infix operator associativity
-}
type InfixDirection
    = InfixDirectionLeft
    | InfixDirectionRight
    | InfixDirectionNon


{-| value/full function declaration.
Note that signature name and implementation name can be different
(which is not valid elm but may be interpreted as a signature and an unrelated variable declaration without a signature)
-}
type alias VariableDeclarationInfo =
    { name : WithStartAndCommentsAfter String
    , signature :
        Maybe
            { colon : StartAndCommentsAfter
            , type_ : Type
            , implementationName : WithStartAndCommentsAfter String
            }
    , parameters : List Pattern
    , equals : StartAndCommentsAfter
    , expression : Expression
    }


{-| A value or function:

  - `ExpressionUnit`: `()`
  - `ExpressionInteger`: `-42`
  - `ExpressionHex`: `0x1F`
  - `ExpressionFloat`: `42.0`
  - `ExpressionChar`: `'a'`
  - `ExpressionString`: `"text"`
  - `ExpressionOperatorFunction`: `(+)`
  - `ExpressionNegation`: `-a`
  - `ExpressionParenthesized`: `(a)`
  - `ExpressionCall`: `add a b`
  - `ExpressionInfixOperation`: `a + b`
  - `ExpressionReference`: `add` or `Basics.True` or `portCmd`
  - `ExpressionIfThenElse`: `if a then b else c`
  - `ExpressionLetIn`: `let a = 4 in a`
  - `ExpressionCaseOf`: `case a of` followed by pattern matches
  - `ExpressionLambda`: `(\a -> a)`
  - `ExpressionRecord`: `{ name = "text" }` or `{ Some.record | name = "text" }`
  - `ExpressionArray`: `[ x, y ]`
  - `ExpressionRecordAccess`: `a.name`
  - `ExpressionRecordAccessFunction`: `.name`

-}
type Expression
    = ExpressionUnit
        -- compiler does not allow whitespace between ()
        StartAndCommentsAfter
    | ExpressionHex
        (WithStartAndCommentsAfter
            -- without 0x
            String
        )
    | -- Int or Float
      ExpressionNumber (WithStartAndCommentsAfter String)
    | ExpressionChar
        { startSingleQuoteStart : Elm.Syntax.Range.Location
        , content : Char
        , endSingleQuote : StartAndCommentsAfter
        }
    | ExpressionString
        { quotingStyle : StringQuotingStyle
        , startQuoteStart : Elm.Syntax.Range.Location
        , content : String
        , endQuote : StartAndCommentsAfter
        }
    | ExpressionOperatorFunction
        -- whitespace between parens and symbol does not compile
        { openParenStart : Elm.Syntax.Range.Location
        , operator : String
        , closedParen : StartAndCommentsAfter
        }
    | ExpressionNegation
        -- whitespace between - and expression does not compile
        { minusStart : Elm.Syntax.Range.Location
        , inner : Expression
        }
    | ExpressionParenthesized
        { openParen : StartAndCommentsAfter
        , inner : Expression
        , closedParenStart : Elm.Syntax.Range.Location
        , fieldAccesses :
            List
                (-- name not including the .
                 String
                )
        , commentsAfter : List Comment
        }
    | ExpressionTuple
        { openParen : StartAndCommentsAfter
        , part0 : Expression
        , comma0 : StartAndCommentsAfter
        , part1 : Expression
        , closedParen : StartAndCommentsAfter
        }
    | ExpressionTriple
        { openParen : StartAndCommentsAfter
        , part0 : Expression
        , comma0 : StartAndCommentsAfter
        , part1 : Expression
        , comma1 : StartAndCommentsAfter
        , part2 : Expression
        , closedParen : StartAndCommentsAfter
        }
    | ExpressionCall
        { called : Expression
        , argument0 : Expression
        , argument1Up : List Expression
        }
    | ExpressionInfixOperation
        { left : Expression
        , operator : WithStartAndCommentsAfter String
        , right : Expression
        }
    | ExpressionReference
        -- can be a variant, record type alias constructor or variable.
        -- Can even be a variable that is .recordAccessed multiple times.
        -- This is fine as the formatter doesn't need to reformat this
        -- and validating is not its job.
        -- includes potential qualification
        (WithStartAndCommentsAfter String)
    | ExpressionIfThenElse
        { ifKeyword : StartAndCommentsAfter
        , condition : Expression
        , thenKeyword : StartAndCommentsAfter
        , onTrue : Expression
        , elseKeyword : StartAndCommentsAfter
        , onFalse : Expression
        }
    | ExpressionLetIn
        { letKeyword : StartAndCommentsAfter
        , -- the compiler is more strict and requires >= 1
          declarations : List LetDeclaration
        , inKeyword : StartAndCommentsAfter
        , result : Expression
        }
    | ExpressionCaseOf
        { caseKeyword : StartAndCommentsAfter
        , matched : Expression
        , ofKeyword : StartAndCommentsAfter
        , -- the compiler is more strict and requires >= 1
          cases :
            List
                { pattern : Pattern
                , arrow : StartAndCommentsAfter
                , result : Expression
                }
        }
    | ExpressionLambda
        { backslash : StartAndCommentsAfter
        , parameters : List Pattern
        , arrow : StartAndCommentsAfter
        , result : Expression
        }
    | ExpressionRecord
        { openCurly : StartAndCommentsAfter
        , inner :
            Maybe
                { firstName : WithStartAndCommentsAfter String
                , inner : ExpressionRecordStartAfterFirstName
                }
        , closedCurly : StartAndCommentsAfter
        }
    | ExpressionList
        { openSquare : StartAndCommentsAfter
        , elements : CommaSeparated Expression
        , closedSquare : StartAndCommentsAfter
        }
    | ExpressionRecordAccessFunction
        (WithStartAndCommentsAfter
            -- name not including the .
            String
        )
    | ExpressionRecordUpdate
        { openCurly : StartAndCommentsAfter
        , record : Expression
        , bar : StartAndCommentsAfter
        , fields :
            CommaSeparated
                { name : WithStartAndCommentsAfter String
                , equals : StartAndCommentsAfter
                , value : Expression
                }
        , closedCurly : StartAndCommentsAfter
        , fieldAccesses :
            List
                (WithStartAndCommentsAfter
                    -- name not including the .
                    String
                )
        }
    | ExpressionGlsl
        { startGlsl : StartAndCommentsAfter
        , content : String
        , endGlsl : StartAndCommentsAfter
        }


{-| Whether the expression record is updating a variable or continuing with the first field value
-}
type ExpressionRecordStartAfterFirstName
    = ExpressionRecordStartUpdate
        { bar : StartAndCommentsAfter
        , fields :
            CommaSeparated
                { name : WithStartAndCommentsAfter String
                , equals : StartAndCommentsAfter
                , value : Expression
                }
        }
    | ExpressionRecordStartField
        { firstEquals : StartAndCommentsAfter
        , firstValue : Expression
        , tailingFields :
            TailingCommaSeparated
                { name : WithStartAndCommentsAfter String
                , equals : StartAndCommentsAfter
                , value : Expression
                }
        }


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetVariable VariableDeclarationInfo
    | LetDestructuring
        { pattern : Pattern
        , equals : StartAndCommentsAfter
        , expression : Expression
        }


{-| Custom type for different type annotations. For example:

  - `TypeVariable`: `a`
  - `TypeConstruct`: `Maybe (Int -> String)`
  - `TypeUnit`: `()`
  - `TypeParenthesized`: `(a -> b)`
  - `TypeRecord`: `{}` or `{ name : String }` or `{ a | name : String }`
  - `TypeFunction`: `Int -> String`

-}
type Type
    = TypeVariable (WithStartAndCommentsAfter String)
    | TypeUnit
        -- compiler does not allow whitespace between ()
        StartAndCommentsAfter
    | TypeConstruct
        { -- includes potential qualification
          name : WithStartAndCommentsAfter String
        , arguments : List Type
        }
    | TypeParenthesized
        { openParen : StartAndCommentsAfter
        , inner : Type
        , closedParen : StartAndCommentsAfter
        }
    | TypeTuple
        { openParen : StartAndCommentsAfter
        , part0 : Type
        , comma0 : StartAndCommentsAfter
        , part1 : Type
        , closedParen : StartAndCommentsAfter
        }
    | TypeTriple
        { openParen : StartAndCommentsAfter
        , part0 : Type
        , comma0 : StartAndCommentsAfter
        , part1 : Type
        , comma1 : StartAndCommentsAfter
        , part2 : Type
        , closedParen : StartAndCommentsAfter
        }
    | TypeRecord
        { openCurly : StartAndCommentsAfter
        , inner :
            Maybe
                { firstName : WithStartAndCommentsAfter String
                , inner : TypeRecordStartAfterFirstName
                }
        , closedCurly : StartAndCommentsAfter
        }
    | TypeFunction
        { parameter0 : Type
        , arrowPart0 :
            { arrow : StartAndCommentsAfter
            , part : Type
            }
        , arrowParts1Up :
            List
                { arrow : StartAndCommentsAfter
                , part : Type
                }
        }


{-| Whether the type record is extending a type variable or continuing with the first field value
-}
type TypeRecordStartAfterFirstName
    = TypeRecordStartExtension
        { bar : StartAndCommentsAfter
        , fields :
            CommaSeparated
                { name : WithStartAndCommentsAfter String
                , colon : StartAndCommentsAfter
                , value : Type
                }
        }
    | TypeRecordStartField
        { firstColon : StartAndCommentsAfter
        , firstValue : Type
        , tailingFields :
            TailingCommaSeparated
                { name : WithStartAndCommentsAfter String
                , colon : StartAndCommentsAfter
                , value : Type
                }
        }


{-| Custom type for all patterns such as:

  - `PatternIgnored`: `_` or `_name`
  - `PatternUnit`: `()`
  - `PatternChar`: `'c'`
  - `PatternString`: `"hello"`
  - `PatternInt`: `42`
  - `PatternHex`: `0x11`
  - `PatternTuple`: `(a, b)`
  - `PatternRecord`: `{name, age}`
  - `PatternListCons`: `x :: xs`
  - `PatternListExact`: `[ x, y ]`
  - `PatternVariable`: `x`
  - `PatternVariant`: `Just _`
  - `PatternAs`: `_ as x`
  - `PatternParenthesized`: `( _ )`

-}
type Pattern
    = PatternIgnored StartAndCommentsAfter
    | PatternVariable (WithStartAndCommentsAfter String)
    | PatternUnit
        -- compiler allows whitespace and comments between ()
        -- (unlike in type and expression)
        { openParen : StartAndCommentsAfter
        , closedParen : StartAndCommentsAfter
        }
    | PatternChar
        { startSingleQuoteStart : Elm.Syntax.Range.Location
        , content : Char
        , endSingleQuote : StartAndCommentsAfter
        }
    | PatternString
        { quotingStyle : StringQuotingStyle
        , startQuoteStart : Elm.Syntax.Range.Location
        , content : String
        , endQuote : StartAndCommentsAfter
        }
    | PatternInt (WithStartAndCommentsAfter String)
    | PatternHex
        (WithStartAndCommentsAfter
            -- without 0x
            String
        )
    | PatternRecord
        { openCurly : StartAndCommentsAfter
        , fields :
            CommaSeparated
                (WithStartAndCommentsAfter String)
        , closedCurly : StartAndCommentsAfter
        }
    | PatternListCons
        { head : Pattern
        , cons0 :
            { cons : StartAndCommentsAfter
            , part : Pattern
            }
        , cons1Up :
            List
                { cons : StartAndCommentsAfter
                , part : Pattern
                }
        }
    | PatternListExact
        { openSquare : StartAndCommentsAfter
        , elements : CommaSeparated Pattern
        , closedSquare : StartAndCommentsAfter
        }
    | PatternVariant
        { -- includes potential qualification
          name : WithStartAndCommentsAfter String
        , values : List Pattern
        }
    | PatternAs
        { pattern : Pattern
        , asKeyword : StartAndCommentsAfter
        , variable : WithStartAndCommentsAfter String
        }
    | PatternParenthesized
        { openParen : StartAndCommentsAfter
        , inner : Pattern
        , closedParen : StartAndCommentsAfter
        }
    | PatternTuple
        { openParen : StartAndCommentsAfter
        , part0 : Pattern
        , comma0 : StartAndCommentsAfter
        , part1 : Pattern
        , closedParen : StartAndCommentsAfter
        }
    | PatternTriple
        { openParen : StartAndCommentsAfter
        , part0 : Pattern
        , comma0 : StartAndCommentsAfter
        , part1 : Pattern
        , comma1 : StartAndCommentsAfter
        , part2 : Pattern
        , closedParen : StartAndCommentsAfter
        }
