Pretty print an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree as [`elm-format`](https://github.com/avh4/elm-format) would
(breaking lines and inserting comments in the right places).

```elm
import ElmSyntaxPrint
import ElmSyntaxParserLenient

"""
module   Sample  exposing(...)
plus2 (n)= {- this adds 2-} n
+
2
"""
    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
    |> Maybe.map
        (\syntaxModule ->
            syntaxModule
                |> ElmSyntaxPrint.module_
                |> ElmSyntaxPrint.toString
        )
-->
Just """module Sample exposing (..)


plus2 n =
    {- this adds 2 -}
    n
        + 2
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/elm-syntax-format/tree/main/node-elm-syntax-format).

If you want to _generate_ code, better bets are [`mdgriffith/elm-codegen`](https://dark.elm.dmy.fr/packages/mdgriffith/elm-codegen/latest/) or [`the-sett/elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/).

## known deviations in printing
  - ranges of `of`, `exposing` and `as` are needed to determine whether they should be on the next line or at the end of last line
  - ranges of `=`, `:`, `,`, `|`, `->`, pattern `::`, the operators and the keywords are needed to determine whether comments are before or after
  - function types outputting a parenthesized function do not preserve the parens because type parens are not stored in the syntax tree
  - comments before/after parenthesized types will get eaten because type parens are not stored in the syntax tree
  - some floats in exponent representation are formatted to without it and the other way around
  - handling int (and float?) overflow (elm-format itself seems to have issues here, too https://github.com/avh4/elm-format/issues/635)
  - formatting documentation markdown

Please [report](https://github.com/lue-bird/elm-syntax-format/issues/new) others you notice <3

## performance problems?
Please [open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new).
