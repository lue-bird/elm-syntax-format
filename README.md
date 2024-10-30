Pretty print an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree as [`elm-format`](https://github.com/avh4/elm-format) would
(breaking lines and inserting comments in the right places).

If you want to _generate_ code, better bets are [`mdgriffith/elm-codegen`](https://dark.elm.dmy.fr/packages/mdgriffith/elm-codegen/latest/) and [`the-sett/elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/).

## TODO
  - put -- comment after on the same line if syntax is single-line and is originally on the same row: type function arguments, recordLiteral/typeRecordExtension if name and value on same line
  - put consecutive {- -} comments in one line if following syntax is single-line

## known deviations in printing
  - ranges of `of`, `exposing` and `as` are needed to determine whether they should be on the next line or at the end of last line
  - ranges of `=`, `:`, `,`, `|`, `->`, the operators and the keywords are needed to determine whether comments are before or after
  - function types outputting a parenthesized function do not preserve the parens because type parens are not stored in the syntax tree
  - comments before/after parenthesized types will get eaten because type parens are not stored in the syntax tree
  - some floats in exponent representation are formatted to without it and the other way around
  - handling int (and float?) overflow (elm-format itself seems to have issues here, too https://github.com/avh4/elm-format/issues/635)
  - formatting documentation markdown

I'm 100% sure there are other deviations I've missed. Please [report](https://github.com/lue-bird/elm-syntax-format/issues/new) if you notice some <3

## performance problems?
Great performance is not a primary goal
but if you notice it's unusable for your use case, please [open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new).

Ideas:
  - pre-filter comments per declaration
  - switch Print to defunctionalized
  - pre-define prints for static parts like symbols

## considering for the future
  - create a _very_ lenient parser. For example:
      - ```elm
        function parameters : Type = result
        ```
        →
        ```elm
        function : Type
        function parameters =
            result
        ```
      - ```elm
        { field0 value, field1 value }
        ```
        →
        ```elm
        { field0 = value, field1 = value }
        ```
        or
        ```elm
        { field0 : value, field1 : value }
        ```
      - ```elm
        { field0, field1 }
        ```
        →
        ```elm
        { field0 = field0, field1 = field1 }
        ```
      - ```elm
        f | g | h
        ```
        →
        ```elm
        f |> g |> h
        ```
      - ```elm
        3 |> String.toInt
        of case
            Nothing ->
                0
              
            Just n ->
                n
        ```
        →
        ```elm
        case 3 |> String.toInt of
            Nothing ->
                0
              
            Just n ->
                n
        ```
