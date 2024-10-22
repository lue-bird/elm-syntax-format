Pretty print an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree as [`elm-format`](https://github.com/avh4/elm-format) would
(breaking lines and inserting comments in the right places).

If you want to _generate_ code, better bets are [`mdgriffith/elm-codegen`](https://dark.elm.dmy.fr/packages/mdgriffith/elm-codegen/latest/) and [`the-sett/elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/).

## TODO
  - [ ] (!) integrate comments in all places elm-format allows
      - [ ] variant declaration parameters
      - [ ] some patterns
      - [ ] some expressions
      - [ ] type
      - [ ] expression/let declaration/port: between name and type

## known deviations
  - ranges of `then`, `of`, `exposing` and `as` are needed to determine whether they should be on the next line or at the end of last line
  - ranges of `=`, `,` and `|` (and the keywords) are needed to determine whether comments are before or after
  - function types outputting a parenthesized function do not preserve the parens because parens are not stored in the syntax tree
  - `"""` and `"` aren't differentiated
  - some floats in exponent representation are formatted to without it and the other way around
  - handling int, float overflow
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
        { field0 value, field0 value }
        ```
        →
        ```elm
        { field0 = value, field1 = value }
        ```
      - ```elm
        f | g | h
        ```
        →
        ```elm
        f |> g |> h
        ```
