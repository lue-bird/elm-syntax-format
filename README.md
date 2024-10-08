Goal: Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

This could enable some cool stuff like elm-review tests not complaining about whitespace or extra parens, or as a starting point for a gren formatter.

Don't directly use this for code-generation. Better bets are [`mdgriffith/elm-codegen`](https://dark.elm.dmy.fr/packages/mdgriffith/elm-codegen/latest/) and [`elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/).

## status
  - [ ] (!) integrate comments in all places elm-format allows
  - [ ] port declaration documentation
  - [x] module header
  - [x] imports
  - [x] declaration hulls
  - [x] pattern
  - [x] type
  - [x] expression
  - [x] module comment
  - [ ] int, float overflow like elm-format (honestly might not do this for now)
  - [ ] format documentation markdown (honestly might not do this for now)


## known deviations due to missing information in elm-syntax
  - ranges of `then`, `of`, `exposing` and `as` are needed to determine whether words/comments/... should be on the next line or at the end of last line
  - ranges of list and record `,` are needed to determine whether comments are before or after
  - `"""` and `"` aren't differentiated
  - some floats in exponent representation are formatted to without it and the other way around


## performance problems?
Performance is not a primary goal
but if you notice it's unusable for your use case, please open an issue.

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
