Goal: Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

This could enable some cool stuff like elm-review tests not complaining about whitespace or extra parens, or as a starting point for a gren formatter.

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
  - ranges of `then` and `of` are needed to determine whether they should be on the next line or at the end of last line
  - `"""` and `"` aren't differentiated

## considering for the future
  - create a _very_ lenient parser
