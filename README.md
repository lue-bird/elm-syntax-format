Goal: Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

This could enable some cool stuff like elm-review tests not complaining about whitespace or extra parens, or as a starting point for a gren formatter.

## status
  - [ ] (!) integrate comments in all places elm-format allows
  - [ ] port declaration documentation
  - [x] module header
      - [ ] exposing members group according to doc tags
  - [x] imports
  - [x] declaration hulls
  - [x] pattern
      - [ ] add more tests
  - [x] type
  - [x] expression
      - [ ] if then else if indentation combine
      - [ ] (!) <| formatting and better right/non-associative operation formatting in general (adjust indentation for left/right etc)
  - [x] module comment
  - [ ] int, float overflow like elm-format (honestly might not do this for now)
  - [ ] format documentation markdown (honestly might not do this for now)

## considering for the future
  - create a _very_ lenient parser
