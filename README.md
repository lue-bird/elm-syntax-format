Goal: Pretty printing an [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) tree
in a way consistent with [`elm-format`](https://github.com/avh4/elm-format).

This could enable some cool stuff like elm-review tests not complaining about whitespace or extra parens, or as a starting point for a gren formatter.

## status
  - [ ] (!) integrate comments in all places elm-format allows
  - [ ] (!) <| formatting and better right/non-associative operation formatting in general (adjust indentation for left/right etc)
  - [ ] port declaration documentation
  - [ ] int, hex, float overflow like elm-format
  - [x] module header
  - [x] imports, except:
    - [ ] group according to doc tags
  - [x] declaration hulls
  - [x] pattern
      - [ ] add more tests
  - [x] type
      - [ ] add more tests
  - [x] module comment
  - [ ] format documentation markdown (honestly might not do this for now)

## considering for the future
  - create a _very_ lenient parser
