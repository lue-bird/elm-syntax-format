Code mostly copied from https://github.com/the-sett/elm-syntax-dsl/tree/master/test

`npm run start` takes .elm files under /examples,
applies formatting using elm-syntax-format and writes the files under pre/,
then applies elm-format and writes the files again under post/.

You can then run a diff (e.g. kdiff3)  on pre/ vs post/
to find where elm-format makes formatting changes.
