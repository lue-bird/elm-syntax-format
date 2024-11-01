#!/bin/bash

# # Instructions
#
# Run the script like (from the root folder) :
#
#     ./find-parser-regressions.sh some-folder/
#
# To run it on all Elm packages in your ELM_HOME, run:
#
#     ./find-parser-regressions.sh ~/.elm/0.19.1/
#
# The version it will compare to is the one in `find-parser-regressions/published/elm.json`
# under the `dependencies.direct.['stil4m/elm-syntax']` field.
#
# The script will also give performance metrics.
# If you only want to get that (and don't care about the regression check), run:
#
#     ./find-parser-regressions.sh --no-check some-folder/
#

cd find-parser-regressions/current
elm make --optimize ../src/ParseMain.elm --output elm.js > /dev/null
sed -e 's/$author$project$ParseMain$timeStart(version)/globalThis.performance.now()/' \
    -e 's/$author$project$ParseMain$timeEnd(version)/globalThis.measurements[version].push(globalThis.performance.now() - start)/' \
    -i elm.js

cd ../..
cd find-parser-regressions/published
elm make --optimize ../src/ParseMain.elm --output elm.js > /dev/null
sed -e 's/$author$project$ParseMain$timeStart(version)/globalThis.performance.now()/' \
    -e 's/$author$project$ParseMain$timeEnd(version)/globalThis.measurements[version].push(globalThis.performance.now() - start)/' \
    -i elm.js

cd ../..
node find-parser-regressions $@