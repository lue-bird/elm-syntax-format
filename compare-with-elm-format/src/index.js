var fs = require('fs');
var glob = require('glob');
var path = require('path');
const { exec } = require('child_process');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
    Elm
} = require('./elm.js');

const app = Elm.Top.init();

glob("examples/*.elm", function (er, files) {
    if (files.length === 0) {
        console.log("No elm files found in examples/. Nested directories not supported!")
    } else {
        console.log("looking at " + files)
    }
    files.forEach(function (file) {
        fs.readFile(file, 'utf8', function (err, contents) {
            var filename = path.basename(file);
            app.ports.modelInPort.send([filename, contents]);
        });
    });
});

app.ports.codeOutPort.subscribe(request => {
    var filename = request[0];
    var contents = request[1];

    fs.mkdir("pre", () => { })
    fs.mkdir("post", () => { })
    fs.writeFile('pre/' + filename, contents, (err) => {
        if (err) throw err;

        exec('"elm-format" pre/' + filename + '  --output post/' + filename);
    });
});