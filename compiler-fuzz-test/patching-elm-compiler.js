#!/usr/bin/env node

const fs = require('node:fs');
const path = require('node:path');
const { spawnSync } = require('node:child_process');

function findOutputPath(args) {
    for (let i = 0; i < args.length; i++) {
        if (args[i] === '--output' && typeof args[i + 1] === 'string') {
            return args[i + 1];
        }
    }
    throw new Error("elm-test didn't use --output flag, it seems: " + args.join(' '));
}

const args = process.argv.slice(2);
const outputPath = findOutputPath(args);

const result = spawnSync('elm', args, { stdio: 'inherit' });

if (result.status !== 0) {
    process.exit(result.status);
}

const outputDir = path.dirname(outputPath);
const projectDir = path.join(outputDir, 'elm-stuff', 'elm-make-compiles');
const srcDir = path.join(projectDir, 'src');
const elmJsonPath = path.join(projectDir, 'elm.json');
const modulePath = path.join(srcDir, 'FuzzedModule.elm');

try {
    fs.mkdirSync(projectDir, { recursive: true });
    fs.mkdirSync(srcDir, { recursive: true });
} catch (e) { }

const elmJson = {
    "type": "package",
    "name": "test/fuzzed-module",
    "summary": "Fuzzed module for testing compilation",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": ["FuzzedModule"],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.2 <= v < 2.0.0"
    },
    "test-dependencies": {}
};
fs.writeFileSync(elmJsonPath, JSON.stringify(elmJson, null, 4));

const absoluteProjectDir = path.resolve(projectDir);
const absoluteModulePath = path.resolve(modulePath);

const jsContent = fs.readFileSync(outputPath, 'utf8');

const escapedProjectDir = JSON.stringify(absoluteProjectDir);
const escapedModulePath = JSON.stringify(absoluteModulePath);

const functionPattern = /var \$author\$project\$ElmMake\$compiles = function \(elmSourceCode\) \{[^}]*return false;[^}]*\};/s;
const replacementFunction = `
// patching-elm-compiler.js START
var fs = require('node:fs');

// Hash function: https://stackoverflow.com/a/52171480
const TSH = s => { for(var i=0,h=9;i<s.length;)h=Math.imul(h^s.charCodeAt(i++),9**9);return h^h>>>9; };

// Memoize ElmMake.compiles
globalThis.__elmMakeCompilesMemo__ = new Map();

// Pre-computed paths (set up by patching-elm-compiler.js)
var $author$project$ElmMake$compiles$projectDir = ${escapedProjectDir};
var $author$project$ElmMake$compiles$modulePath = ${escapedModulePath};

var $author$project$ElmMake$compiles = function (elmSourceCode) {
    var { spawnSync } = require('node:child_process');

    var hash = TSH(elmSourceCode);
    var memo = globalThis.__elmMakeCompilesMemo__;
    if (memo.has(hash)) {
        return memo.get(hash);
    }

    console.log(elmSourceCode.slice(44,64).replace(/\\n/g, ' ').trim()+' ...');
    
    fs.writeFileSync($author$project$ElmMake$compiles$modulePath, elmSourceCode);
    
    var makeResult = spawnSync('elm', ['make', 'src/FuzzedModule.elm'], {
        cwd: $author$project$ElmMake$compiles$projectDir,
        stdio: 'pipe'
    });

    var result = makeResult.status === 0;
    if (!result) {
        console.log("elm make failed for source:");
        console.log(elmSourceCode);
        console.log('elm make error:');
        console.log(makeResult.stderr ? makeResult.stderr.toString() : '(no stderr)');
    }
    memo.set(hash, result);
    return result;
};

// patching-elm-compiler.js END
`;

const patchedContent = jsContent.replace(functionPattern, replacementFunction);

if (patchedContent === jsContent) {
    throw new Error('Warning: Could not find the compiles function to patch');
}

fs.writeFileSync(outputPath, patchedContent, 'utf8');
console.log(`Patched ${outputPath} successfully`);