// Transpile to JS
// How to use: node transpiler.js my-file.yl

const fs = require('fs');
const Parser = require('./parser');

const input = process.argv[2];
const code = fs.readFileSync(input, 'utf8');

const ast = Parser(code);
const astJson = JSON.stringify(ast);
const interpreter = fs.readFileSync(`${__dirname}/interpreter.js`, 'utf8');

// Output transpiled code
console.log(`const INPUT_AST = ${astJson};`)
console.log(interpreter)
// Replace argv built-in function for transpiled code to work
console.log('GLOBAL_VARS.argv = function (n) { return process.argv[n+2] }');
console.log('evaluate(INPUT_AST);')
