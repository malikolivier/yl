const fs = require('fs');

const Parser = require('./parser');
const Interpreter = require('./interpreter');

var inputFile = process.argv[2];
if (!inputFile) {
    console.error('No input!');
    process.exit(1);
}
var code = fs.readFileSync(inputFile, 'utf8');
var ast = Parser(code);
var ret = Interpreter(ast);

process.exit(ret);
