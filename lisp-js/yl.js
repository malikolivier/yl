const fs = require('fs');
const readline = require('readline');

const Parser = require('./parser');
const Interpreter = require('./interpreter');

var inputFile = process.argv[2];
if (!inputFile) {
    var rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    process.stdout.write('> ');
    rl.on('line', function (cmd) {
        var ast = Parser(cmd);
        var ret = Interpreter(ast);
        console.log(ret);
        process.stdout.write('> ');
    });
} else {
    var code = fs.readFileSync(inputFile, 'utf8');
    var ast = Parser(code);
    var ret = Interpreter(ast);

    process.exit(ret);
}
