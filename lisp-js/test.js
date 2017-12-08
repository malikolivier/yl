const Parser = require('./parser');
const Interpreter = require('./interpreter');

console.log('======================= Parser =======================');

console.log(
  Parser('')
)
console.log(
  Parser('fa')
)
console.log(
  Parser('fa "fd 1"')
)
console.log(
  Parser('()')
)

console.log(
  Parser('(print 1)')
)

console.log(
  Parser('(let x 1) (do it later ())')
)

console.log(
  Parser('(let x 1) (do it later ()) \n ; f \n as')
)


console.log('===================== Interpreter =====================');


console.log(
    Interpreter([])
)


console.log(
    Interpreter('1')
)


console.log(
    Interpreter([ 'fa' ])
)

console.log(
    Interpreter([ [] ])
)

//console.log(
    Interpreter([ [ 'print', '1' ] ])
//)
