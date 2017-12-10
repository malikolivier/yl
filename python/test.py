import parser
import interpreter

print('======================= Parser =======================');
print(
  parser.parse('')
)
print(
  parser.parse('fa')
)
print(
  parser.parse('fa "fd 1"')
)
print(
  parser.parse('()')
)

print(
  parser.parse('(print 1)')
)

print(
  parser.parse('(let x 1) (do it later ())')
)

print(
  parser.parse('(let x 1) (do it later ()) \n ; f \n as')
)

print('===================== Interpreter =====================');

print(
    interpreter.evaluate([])
)

print(
    interpreter.evaluate('1')
)


print(
    interpreter.evaluate([ 'fa' ])
)

print(
    interpreter.evaluate([ [] ])
)

#print(
interpreter.evaluate([ [ 'print', '1' ] ])
#)
