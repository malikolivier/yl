import sys

import parser
import interpreter


if len(sys.argv) < 2:
    cli_scope = interpreter.Scope()
    while True:
        cmd = input('> ')
        ast = parser.parse(cmd)
        ret = interpreter.evaluate(ast, cli_scope, False)
        print(ret)
else:
    if sys.argv[1] == '-e':
        code = sys.argv[2]
    else:
        f = open(sys.argv[1], 'r')
        code = f.read()
    ast = parser.parse(code)
    ret = interpreter.evaluate(ast)
    exit(ret)
