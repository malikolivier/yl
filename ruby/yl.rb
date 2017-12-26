require_relative 'parser'
require_relative 'interpreter'

scope = Scope.new
if ARGV.empty?
  loop do
    print '> '
    cmd = gets
    ast = Parser.parse(cmd)
    # Parser.print_ast ast
    ret = AST.new(ast).evaluate(scope)
    puts ret
  end
else
  code = if ARGV[0] == '-e'
           ARGV[1]
         else
           File.read(ARGV[0])
         end
  ast = Parser.parse(code)
  ret = AST.new(ast).evaluate(scope)
  exit ret.to_i
end
