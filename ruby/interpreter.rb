YL_FALSE = nil
YL_TRUE = 1

# Scope for YL
class Scope
  def initialize(parent = nil)
    @vars = {}
    @parent = parent
  end

  def get(name)
    @vars[name] || (@parent && @parent.get(name))
  end

  def include?(name)
    !get(name).nil?
  end

  def set(name, value)
    @vars[name] = value
  end

  def extend
    Scope.new(@parent)
  end
end

# Syntax tree
class AST
  def initialize(ast)
    @ast = ast
  end

  def evaluate(scope, evaluate_function = true)
    if !@ast.is_a? Array
      evaluate_singleton(scope)
    elsif evaluate_function && function_to_execute?(scope)
      # Evaluate function
    else
      ret = YL_FALSE
      @ast.each do |subast|
        ret = AST.new(subast).evaluate(scope)
      end
      ret
    end
  end

  private

  def evaluate_singleton(scope)
    ret = scope.get(@ast)
    if ret
      ret
    elsif /^[0-9]+\.?[0-9]*$/ =~ ret
      @ast.to_f
    else
      @ast
    end
  end

  def function_to_execute?(scope)
    !@ast.empty? && !@ast[0].is_a?(Array) && scope.include?(@ast[0])
  end
end
