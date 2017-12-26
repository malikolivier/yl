YL_FALSE = nil
YL_TRUE = 1

GLOBAL_VARS = {
  'print' => lambda { |args, _scope|
    if args[0] == YL_FALSE
      puts '()'
    elsif args[0].is_a? Float
      puts args[0].to_i
    else
      puts args[0]
    end
  },
  '!' => lambda { |args, _scope|
    if args[0] == YL_FALSE
      YL_TRUE
    else
      YL_FALSE
    end
  },
  '=' => lambda { |args, _scope|
    if args[0] == args[1]
      YL_TRUE
    else
      YL_FALSE
    end
  },
  '<' => lambda { |args, _scope|
    if args[0] < args[1]
      YL_TRUE
    else
      YL_FALSE
    end
  },
  '<=' => lambda { |args, _scope|
    if args[0] <= args[1]
      YL_TRUE
    else
      YL_FALSE
    end
  },
  '>' => lambda { |args, _scope|
    if args[0] > args[1]
      YL_TRUE
    else
      YL_FALSE
    end
  },
  '>=' => lambda { |args, _scope|
    if args[0] >= args[1]
      YL_TRUE
    else
      YL_FALSE
    end
  },
  '+' => lambda { |args, _scope|
    args[0] + args[1]
  },
  '-' => lambda { |args, _scope|
    args[0] - args[1]
  },
  '*' => lambda { |args, _scope|
    args[0] * args[1]
  },
  '/' => lambda { |args, _scope|
    args[0] / args[1]
  },
  '%' => lambda { |args, _scope|
    args[0] % args[1]
  },
  'let' => lambda { |args, scope|
    scope.set(args[0], args[1])
    args[1]
  }
}.freeze

# Scope for YL
class Scope
  def initialize(parent = nil)
    @vars = if parent.nil?
              GLOBAL_VARS.dup
            else
              {}
            end
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
      case @ast[0]
      when 'def'
        scope.get('def').call(@ast.drop(1), scope)
      else
        args = @ast.drop(1).map do |subast|
          AST.new(subast).evaluate(scope)
        end
        scope.get(@ast[0]).call(args, scope)
      end
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
    elsif /^[0-9]+\.?[0-9]*$/ =~ @ast
      @ast.to_f
    else
      @ast
    end
  end

  def function_to_execute?(scope)
    !@ast.empty? && !@ast[0].is_a?(Array) && scope.include?(@ast[0])
  end
end
