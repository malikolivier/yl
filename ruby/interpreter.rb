# YlFalse
class YlFalse
  def to_i
    0
  end
end

YL_FALSE = YlFalse.new
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
  },
  'def' => lambda { |args, scope|
    rhs = lambda { |argv, _scope|
      fn_scope = scope.extend
      args[1].each_with_index do |name, i|
        if i < argv.length
          fn_scope.set(name, argv[i])
        else
          fn_scope.set(name, YL_FALSE)
        end
      end
      AST.new(args.drop(2)).evaluate(fn_scope, false)
    }
    scope.set(args[0], rhs)
    rhs
  },
  'if' => lambda { |args, scope|
    if args[0] == YL_FALSE
      if args.length > 2
        AST.new(args[2]).evaluate(scope, false)
      else
        YL_FALSE
      end
    else
      AST.new(args[1]).evaluate(scope, false)
    end
  },
  'loop' => lambda { |args, scope|
    ret = YL_FALSE
    identifier, values, exp = args
    loop_ast = AST.new(exp)
    if values[0] == 'range'
      if values.length == 2
        mini = 0
        maxi = AST.new(values[1]).evaluate(scope)
      else
        mini = AST.new(values[1]).evaluate(scope)
        maxi = AST.new(values[2]).evaluate(scope)
      end
      values = mini.to_i..(maxi.to_i - 1)
    end
    loop_scope = scope.extend
    values.each do |value|
      loop_scope.set(identifier, value)
      ret = loop_ast.evaluate(loop_scope)
    end
    ret
  },
  'argv' => lambda { |args, _scope|
    n = args[0].to_i + 1
    if n < ARGV.length
      if /^[0-9]+\.?[0-9]*$/ =~ ARGV[n]
        ARGV[n].to_f
      else
        ARGV[n]
      end
    else
      YL_FALSE
    end
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
      when 'if'
        cond = AST.new(@ast[1]).evaluate(scope)
        args = [cond]
        @ast.drop(2).each do |subast|
          args.push(subast)
        end
        scope.get('if').call(args, scope)
      when 'loop'
        scope.get('loop').call(@ast.drop(1), scope)
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
