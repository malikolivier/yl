# Stream chars one by one out of an input string
class InputStream
  attr_reader :input

  def initialize(input)
    @input = input
    @pos = 0
    @line = 1
    @col = 0
  end

  def next
    ch = @input[@pos]
    @pos += 1
    if ch == '\n'
      @line += 1
      @col = 0
    else
      @col += 1
    end
    ch
  end

  def peek
    return unless @pos < @input.length
    @input[@pos]
  end

  def eof
    peek.nil?
  end

  def croak(msg)
    raise Error("#{msg}, #{@line}:#{@col}")
  end
end

# Stream tokens one by one out of an input stream
class TokenStream
  def initialize(input)
    @input = input
  end

  def croak
    @input.croak
  end

  def next
    tok = @current
    @current = nil
    tok || read_next
  end

  private

  def read_while
    string = ''
    string += @input.next while !@input.eof && yield(@input.peek)
    string
  end

  def read_escaped
    escaped = false
    string = ''
    @input.next
    until @input.eof
      ch = @input.next
      if escaped
        string += ch
        escaped = false
      elsif ch == '\\'
        escaped = true
      elsif ch == '"'
        break
      else
        string += ch
      end
    end
    string
  end

  def read_string
    { type: :symbol, value: read_escaped }
  end

  def read_symbol
    identifier = read_while { |c| !" \t\n()".include?(c) }
    { type: :symbol, value: identifier }
  end

  def skip_comment
    read_while { |c| c != '\n' }
    @input.next
  end

  def read_next
    read_while { |c| " \t\n".include?(c) }
    return if @input.eof
    case @input.peek
    when ';'
      skip_comment
      read_next
    when '"'
      read_string
    when '(', ')'
      { type: :punc, value: @input.next }
    else
      read_symbol
    end
  end
end

# Parse code
module Parser
  class << self
    def parse(code)
      build_ast(TokenStream.new(InputStream.new(code)))
    end

    def print_ast(ast)
      print '('
      ast.each do |node|
        if node.is_a?(Array)
          print_ast(node)
        else
          print node
          print ' '
        end
      end
      print ') '
    end

    private

    def build_ast(tokstream)
      ast = []
      tok = tokstream.next
      while not_closed?(tok)
        if tok[:type] == :symbol
          ast.push(tok[:value])
        elsif tok[:type] == :punc && tok[:value] == '('
          ast.push(build_ast(tokstream))
        end
        tok = tokstream.next
      end
      ast
    end

    def not_closed?(tok)
      !tok.nil? && !(tok[:type] == :punc && tok[:value] == ')')
    end
  end
end
