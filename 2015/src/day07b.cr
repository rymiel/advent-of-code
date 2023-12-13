WIRES = Hash(String, UInt16).new

enum Operation
  And
  Or
  LShift
  RShift
  Not
  Identity
end

alias Operand = String | UInt16 | Nil

record Calc, op : Operation, a : Operand, b : Operand, output : String do
  def calculable?
    a = @a
    if a.is_a? String
      return false if WIRES[a]?.nil?
    end
    b = @b
    if b.is_a? String
      return false if WIRES[b]?.nil?
    end
    return true
  end

  def evaluate!
    return if WIRES.has_key? output

    a = @a
    if a.is_a? String
      a = WIRES[a]
    end
    b = @b
    if b.is_a? String
      b = WIRES[b]
    end

    res = case op
          in .and?      then a.not_nil! & b.not_nil!
          in .or?       then a.not_nil! | b.not_nil!
          in .r_shift?  then a.not_nil! >> b.not_nil!
          in .l_shift?  then a.not_nil! << b.not_nil!
          in .not?      then ~a.not_nil!
          in .identity? then a.not_nil!
          end

    WIRES[output] = res
  end
end

def read_operand(str : String) : Operand
  if num = str.to_u16?
    return num
  end
  return str
end

def read_operation(str : String) : Operation
  case str
  when "AND"    then Operation::And
  when "OR"     then Operation::Or
  when "LSHIFT" then Operation::LShift
  when "RSHIFT" then Operation::RShift
  when "NOT"    then Operation::Not
  else               raise "Invalid operation"
  end
end

calcs = File.read_lines("txt/day7").map do |line|
  input, output = line.split(" -> ")
  parts = input.split(" ")
  case parts.size
  when 1 then Calc.new(:identity, read_operand(parts[0]), nil, output)
  when 2 then Calc.new(read_operation(parts[0]), read_operand(parts[1]), nil, output)
  when 3 then Calc.new(read_operation(parts[1]), read_operand(parts[0]), read_operand(parts[2]), output)
  else        raise "Invalid input"
  end
end

def iterative_evaluate(calcs : Array(Calc))
  factored = Set(Int32).new

  until factored.size == calcs.size
    calcs.each_with_index do |calc, index|
      next unless calc.calculable?
      next if index.in? factored
      calc.evaluate!
      factored << index
    end
  end
end

iterative_evaluate calcs

wire_a = WIRES["a"]
WIRES.clear
WIRES["b"] = wire_a

iterative_evaluate calcs

p WIRES["a"]
