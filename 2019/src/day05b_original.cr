program = File.read("txt/day5").strip.split(",").map &.to_i

class Program
  property ip = 0
  getter program

  def initialize(@program : Array(Int32))
  end

  def opcode
    program[ip] % 100
  end

  def addressing(n : Int32) : Int32
    program[ip] // (10 ** (n + 1)) % 10
  end

  def param(n : Int32) : Int32
    case addr = addressing(n)
    when 0 then program[at(n)]
    when 1 then at(n)
    else        raise "Invalid addressing mode #{addr} for #{program[ip]} @ #{n}"
    end
  end

  def at(n : Int32) : Int32
    program[ip + n]
  end

  def run
    loop do
      case opcode
      when 1
        program[at(3)] = param(1) + param(2)
        @ip += 4
      when 2
        program[at(3)] = param(1) * param(2)
        @ip += 4
      when 3
        program[at(1)] = 5
        @ip += 2
      when 4
        diagnostic = param(1)
        p diagnostic if diagnostic != 0
        @ip += 2
      when 5
        if !param(1).zero?
          @ip = param(2)
        else
          @ip += 3
        end
      when 6
        if param(1).zero?
          @ip = param(2)
        else
          @ip += 3
        end
      when 7
        program[at(3)] = (param(1) < param(2)) ? 1 : 0
        @ip += 4
      when 8
        program[at(3)] = (param(1) == param(2)) ? 1 : 0
        @ip += 4
      when 99
        exit
      else raise "Invalid opcode #{opcode}"
      end
    end
  end
end

Program.new(program).run
