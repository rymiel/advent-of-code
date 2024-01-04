def read_program(filename : String) : Array(Int64)
  File.read(filename).strip.split(",").map(&.to_i64)
end

class Program
  property ip = 0i64
  property base = 0i64
  @program : Array(Int64)
  @input_callback : -> Int64
  @output_callback : Int64 ->

  def initialize(@program, @input_callback, @output_callback)
  end

  def opcode
    self[ip] % 100
  end

  def addressing(n : Int32) : UInt8
    (self[ip] // (10 ** (n + 1)) % 10).to_u8!
  end

  def param(n : Int32) : Int64
    case addr = addressing(n)
    when 0 then self[self[ip + n]]
    when 1 then self[ip + n]
    when 2 then self[self[ip + n] + base]
    else        raise "Invalid addressing mode #{addr}"
    end
  end

  def assign(n : Int32) : Int64
    case addr = addressing(n)
    when 0 then self[ip + n]
    when 1 then raise "immediate addressing invalid for assignment"
    when 2 then self[ip + n] + base
    else        raise "Invalid addressing mode #{addr}"
    end
  end

  def [](addr : Int64) : Int64
    @program[addr]? || 0i64
  end

  def []=(addr : Int64, value : Int64)
    if addr >= @program.size
      new_array = Array(Int64).new(addr + 1) { |i| @program[i]? || 0i64 }
      @program = new_array
    end
    @program[addr] = value
  end

  def run
    loop do
      case opcode
      when 1
        self[assign(3)] = param(1) + param(2)
        @ip += 4
      when 2
        self[assign(3)] = param(1) * param(2)
        @ip += 4
      when 3
        self[assign(1)] = @input_callback.call
        @ip += 2
      when 4
        @output_callback.call param(1)
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
        self[assign(3)] = (param(1) < param(2)) ? 1 : 0
        @ip += 4
      when 8
        self[assign(3)] = (param(1) == param(2)) ? 1 : 0
        @ip += 4
      when 9
        @base += param(1)
        @ip += 2
      when 99
        break
      else raise "Invalid opcode #{opcode}"
      end
    end

    self
  end
end
