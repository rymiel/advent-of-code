enum Opcode
  HLF
  TPL
  INC
  JMP
  JIE
  JIO
end

record Instruction, opcode : Opcode, register : UInt8, offset : Int32

program = File.read_lines("txt/day23").map { |line|
  register = 0u8
  offset = 0
  opcode, operands = line.split(" ", limit: 2)
  opcode = Opcode.parse opcode
  operands.split(", ").each { |i|
    if i == "a"
      register = 0u8
    elsif i == "b"
      register = 1u8
    else
      offset = i.to_i
    end
  }
  Instruction.new opcode, register, offset
}

ip = 0
registers = Array(Int32).new(2, 0)
loop do
  instr = program[ip]?
  break if instr.nil?

  next_ip = ip + 1

  case instr.opcode
  in .hlf? then registers[instr.register] //= 2
  in .tpl? then registers[instr.register] *= 3
  in .inc? then registers[instr.register] += 1
  in .jmp? then next_ip = ip + instr.offset
  in .jie? then next_ip = ip + instr.offset if registers[instr.register].even?
  in .jio? then next_ip = ip + instr.offset if registers[instr.register] == 1
  end

  ip = next_ip
end

p registers[1]
