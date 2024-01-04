program = File.read("txt/day2").strip.split(",").map &.to_i

program[1] = 12
program[2] = 2

ip = 0
loop do
  case opcode = program[ip]
  when 1
    program[program[ip+3]] = program[program[ip+1]] + program[program[ip+2]]
    ip += 4
  when 2
    program[program[ip+3]] = program[program[ip+1]] * program[program[ip+2]]
    ip += 4
  when 99
    p program[0]
    exit
  else raise "Invalid opcode"
  end
end
