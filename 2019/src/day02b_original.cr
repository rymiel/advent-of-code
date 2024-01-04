initial_program = File.read("txt/day2").strip.split(",").map &.to_i

(0..99).each do |noun|
  (0..99).each do |verb|
    program = initial_program.dup
    program[1] = noun
    program[2] = verb
    ip = 0
    loop do
      case opcode = program[ip]
      when 1
        program[program[ip + 3]] = program[program[ip + 1]] + program[program[ip + 2]]
        ip += 4
      when 2
        program[program[ip + 3]] = program[program[ip + 1]] * program[program[ip + 2]]
        ip += 4
      when 99
        if program[0] == 19690720
          p (100 * noun) + verb
          exit
        end
        break
      else raise "Invalid opcode"
      end
    end
  end
end
