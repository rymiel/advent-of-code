require "./intcode"

program = read_program("txt/day2")
program[1] = 12
program[2] = 2
cpu = Program.new(program,
  ->{ 0i64 },
  ->(o : Int64) { }
).run

puts cpu[0]

