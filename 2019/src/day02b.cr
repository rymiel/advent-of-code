require "./intcode"

initial_program = read_program("txt/day2")
(0..99).each do |noun|
  (0..99).each do |verb|
    program = initial_program.dup
    program[1] = noun
    program[2] = verb
    cpu = Program.new(program,
      ->{ 0i64 },
      ->(o : Int64) {}
    ).run

    if cpu[0] == 19690720
      p (100 * noun) + verb
      exit
    end
  end
end
