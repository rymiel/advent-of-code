require "./intcode"

program = read_program("txt/day7")
PHASE_SETTINGS = [0, 1, 2, 3, 4]

p PHASE_SETTINGS.each_permutation.max_of { |permutation|
  amp = 0
  permutation.each do |phase|
    state = true
    cpu = Program.new(program.dup,
      ->{
        value = state ? phase : amp
        state = false
        value.to_i64
      },
      ->(o : Int64) { amp = o }
    ).run
  end
  amp
}
