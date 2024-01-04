require "./intcode"

program = read_program("txt/day7")
PHASE_SETTINGS = [5, 6, 7, 8, 9]

p PHASE_SETTINGS.each_permutation.max_of { |permutation|
  amp = 0
  channels = permutation.map { Channel(Int64).new 1 }
  terminal = Channel(Bool).new
  cpus = permutation.map_with_index { |phase, i|
    spawn do
      cpu = Program.new(program.dup,
        ->{ channels[i].receive },
        ->(o : Int64) {
          n = (i + 1) % permutation.size
          channels[n].send o
          amp = o if n == 0
        }
      ).run
      terminal.send true
    end
    channels[i].send phase
  }
  channels[0].send 0i64
  permutation.size.times { terminal.receive }
  amp
}
