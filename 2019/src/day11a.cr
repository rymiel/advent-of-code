require "./intcode"

alias Coords = {Int32, Int32}
DIRS = { {0, -1}, {1, 0}, {0, 1}, {-1, 0} }

white_tiles = Set(Coords).new
painted = Set(Coords).new
pos = {0, 0}
dir = 0
state = true

Program.new(read_program("txt/day11"),
  ->{ white_tiles.includes?(pos) ? 1i64 : 0i64 },
  ->(o : Int64) {
    if state
      if o.zero?
        white_tiles.delete(pos)
      else
        white_tiles.add(pos)
      end
      painted.add(pos)
    else
      if o.zero?
        dir -= 1
      else
        dir += 1
      end
      dir %= 4
      pos = {pos[0] + DIRS[dir][0], pos[1] + DIRS[dir][1]}
    end
    state = !state
  }
).run

puts(painted.size)
