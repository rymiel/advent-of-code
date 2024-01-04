require "./intcode"

alias Coords = {Int32, Int32}
DIRS = { {0, -1}, {1, 0}, {0, 1}, {-1, 0} }

white_tiles = Set(Coords).new
pos = {0, 0}
dir = 0
state = true

white_tiles << {0, 0}

def draw(white_tiles : Set(Coords))
  puts
  return puts "." if white_tiles.empty?
  min_x, max_x = white_tiles.minmax_of(&.[0])
  min_y, max_y = white_tiles.minmax_of(&.[1])

  (min_y - 1..max_y + 1).each { |y|
    (min_x - 1..max_x + 1).each { |x|
      print(white_tiles.includes?({x, y}) ? '\u2588' : ' ')
    }
    puts
  }
end

Program.new(read_program("txt/day11"),
  ->{ white_tiles.includes?(pos) ? 1i64 : 0i64 },
  ->(o : Int64) {
    if state
      if o.zero?
        white_tiles.delete(pos)
      else
        white_tiles.add(pos)
      end
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

draw(white_tiles)
