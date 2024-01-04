require "./intcode"

enum Tile : UInt8
  Empty
  Wall
  Block
  Paddle
  Ball
end

alias Coords = {Int32, Int32}
screen = Hash(Coords, Tile).new
buffer = [] of Int64

Program.new(read_program("txt/day13"),
  ->{ 0i64 },
  ->(o : Int64) {
    buffer << o

    if buffer.size == 3
      x, y, tile = buffer
      buffer.clear

      screen[{x.to_i32, y.to_i32}] = Tile.new tile.to_u8
    end
  }
).run

min_x, max_x = screen.keys.minmax_of(&.[0])
min_y, max_y = screen.keys.minmax_of(&.[1])

(min_y - 1..max_y + 1).each { |y|
  (min_x - 1..max_x + 1).each { |x|
    tile = screen[{x, y}]? || Tile::Empty
    print case tile
    in .empty?  then ' '
    in .wall?   then '\u2588'
    in .block?  then '\u2592'
    in .paddle? then '\u2501'
    in .ball?   then '\u00b7'
    end
  }
  puts
}

puts screen.values.count(&.block?)
