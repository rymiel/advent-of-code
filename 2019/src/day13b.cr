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
ball = 0
paddle = 0
score = 0

program = read_program("txt/day13")
program[0] = 2
Program.new(program,
  ->{ (ball <=> paddle).to_i64 },
  ->(o : Int64) {
    buffer << o

    if buffer.size == 3
      x, y, tile = buffer
      buffer.clear

      if x == -1 && y == 0
        score = tile
      else
        t = Tile.new tile.to_u8
        screen[{x.to_i32, y.to_i32}] = t
        if t.ball?
          ball = x.to_i32
        elsif t.paddle?
          paddle = x.to_i32
        end
      end
    end
  }
).run

puts score
