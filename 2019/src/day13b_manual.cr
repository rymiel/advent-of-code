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
score = 0

program = read_program("txt/day13")
program[0] = 2
Program.new(program,
  ->{
    draw(screen)
    puts "Score : #{score}"
    move = gets(chomp: true)
    if move == "a"
      -1i64
    elsif move == "d"
      1i64
    else
      0i64
    end
  },
  ->(o : Int64) {
    buffer << o

    if buffer.size == 3
      x, y, tile = buffer
      buffer.clear

      if x == -1 && y == 0
        score = tile
      else
        screen[{x.to_i32, y.to_i32}] = Tile.new tile.to_u8
      end
    end
  }
).run

def draw(screen : Hash(Coords, Tile))
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
end

puts score
