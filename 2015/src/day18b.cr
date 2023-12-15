alias Coords = {Int32, Int32}

DIRECTIONS = { {-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1} }

class Board
  @alive = Set(Coords).new

  def initialize(@size : Int32)
    @alive << {0, 0} << {0, @size - 1} << {@size - 1, 0} << {@size - 1, @size - 1}
  end

  def <<(coords : Coords)
    @alive << coords
  end

  def neighbours(point : Coords)
    DIRECTIONS.count { |(dx, dy)|
      {point[0] + dx, point[1] + dy}.in? @alive
    }
  end

  def step
    new_state = Set(Coords).new
    (0...@size).each do |y|
      (0...@size).each do |x|
        stay_alive = {x, y}.in? @alive
        neighbour_count = neighbours({x, y})
        if stay_alive
          stay_alive = (neighbour_count == 2) || (neighbour_count == 3)
        else
          stay_alive = neighbour_count == 3
        end

        new_state << {x, y} if stay_alive
      end
    end

    new_state << {0, 0} << {0, @size - 1} << {@size - 1, 0} << {@size - 1, @size - 1}

    @alive = new_state
  end

  def count
    @alive.size
  end
end

board = Board.new 100

File.read_lines("txt/day18").each_with_index { |line, y|
  line.each_char_with_index { |c, x|
    board << {x, y} if c == '#'
  }
}

100.times { board.step }
p board.count
