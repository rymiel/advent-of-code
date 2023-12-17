map = File.read_lines("txt/day17").map { |line|
  line.each_char.map_with_index { |c|
    c.to_i
  }
}

alias Coords = {Int32, Int32}

class LavaMap
  @map : Array(Array(Int32))

  def initialize(@map)
  end

  def at(coords : Coords)
    @map[coords[1]][coords[0]]
  end

  def height
    @map.size
  end

  def width
    @map.first.size
  end

  def out_of_bounds(point : Coords) : Bool
    point[0] < 0 || point[1] < 0 || point[0] >= width || point[1] >= height
  end

  def neighbours(point : Coords, & : Coords, UInt8 ->)
    { {-1, 0}, {0, -1}, {0, 1}, {1, 0} }.each_with_index do |diff, i|
      next_point = {point[0] + diff[0], point[1] + diff[1]}
      next if out_of_bounds(next_point)
      yield next_point, i.to_u8!
    end
  end

  OPPOSITE = [3, 2, 1, 0]

  def a_star(start : Coords, goal : Coords)
    open_set = Set({Int32, Coords, Array(UInt8)}).new
    open_set << {0, start, [] of UInt8}

    seen = Set({Coords, Array(UInt8)}).new

    until open_set.empty?
      best = open_set.min_by { |(s, i, h)| s }
      open_set.delete(best)
      score, current, history = best
      # p! score, current, history
      if current == goal
        return score
      end

      next if {current, history}.in? seen
      seen << {current, history}

      neighbours(current) { |neighbour, dir|
        mismatch = dir == history.last?
        next_history = mismatch ? [*history, dir] : [dir]
        next if next_history.size >= 4
        next if history[-1]? == OPPOSITE[dir]

        next if {neighbour, next_history}.in? seen

        new_score = score + at(neighbour)
        open_set << {new_score, neighbour, next_history}
      }
    end

    raise "No path!"
  end

  def run
    a_star({0, 0}, {width - 1, height - 1})
  end
end

p LavaMap.new(map).run

