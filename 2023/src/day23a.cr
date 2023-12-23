alias Coords = {Int32, Int32}

PATH   = Set(Coords).new
SLOPES = Hash(Coords, Char).new

LINES = File.read_lines("txt/day23")
LINES.each_with_index { |line, y|
  line.each_char_with_index { |c, x|
    next if c == '#'
    PATH << {x, y}
    SLOPES[{x, y}] = c if c != '.'
  }
}

HEIGHT = LINES.size
WIDTH  = LINES.first.size

START = PATH.find!(&.[1].== 0)
END   = PATH.max_by(&.[1])

DIRS = { {1, 0}, {0, 1}, {-1, 0}, {0, -1} }

EMPTY_SET = Set(Coords).new

def traverse(point : Coords, trail : Set(Coords)) : Set(Coords)
  neighbours = Array(Coords).new
  current = point
  loop do
    if current == END
      trail << current
      return trail
    end
    dirs = case SLOPES[current]?
           when '>' then { {1, 0} }
           when '<' then { {-1, 0} }
           when 'v' then { {0, 1} }
           when '^' then { {0, -1} }
           when nil then DIRS
           else          raise "Invalid slope"
           end
    # p! current
    # p! dirs
    # puts "north t=#{{current[0], current[1] - 1}.in?(trail)} p=#{{current[0], current[1] - 1}.in?(PATH)}"
    # puts "east  t=#{{current[0] + 1, current[1]}.in?(trail)} p=#{{current[0] + 1, current[1]}.in?(PATH)}"
    # puts "south t=#{{current[0], current[1] + 1}.in?(trail)} p=#{{current[0], current[1] + 1}.in?(PATH)}"
    # puts "west  t=#{{current[0] - 1, current[1]}.in?(trail)} p=#{{current[0] - 1, current[1]}.in?(PATH)}"
    neighbours = dirs.map { |(dx, dy)| {current[0] + dx, current[1] + dy} }.select { |i| !i.in?(trail) && i.in? PATH }
    # p! neighbours

    trail << current
    if neighbours.size == 1
      current = neighbours.first
    elsif neighbours.size == 0
      return EMPTY_SET # dead end
    else
      break
    end
  end

  # p! neighbours

  neighbours.map { |n| traverse(n, trail.dup) }.max_by &.size
end

trail = traverse(START, Set(Coords).new)

s = Time.monotonic
puts trail.size - 1
p (Time.monotonic - s)
p (Time.monotonic - s).total_microseconds
