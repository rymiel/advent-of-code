require "stumpy_png"

record Connections, n : Bool = false, e : Bool = false, s : Bool = false, w : Bool = false, start : Bool = false do
  def [](symbol : Symbol)
    case symbol
    when :n then @n
    when :s then @s
    when :e then @e
    when :w then @w
    else         raise "Invalid direction"
    end
  end
end

alias Coords = {Int32, Int32}

DIRECTIONS_WITH_NAME = [
  {:n, 0, -1},
  {:s, 0, 1},
  {:e, 1, 0},
  {:w, -1, 0},
]

OPPOSITE_DIR = {
  n: :s,
  s: :n,
  e: :w,
  w: :e,
}

def connections(char : Char) : Connections
  case char
  when '|' then Connections.new n: true, s: true
  when '-' then Connections.new e: true, w: true
  when 'L' then Connections.new n: true, e: true
  when 'J' then Connections.new n: true, w: true
  when '7' then Connections.new s: true, w: true
  when 'F' then Connections.new s: true, e: true
  when '.' then Connections.new
  when 'S' then Connections.new n: true, e: true, s: true, w: true, start: true
  else          raise "Invalid pipe"
  end
end

MAP = File.read_lines("txt/day10").map_with_index do |line, y|
  line.chars.map_with_index do |char, x|
    { {x, y}, connections(char) }
  end
end.flatten.to_h

def find_neighbours(coords : Coords)
  DIRECTIONS_WITH_NAME.compact_map do |(dir, x_diff, y_diff)|
    adjacent_coords = {coords[0] + x_diff, coords[1] + y_diff}
    next nil unless MAP[adjacent_coords]?.try &.[OPPOSITE_DIR[dir]]
    next nil unless MAP[coords][dir]
    adjacent_coords
  end
end

def find_neighbour_directions(coords : Coords)
  DIRECTIONS_WITH_NAME.compact_map do |(dir, x_diff, y_diff)|
    adjacent_coords = {coords[0] + x_diff, coords[1] + y_diff}
    next nil unless MAP[adjacent_coords]?.try &.[OPPOSITE_DIR[dir]]
    next nil unless MAP[coords][dir]
    dir
  end
end

start_point = MAP.find! &.[1].start
neighbours = find_neighbours start_point[0]
neighbour_directions = find_neighbour_directions start_point[0]
real_start_shape = case {neighbour_directions[0], neighbour_directions[1]}
                   when {:n, :s} then '|'
                   when {:e, :w} then '-'
                   when {:n, :e} then 'L'
                   when {:n, :w} then 'J'
                   when {:s, :w} then '7'
                   when {:s, :e} then 'F'
                   else               raise "invalid start shape"
                   end
MAP[start_point[0]] = connections(real_start_shape).copy_with(start: true)

cycle = [] of Coords

previous = start_point[0]
traversing = neighbours.first
endpoint = neighbours.last
cycle << previous
cycle << traversing

until traversing == endpoint
  next_neighbours = find_neighbours(traversing)
  next_traversing = next_neighbours.reject(previous).first
  previous = traversing
  traversing = next_traversing
  cycle << traversing
end

cycle << endpoint

cycle = cycle.to_set

WIDTH     = MAP.keys.max_of(&.[0]) + 1
EX_WIDTH  = WIDTH * 3
HEIGHT    = MAP.keys.max_of(&.[1]) + 1
EX_HEIGHT = HEIGHT * 3
DIRS      = { {0, 1}, {0, -1}, {1, 0}, {-1, 0} }

EXPANDED_MAP    = Set({Int32, Int32}).new
EXPANDED_POINTS = Set({Int32, Int32}).new

MAP.each do |key, value|
  x, y = key
  EXPANDED_POINTS << {x * 3 + 1, y * 3 + 1}
  next unless cycle.includes? key
  EXPANDED_MAP << {x * 3 + 1, y * 3 + 1}
  EXPANDED_MAP << {x * 3 + 1, y * 3 + 0} if value.n
  EXPANDED_MAP << {x * 3 + 1, y * 3 + 2} if value.s
  EXPANDED_MAP << {x * 3 + 2, y * 3 + 1} if value.e
  EXPANDED_MAP << {x * 3 + 0, y * 3 + 1} if value.w
end

def out_of_bounds(coords : Coords) : Bool
  (coords[0] < 0) || (coords[1] < 0) || (coords[0] >= EX_WIDTH) || (coords[1] >= EX_HEIGHT)
end

def spread_outside(outside : Set(Coords))
  (0...EX_WIDTH).each do |x|
    (0...EX_HEIGHT).each do |y|
      next if EXPANDED_MAP.includes?({x, y})
      spread_to = DIRS.any? { |dir|
        neighbour = {x + dir[0], y + dir[1]}
        out_of_bounds(neighbour) || outside.includes? neighbour
      }
      outside << {x, y} if spread_to
    end
  end
  outside
end

outside = Set(Coords).new

previous_outside_count = outside.size
loop do
  spread_outside outside

  break if outside.size == previous_outside_count
  previous_outside_count = outside.size
end

# Original text-based visualizations

# (0...EX_WIDTH).each do |x|
#   (0...EX_HEIGHT).each do |y|
#     if EXPANDED_MAP.includes?({x, y})
#       print '#'
#     elsif outside.includes?({x, y})
#       print ' '
#     elsif EXPANDED_POINTS.includes?({x, y})
#       print '~'
#     else
#       print '.'
#     end
#   end
#   puts
# end

# File.read_lines("txt/day10").map_with_index do |line, y|
#   line.each_char_with_index do |char, x|
#     if cycle_index = cycle.index({x, y})
#       print "\e[34m"
#     end
#     if outside_index = outside.index({x * 3 + 1, y * 3 + 1})
#       print "\e[92m"
#     end
#     if cycle_index && outside_index
#       print "\e[91m\e[5m" # this should never happen
#     end
#     print char
#     print "\e[0m"
#   end
#   puts
# end

# New image-based visualizations

canvas = StumpyPNG::Canvas.new(WIDTH, HEIGHT)
(0...WIDTH).each do |x|
  (0...HEIGHT).each do |y|
    in_cycle = cycle.includes?({x, y})
    in_outside = outside.includes?({x * 3 + 1, y * 3 + 1})
    if in_cycle && in_outside
      canvas[x, y] = StumpyPNG::RGBA::RED # this should never happen
    elsif in_cycle
      canvas[x, y] = StumpyPNG::RGBA::DARKBLUE
    elsif in_outside
      canvas[x, y] = StumpyPNG::RGBA::ORANGE
    else
      canvas[x, y] = StumpyPNG::RGBA::WHITE
    end
  end
end
StumpyPNG.write(canvas, "day10b_visualization_1.png")

canvas = StumpyPNG::Canvas.new(EX_WIDTH, EX_HEIGHT)
(0...EX_WIDTH).each do |x|
  (0...EX_HEIGHT).each do |y|
    if EXPANDED_MAP.includes?({x, y})
      canvas[x, y] = StumpyPNG::RGBA::WHITE
    elsif outside.includes?({x, y})
      canvas[x, y] = EXPANDED_POINTS.includes?({x, y}) ? StumpyPNG::RGBA::GRAY : StumpyPNG::RGBA::BLACK
    elsif EXPANDED_POINTS.includes?({x, y})
      canvas[x, y] = StumpyPNG::RGBA::CYAN
    else
      canvas[x, y] = StumpyPNG::RGBA::DARKBLUE
    end
  end
end
StumpyPNG.write(canvas, "day10b_visualization_2.png")
