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

REVERSE_DIRECTIONS = [
  {:n, 0, 1},
  {:s, 0, -1},
  {:e, -1, 0},
  {:w, 1, 0},
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
  REVERSE_DIRECTIONS.compact_map do |(dir, x_diff, y_diff)|
    adjacent_coords = {coords[0] + x_diff, coords[1] + y_diff}
    next nil unless MAP[adjacent_coords]?.try &.[dir]
    next nil unless MAP[coords][OPPOSITE_DIR[dir]]
    adjacent_coords
  end
end

start_point = MAP.find! &.[1].start.== true
neighbours = find_neighbours start_point[0]

cycle = [] of Coords

previous = start_point[0]
traversing = neighbours.first
endpoint = neighbours.last
cycle << traversing

until traversing == endpoint
  next_neighbours = find_neighbours(traversing)
  next_traversing = next_neighbours.reject(previous).first
  previous = traversing
  traversing = next_traversing
  cycle << traversing
end

cycle << endpoint

def generate_rainbow_colors(num_steps)
  raise "Number of steps must be greater than or equal to 1" if num_steps < 1

  colors = [] of {Int32, Int32, Int32}
  step_size = 320.0 / num_steps

  (0...num_steps).each do |i|
    hue = i * step_size
    rgb = hsl_to_rgb(hue, 1, 0.5)
    colors << rgb
  end

  colors
end

def hue_to_rgb(p : Float64, q : Float64, t : Float64)
  t += 1 if t < 0
  t -= 1 if t > 1

  return p + (q - p) * 6 * t if t < 1 / 6.0
  return q if t < 1 / 2.0
  return p + (q - p) * (2 / 3.0 - t) * 6 if t < 2 / 3.0

  p
end

def hsl_to_rgb(h, s, l)
  h /= 360.0
  s /= 1.0
  l /= 1.0

  if s.zero?
    r = g = b = l
  else
    q = l < 0.5 ? l * (1 + s) : l + s - l * s
    p = 2 * l - q

    r = hue_to_rgb(p, q, h + 1 / 3.0)
    g = hue_to_rgb(p, q, h)
    b = hue_to_rgb(p, q, h - 1 / 3.0)
  end

  {(r * 255).to_i, (g * 255).to_i, (b * 255).to_i}
end

rainbow = generate_rainbow_colors(cycle.size)

# original text-based visualization
# File.read_lines("txt/day10").map_with_index do |line, y|
#   line.each_char_with_index do |char, x|
#     if index = cycle.index({x, y})
#       r, g, b = rainbow[index]
#       print "\e[38;2;#{r};#{g};#{b}m"
#     end
#     if {x, y} == start_point[0]
#       print "\e[7m"
#     end
#     print char
#     print "\e[0m"
#   end
#   puts
# end

# New image-based visualization
WIDTH  = MAP.keys.max_of(&.[0]) + 1
HEIGHT = MAP.keys.max_of(&.[1]) + 1
canvas = StumpyPNG::Canvas.new(WIDTH, HEIGHT)
(0...WIDTH).each do |x|
  (0...HEIGHT).each do |y|
    if {x, y} == start_point[0]
      canvas[x, y] = StumpyPNG::RGBA::WHITE
    elsif index = cycle.index({x, y})
      canvas[x, y] = StumpyPNG::RGBA.from_rgb rainbow[index]
    else
      canvas[x, y] = StumpyPNG::RGBA::BLACK
    end
  end
end
StumpyPNG.write(canvas, "day10a_visualization_1.png")
