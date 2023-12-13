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

start_point = MAP.find! &.[1].start
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

p cycle.size // 2

