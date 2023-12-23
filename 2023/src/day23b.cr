alias Coords = {UInt8, UInt8}

PATH  = Set(Coords).new
LINES = File.read_lines("txt/day23")
LINES.map_with_index { |line, y|
  line.chars.map_with_index { |c, x|
    next if c == '#'
    PATH << {x.to_u8!, y.to_u8!}
  }
}

HEIGHT = LINES.size
WIDTH  = LINES.first.size

START = PATH.find!(&.[1].== 0)
END   = PATH.max_by(&.[1])

DIRS = { {1, 0}, {0, 1}, {-1, 0}, {0, -1} }

NODES = Set(Coords).new
NODES << START
NODES << END

def locate_intersections
  seen = Set(Coords).new
  queue = Deque(Coords).new
  queue << START
  until queue.empty?
    front = queue.shift
    seen << front
    neighbours = DIRS.map { |(dx, dy)| {front[0] &+ dx, front[1] &+ dy} }.select &.in? PATH
    if neighbours.size > 2
      NODES << front
    end
    queue.concat neighbours.reject(&.in? seen)
  end
end

def measure_distances(point : Coords, trail : Set(Coords), previous : Coords)
  count = 0
  curr = point
  found = nil
  loop do
    neighbours = DIRS
      .map { |(dx, dy)| {curr[0] + dx, curr[1] + dy} }
      .select { |i| i != previous && !i.in?(trail) && i.in? PATH }
    if neighbours.size > 1
      raise "Unexpected intersection"
    elsif neighbours.size == 0
      return
    end
    n = neighbours.first
    if n.in? NODES
      found = n
      break
    end
    trail << curr
    count += 1
    curr = n
  end

  found = found.not_nil!
  DISTANCES[{previous, found}] = count + 2

  neighbours = DIRS
    .map { |(dx, dy)| {found[0] + dx, found[1] + dy} }
    .select { |i| i != previous && !i.in?(trail) && i.in? PATH }
  neighbours.each do |n|
    measure_distances(n, trail, found)
  end
end

def distance(a : Coords, b : Coords) : Int32
  DISTANCES[{a, b}]? || DISTANCES[{b, a}]
end

def traverse : Int32
  queue = Deque({Coords, Array(Coords)}).new
  queue << {START, [] of Coords}
  max = 0
  until queue.empty?
    # i = queue.shift
    # queue.delete i
    # front, trail, d = i
    front, trail = queue.shift

    if front == END
      trail << END
      d = trail.each_cons_pair.sum { |a, b| distance(a, b) }
      max = d if d > max
      next
    end

    neighbours = NEIGHBOURS[front] - trail
    trail << front
    neighbours.each do |n|
      queue << {n, trail.dup}
    end
  end

  max
end

locate_intersections

DISTANCES  = Hash({Coords, Coords}, Int32).new
NEIGHBOURS = Hash(Coords, Array(Coords)).new { |hash, key| hash[key] = Array(Coords).new }

measure_distances({1u8, 1u8}, Set(Coords).new, START)
DISTANCES.each_key do |(a, b)|
  NEIGHBOURS[a] << b
  NEIGHBOURS[b] << a
end
p traverse
