alias Coords = {Int32, Int32}

ROCKS = Set(Coords).new
WALLS = Set(Coords).new

LINES = File.read_lines("txt/day14")
LINES.each_with_index { |line, y|
  line.each_char_with_index { |c, x|
    ROCKS << {x, y} if c == 'O'
    WALLS << {x, y} if c == '#'
  }
}
SIZE = LINES.size

def segments(column : Int32, & : Range(Int32, Int32) ->)
  x = 0
  WALLS.each.select(&.[0].== column).map(&.[1]).each do |i|
    if (i != x + 1) && (i != x) && (i != SIZE)
      yield (x...i)
    end
    x = i + 1
  end

  if x < SIZE
    yield (x...SIZE)
  end
end

def rearrange_in_segment(column : Int32, segment : Range(Int32, Int32))
  ROCKS.select { |r| r[0] == column && r[1].in? segment }.each_with_index do |rock, i|
    ROCKS.delete rock
    ROCKS << {column, segment.begin + i}
  end
end

def rotate_clockwise
  new_rocks = Array(Coords).new
  ROCKS.to_a.each do |(x, y)|
    new_rocks << {SIZE - y - 1, x}
  end
  new_walls = Array(Coords).new
  WALLS.to_a.each do |(x, y)|
    new_walls << {SIZE - y - 1, x}
  end

  ROCKS.clear.concat(new_rocks.sort!)
  WALLS.clear.concat(new_walls.sort!)
end

def spin_cycle
  4.times do |j|
    (0...SIZE).each do |i|
      segments(i) { |j| rearrange_in_segment(i, j) }
    end
    rotate_clockwise
  end
end

PREVIOUS_STATES = Set({Set(Coords), Set(Coords)}).new

def repeats_after
  PREVIOUS_STATES.clear
  (0..).find! do |i|
    spin_cycle

    if PREVIOUS_STATES.includes?({ROCKS, WALLS})
      next true
    end
    PREVIOUS_STATES << {ROCKS.dup, WALLS.dup}
    false
  end
end

TARGET = 1_000_000_000

def load
  (0...SIZE).sum { |i|
    (SIZE - i) * (ROCKS.count &.[1].== i)
  }
end

first_loop = repeats_after # Don't know how many cycles until entering loop from startpoint
second_loop = repeats_after
so_far_at = first_loop + second_loop + 2
to_go = TARGET - so_far_at
can_skip = (to_go // second_loop) * second_loop
left_to_simulate = to_go - can_skip

left_to_simulate.times { spin_cycle }

p load
