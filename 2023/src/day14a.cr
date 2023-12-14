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
WIDTH  = LINES.first.size
HEIGHT = LINES.size

def segments(column : Int32, & : Range(Int32, Int32) ->)
  x = 0
  WALLS.each.select(&.[0].== column).map(&.[1]).each do |i|
    if (i != x + 1) && (i != x) && (i != HEIGHT)
      yield (x...i)
    end
    x = i + 1
  end

  if x < (HEIGHT)
    yield (x...HEIGHT)
  end
end

def rearrange_in_segment(column : Int32, segment : Range(Int32, Int32))
  ROCKS.select { |r| r[0] == column && r[1].in? segment }.each_with_index do |rock, i|
    ROCKS.delete rock
    ROCKS << {column, segment.begin + i}
  end
end

(0...WIDTH).each do |i|
  segments(i) { |j| rearrange_in_segment(i, j) }
end

p (0...HEIGHT).sum { |i|
  (HEIGHT - i) * (ROCKS.count &.[1].== i)
}

