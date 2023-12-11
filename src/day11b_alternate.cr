# An alternate solution that uses transpose to avoid code duplication

alias Coords = {Int64, Int64}

class GalaxyMap
  EXPANSION_FACTOR = 1_000_000 - 1

  @map = Set(Coords).new

  def <<(coords : Coords)
    @map << coords
  end

  def width
    @map.max_of &.[0]
  end

  def column(index : Int32)
    @map.each.select(&.[0].== index)
  end

  def expand_empty_columns
    i = 0
    until i >= width
      if column(i).empty?
        @map.select(&.[0].> i).each do |j|
          @map.delete j
          @map.add({j[0] + EXPANSION_FACTOR, j[1]})
        end
        i += EXPANSION_FACTOR
      end
      i += 1
    end
  end

  def self.distance(a : Coords, b : Coords) : Int64
    (a[0] - b[0]).abs + (a[1] - b[1]).abs
  end

  def all_distances
    @map.to_a
      .combinations(2)
      .map { |(a, b)| self.class.distance a, b }
      .sum
  end

  def transpose
    @map = @map.map { |(x, y)| {y, x} }.to_set
  end
end

MAP = GalaxyMap.new
File.read_lines("txt/day11").each_with_index do |line, y|
  line.each_char_with_index do |char, x|
    MAP << {x.to_i64, y.to_i64} if char == '#'
  end
end

MAP.expand_empty_columns
MAP.transpose
MAP.expand_empty_columns

p MAP.all_distances
