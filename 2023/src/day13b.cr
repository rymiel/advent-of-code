class VolcanoMap
  @map : Array(Bytes)

  def initialize(@map)
  end

  def is_mirror_point(left_index, right_index, max_index, & : Int32 -> Array(UInt8)) : Bool
    left = yield left_index
    right = yield right_index
    return false if left != right

    until left_index <= 0 || right_index >= (max_index - 1)
      left_index -= 1
      right_index += 1
      left = yield left_index
      right = yield right_index
      return false if left != right
    end

    true
  end

  def column(column : Int32) : Array(UInt8)
    @map.map { |line| line[column] }
  end

  def row(row : Int32) : Array(UInt8)
    @map[row].to_a
  end

  def width
    @map.first.size
  end

  def height
    @map.size
  end

  def find_reflection(*, ignore = 0) : Int32
    (1...width).each do |i|
      if is_mirror_point(i - 1, i, width) { |j| column j }
        return i unless i == ignore
      end
    end

    (1...height).each do |i|
      if is_mirror_point(i - 1, i, height) { |j| row j }
        return i * 100 unless (i * 100) == ignore
      end
    end

    0
  end

  def each_smudge : Int32
    unsmudged_result = find_reflection
    (0...height).each do |y|
      (0...width).each do |x|
        map_copy = @map.dup
        line_copy = map_copy[y].dup
        char_at = line_copy[x]
        line_copy[x] = char_at == '.'.ord.to_u8! ? '#'.ord.to_u8! : '.'.ord.to_u8!
        map_copy[y] = line_copy

        smudged_map = VolcanoMap.new map_copy
        smudged_result = smudged_map.find_reflection ignore: unsmudged_result

        return smudged_result unless smudged_result.zero?
      end
    end

    return 0
  end

  def print(io : IO = STDOUT)
    @map.each do |line|
      io.puts String.new line
    end
  end
end

map_fields = File.read("txt/day13").split("\n\n").map(&.strip).map(&.split("\n").map &.to_slice)

p map_fields.map { |map| m = VolcanoMap.new(map).each_smudge }.sum
