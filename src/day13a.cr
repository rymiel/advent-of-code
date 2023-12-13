def is_mirror_point(left_index, right_index, max_index, & : Int32 -> Array(Char)) : Bool
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

def column(map : Array(String), column : Int32) : Array(Char)
  map.map { |line| line[column] }
end

def row(map : Array(String), row : Int32) : Array(Char)
  map[row].chars
end

def find_reflection(map : Array(String)) : Int32
  width = map.first.size

  (1...width).each do |i|
    if is_mirror_point(i - 1, i, width) { |j| column(map, j) }
      return i
    end
  end

  height = map.size
  (1...height).each do |i|
    if is_mirror_point(i - 1, i, height) { |j| row(map, j) }
      return i * 100
    end
  end

  0
end

map_fields = File.read("txt/day13").split("\n\n").map(&.strip).map(&.split "\n")

p map_fields.map { |map| find_reflection map }.sum
