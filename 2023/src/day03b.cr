DIRS = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 0}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]

chars = File.read("txt/day3").strip
map = chars.split("\n").map_with_index { |line, x| line.chars.map_with_index { |c, y| { {x, y}, c } } }.flatten.to_h
keys = map.keys

chars = chars.delete('\n').chars

gears = map.select { |_, i| i == '*' }.map { |i, _| {i, [] of Int32} }.to_h

chars.each_with_index { |c, i|
  next if i > 0 && chars[i - 1].ascii_number?

  endnum = chars[i..].each_with_index.find(&.[0].ascii_number?.!).try &.[1]
  next if endnum.nil? || endnum == 0

  number = chars[i...(i + endnum)]
  coords = keys[i...(i + endnum)]

  # I don't know if a number can be a part of multiple "gears" but I'm gonna write this assuming it can
  gears.keys.select { |(x, y)|
    DIRS.any? { |(dir_x, dir_y)| {x + dir_x, y + dir_y}.in? coords }
  }.each { |gear|
    gears[gear] << number.join.to_i
  }
}

p gears.values.select(&.size.== 2).map(&.product).sum
