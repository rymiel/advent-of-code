DIRS = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 0}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]

chars = File.read("txt/day3").strip
map = chars.split("\n").map_with_index { |line, x| line.chars.map_with_index { |c, y| { {x, y}, c } } }.flatten.to_h

chars = chars.delete('\n')

p chars.chars.map_with_index { |c, i|
  next 0 if i > 0 && chars[i - 1].ascii_number?

  endnum = chars[i..].chars.each_with_index.find(&.[0].ascii_number?.!).try &.[1]
  next 0 if endnum.nil? || endnum == 0

  number = chars[i...(i + endnum)]
  coords = map.keys[i...(i + endnum)]

  has_symbol_neighbour = coords.any? { |(x, y)|
    DIRS.any? { |(dir_x, dir_y)|
      symbol = map.fetch({x + dir_x, y + dir_y}, '.')
      !symbol.ascii_number? && symbol != '.'
    }
  }
  next 0 unless has_symbol_neighbour
  number.to_i
}.sum

