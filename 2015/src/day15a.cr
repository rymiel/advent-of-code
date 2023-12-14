INGREDIENTS = Array(String).new
PROPERTIES  = Hash(String, Array(Int32)).new

File.read_lines("txt/day15").each { |line|
  name, _ = line.split(":")
  *properties, calories = line.scan(/-?\d+/).map &.[0].to_i
  INGREDIENTS << name
  PROPERTIES[name] = properties
}

properties_indices = (0u8...INGREDIENTS.size.to_u8!).to_a

# For some reason each_repeated_combination that return an Iterator simply doesn't work, so I have to manually do a
# max using the block overload
max = 0
properties_indices.each_repeated_combination(100) { |i|
  result = i.reduce([0, 0, 0, 0]) { |a, j|
    b = PROPERTIES[INGREDIENTS[j]]
    [a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3]]
  }.map { |j| Math.max(j, 0) }.product
  max = result if result > max
}

p max
