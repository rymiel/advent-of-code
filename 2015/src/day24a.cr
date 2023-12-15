packages = File.read_lines("txt/day24").map(&.to_i)

third = (packages.sum) // 3

valid_thirds = Set(Array(Int32)).new
(1..).each do |upto|
  packages.each_combination(upto, reuse: true) { |group|
    valid_thirds << group.sort if group.sum == third
  }
  break unless valid_thirds.empty?
end

p valid_thirds.min_of(&.map(&.to_u64).product)
