packages = File.read_lines("txt/day24").map(&.to_i)

fourth = (packages.sum) // 4

valid_fourths = Set(Array(Int32)).new
(1..).each do |upto|
  packages.each_combination(upto, reuse: true) { |group|
    valid_fourths << group.sort if group.sum == fourth
  }
  break unless valid_fourths.empty?
end

p valid_fourths.min_of(&.map(&.to_u64).product)
