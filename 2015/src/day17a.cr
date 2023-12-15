CONTAINERS = File.read_lines("txt/day17").map(&.to_i)

p (1..CONTAINERS.size).each.flat_map { |i| CONTAINERS.each_combination(i) }.select(&.sum.== 150).size
