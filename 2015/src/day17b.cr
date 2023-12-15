CONTAINERS = File.read_lines("txt/day17").map(&.to_i)

correct = (1..CONTAINERS.size).each.flat_map { |i| CONTAINERS.each_combination(i) }.select(&.sum.== 150).to_a
min_correct = correct.min_of(&.size)
p correct.select(&.size.== min_correct).size
