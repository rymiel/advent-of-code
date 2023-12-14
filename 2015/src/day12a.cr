# who needs json parsing when you have regex

p File.read("txt/day12").scan(/-?\d+/).map { |i| i[0].to_i }.sum
