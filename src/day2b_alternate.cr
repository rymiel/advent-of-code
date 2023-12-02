# i just wanted to make a version that's "one line"
p File.read_lines("txt/day2").map(&.split(": ")[1].split("; ").map(&.split(", ").map(&.split(" ").reverse).to_h)).map { |bags| ["red", "green", "blue"].map { |c| bags.max_of(&.fetch(c, 0).to_i) }.product(1) }.sum
