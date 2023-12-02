p File.read_lines("txt/day2").map { |i|
  num, game = i.lchop("Game ").split(": ")
  bags = game.split("; ").map { |j|
    j.split(", ").map { |k|
      count, color = k.split(" ")
      {color, count.to_i}
    }.to_h
  }
  {num.to_i, bags}
}.map { |(num, bags)|
  ["red", "green", "blue"].map { |c| bags.max_of(&.fetch(c, 0)) }.product(1)
}.sum

# here's my original version of lines 10-12, but i thought i'd make it spicier afterwards
# }.map { |(num, bags)|
# max_r = bags.max_of { |i| i["red"]? || 0 }
# max_g = bags.max_of { |i| i["green"]? || 0 }
# max_b = bags.max_of { |i| i["blue"]? || 0 }
# max_r * max_g * max_b
# }.sum
