p File.read_lines("txt/day2").map { |i|
  num, game = i.lchop("Game ").split(": ")
  bags = game.split("; ").map { |j|
    j.split(", ").map { |k|
      count, color = k.split(" ")
      {color, count.to_i}
    }.to_h
  }
  {num.to_i, bags}
}.select { |(num, bags)|
  bags.all? { |bag| ((bag["red"]? || 0) <= 12) && ((bag["green"]? || 0) <= 13) && ((bag["blue"]? || 0) <= 14) }
}.sum(&.[0])
