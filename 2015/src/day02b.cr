p File.read_lines("txt/day2").map { |line|
  w, l, h = line.split("x").map &.to_i
  ribbon = {w, l, h}.min(2).sum * 2
  bow = w * l * h
  ribbon + bow
}.sum
