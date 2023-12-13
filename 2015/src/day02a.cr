p File.read_lines("txt/day2").map { |line|
  w, l, h = line.split("x").map &.to_i
  area = (2 * l * w) + (2* w * h) + (2 * h * l)
  slack = {l * w, w * h, h * l}.min
  area + slack
}.sum
