p File.read_lines("txt/day4").map(&.split(':')[1].split('|').map(&.strip)).map { |(win, actual)|
  win = win.split(" ", remove_empty: true).map(&.strip.to_i)
  actual = actual.split(" ", remove_empty: true).map(&.strip.to_i)
  matches = actual.count(&.in? win)
  points = (matches.zero? ? 0 : 2 ** (matches - 1))
  points
}.sum
