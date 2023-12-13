p File.read_lines("txt/day9").map { |line|
  levels = [] of Array(Int64)
  levels << line.split(" ").map(&.to_i64)
  until levels.last.all?(&.zero?)
    levels << levels.last.each_cons_pair.map { |a, b| b - a }.to_a
  end
  levels.map(&.last).sum
}.sum
