p File.read("txt/day1").strip.chars.accumulate(0) { |i, c| c == '(' ? i + 1 : i - 1 }.index(-1)
