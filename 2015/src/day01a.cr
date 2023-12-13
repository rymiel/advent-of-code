p File.read("txt/day1").strip.chars.reduce(0) { |i, c| c == '(' ? i + 1 : i - 1 }
