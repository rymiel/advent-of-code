p File.read_lines("txt/day1").map(&.chars.select(&.ascii_number?)).map { |i| "#{i.first}#{i.last}".to_i }.sum
