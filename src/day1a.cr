puts STDIN.gets_to_end.strip.split("\n").map(&.chars.select(&.ascii_number?)).map { |i| "#{i.first}#{i.last}".to_i }.sum
