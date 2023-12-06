time, dist = File.read_lines("txt/day6").map(&.split(":")[1].delete(' ').to_u64)
p (1u64...time).count { |i| (i * (time - i)) > dist }
