times, distances = File.read_lines("txt/day6").map(&.split(":")[1].split(" ", remove_empty: true).map &.to_i)
p times.zip(distances).map { |time, dist| (1...time).count { |i| (i * (time - i)) > dist } }.product
