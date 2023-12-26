def fuel(mass : Int32) : Int32
  f = (mass / 3).floor.to_i - 2
  return 0 if f <= 0
  f + fuel(f)
end

p File.read_lines("txt/day1").map { |i| fuel i.to_i }.sum
