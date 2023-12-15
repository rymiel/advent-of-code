# Yes this is a brute force solution, because I'm lazy, and it runs surprisngly fast anyway
# Random upper bound
HOUSES = Array(Int32).new(1_000_000, 0)

# Random upper bound
(1..1_000_000).each do |i|
  c = i

  50.times do
    break if c >= 1_000_000
    HOUSES[c] += i * 11
    c += i
  end
end

need = File.read("txt/day20").strip.to_i
p HOUSES.index { |i| i >= need }
