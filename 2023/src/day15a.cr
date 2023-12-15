def custom_hash(str : String) : Int32
  value = 0
  str.each_char do |c|
    value += c.ord
    value *= 17
    value = value % 256
  end

  value
end

p File.read("txt/day15").strip.split(",").map { |i| custom_hash i }.sum
