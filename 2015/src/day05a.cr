def is_nice?(str : String) : Bool
  return false if "ab".in?(str) || "cd".in?(str) || "pq".in?(str) || "xy".in?(str)
  return false unless str.chars.select(&.in?({'a', 'e', 'i', 'o', 'u'})).size >= 3
  return false unless str.chars.each_cons_pair.any? { |a, b| a == b }

  true
end

p File.read_lines("txt/day5").count { |line| is_nice? line }
