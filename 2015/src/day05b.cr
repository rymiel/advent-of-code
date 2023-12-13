def is_nice?(str : String) : Bool
  return false unless str.matches? /(..).*\1/
  return false unless str.matches? /(.).\1/

  true
end

p File.read_lines("txt/day5").count { |line| is_nice? line }
