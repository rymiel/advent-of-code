def valid_password?(str : String) : Bool
  return false if {'i', 'o', 'l'}.any? &.in? str
  return false unless str.matches? /(.)\1.*(.)\2/
  return false unless str.chars.each_cons(3).any? { |(a, b, c)|
                        (a.ord + 2) == (b.ord + 1) == c.ord
                      }

  return true
end

input = File.read("txt/day11").strip

p (((input..).find! { |i| valid_password? i }).succ..).find { |i| valid_password? i }
