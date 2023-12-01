# keep the end of the word in place for overlapping numbers
NUMBERS = {
  "one"   => "1ne",
  "two"   => "2wo",
  "three" => "3hree",
  "four"  => "4our",
  "five"  => "5ive",
  "six"   => "6ix",
  "seven" => "7even",
  "eight" => "8ight",
  "nine"  => "9ine",
}
puts STDIN.gets_to_end.strip.split("\n").map { |i|
  # perform twice for overlapping numbers ("eighthree")
  i.gsub(Regex.union(NUMBERS.keys), NUMBERS).gsub(Regex.union(NUMBERS.keys), NUMBERS)
}.map(&.chars.select(&.ascii_number?)).map { |i| "#{i.first}#{i.last}".to_i }.sum
