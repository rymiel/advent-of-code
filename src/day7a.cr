def hand_type(tally : Hash(Char, Int32)) : Int32
  values = tally.values
  return 7 if tally.size == 1                # five of a kind
  return 6 if 4.in?(values)                  # four of a kind
  return 5 if 3.in?(values) && 2.in?(values) # full house
  return 4 if 3.in?(values)                  # three of a kind
  return 3 if values.count(2) == 2           # two pair
  return 2 if 2.in?(values)                  # pair
  return 1                                   # high card
end

LABELS = "23456789TJQKA"

p File.read_lines("txt/day7")
  .map(&.split(" "))
  .sort_by(&.[0].chars.map { |i| LABELS.index! i })
  .sort_by { |i| hand_type(i[0].chars.tally) }
  .map_with_index { |j, i| j[1].to_i * (i + 1) }
  .sum
