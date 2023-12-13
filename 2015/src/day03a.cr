p File.read("txt/day3").strip.chars.accumulate({0, 0}) { |coord, char|
  case char
  when '>' then {coord[0] + 1, coord[1]}
  when '<' then {coord[0] - 1, coord[1]}
  when 'v' then {coord[0], coord[1] + 1}
  when '^' then {coord[0], coord[1] - 1}
  else          raise "invalid direction"
  end
}.uniq.size
