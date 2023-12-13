p File.read("txt/day3").strip.chars.accumulate({ {0, 0}, {0, 0} }) { |(coord, robo_coord), char|
  new_coord = case char
  when '>' then {coord[0] + 1, coord[1]}
  when '<' then {coord[0] - 1, coord[1]}
  when 'v' then {coord[0], coord[1] + 1}
  when '^' then {coord[0], coord[1] - 1}
  else          raise "invalid direction"
  end
  {robo_coord, new_coord}
}.map(&.to_a).flatten.uniq.size
