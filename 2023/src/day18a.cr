alias Coords = {Int32, Int32}

POINTS = Set(Coords).new

current = {0, 0}
POINTS << current

File.read_lines("txt/day18").each { |line|
  dir, steps, color = line.split(" ")

  steps.to_i.times do
    dx, dy = case dir
             when "R" then {1, 0}
             when "U" then {0, -1}
             when "D" then {0, 1}
             when "L" then {-1, 0}
             else          raise "invalid direction"
             end

    current = {current[0] + dx, current[1] + dy}
    POINTS << current
  end
}

MIN_X = POINTS.min_of &.[0]
MIN_Y = POINTS.min_of &.[1]
MAX_X = POINTS.max_of &.[0]
MAX_Y = POINTS.max_of &.[1]

# visualization
# (MIN_X-1..MAX_X+1).each do |x|
#   (MIN_Y-1..MAX_Y+1).each do |y|
#     print ({x, y}.in?(POINTS) ? '#' : '.')
#   end
#   puts
# end

DIRS = { {0, 1}, {0, -1}, {1, 0}, {-1, 0} }

def out_of_bounds(point : Coords)
  point[0] < (MIN_X - 2) || point[1] < (MIN_Y - 2) || point[0] > (MAX_X + 2) || point[1] > (MAX_Y + 2)
end

def spread_outside(outside : Set(Coords))
  (MIN_Y - 1..MAX_Y + 1).each do |y|
    (MIN_X - 1..MAX_X + 1).each do |x|
      next if POINTS.includes?({x, y})
      spread_to = DIRS.any? { |dir|
        neighbour = {x + dir[0], y + dir[1]}
        out_of_bounds(neighbour) || outside.includes? neighbour
      }
      outside << {x, y} if spread_to
    end
  end
  outside
end

outside = Set(Coords).new
outside << {MIN_X - 1, MIN_Y - 1}
previous_outside_count = outside.size
loop do
  spread_outside outside

  break if outside.size == previous_outside_count
  previous_outside_count = outside.size
end

# visualization
# (MIN_Y - 1..MAX_Y + 1).each do |y|
#   (MIN_X - 1..MAX_X + 1).each do |x|
#     print({x, y}.in?(outside) ? '.' : {x, y}.in?(POINTS) ? '#' : '@')
#   end
#   puts
# end

count = 0
(MIN_Y..MAX_Y).each do |y|
  (MIN_X..MAX_X).each do |x|
    count += 1 unless ({x, y}.in? outside)
  end
end
p count
