alias Coords = {Int64, Int64}

CORNERS = Set(Coords).new

current = {0i64, 0i64}
CORNERS << current

length = 0

File.read_lines("txt/day18").each { |line|
  color = line.split(" ")[2].lchop('(').rchop(')')

  steps = color[1..5].to_i(16)
  dir = color[6]

  dx, dy = case dir
           when '0' then {1, 0}
           when '1' then {0, 1}
           when '2' then {-1, 0}
           when '3' then {0, -1}
           else          raise "invalid direction"
           end

  length += steps
  current = {current[0] + (dx * steps), current[1] + (dy * steps)}
  CORNERS << current
}

CORNERS << {0i64, 0i64}

MIN_X = CORNERS.min_of &.[0]
MIN_Y = CORNERS.min_of &.[1]
MAX_X = CORNERS.max_of &.[0]
MAX_Y = CORNERS.max_of &.[1]

C = CORNERS.to_a
N = C.size

area = 0i64
(0..(N - 1)).each do |i|
  j = (i + 1) % N
  area = area + C[i][0] * C[j][1]
  area = area - C[i][1] * C[j][0]
end
area //= 2

wtf = area - (length // 2) + 1
p wtf + length
