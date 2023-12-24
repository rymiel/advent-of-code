# once again, bad problems deserve bad solutions

hailstones = File.read_lines("txt/day24").map { |line|
  pos, dir = line.split(" @ ").map(&.split(", ").map &.to_f64)

  { {pos[0], pos[1]}, {dir[0], dir[1]} }
}

alias Vec = {Float64, Float64}
alias Ray = {Vec, Vec}

def ray_intersection(ray1, ray2) : Vec?
  p1, v1 = ray1
  p2, v2 = ray2

  t1 = ((p2[0] - p1[0]) * v2[1] - (p2[1] - p1[1]) * v2[0]) / (v1[0] * v2[1] - v1[1] * v2[0])
  t2 = ((p2[0] - p1[0]) * v1[1] - (p2[1] - p1[1]) * v1[0]) / (v1[0] * v2[1] - v1[1] * v2[0])

  if t1 >= 0 && t2 >= 0
    return ({p1[0] + t1 * v1[0], p1[1] + t1 * v1[1]})
  end
  return nil
end

BOUND = 200000000000000..400000000000000

p hailstones.each_combination(2, true).count { |(a, b)|
  ins = ray_intersection(a, b)
  next false if ins.nil?
  ins[0].in?(BOUND) && ins[1].in?(BOUND)
}
