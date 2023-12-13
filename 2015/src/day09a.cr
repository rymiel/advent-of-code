ROUTES    = {} of {String, String} => Int32
LOCATIONS = Set(String).new

def route(a : String, b : String) : Int32
  ROUTES[{a, b}]? || ROUTES[{b, a}]
end

File.read_lines("txt/day9").each do |line|
  route, distance = line.split(" = ")
  distance = distance.to_i
  src, dst = route.split(" to ")
  LOCATIONS << src << dst
  ROUTES[{src, dst}] = distance
end

p LOCATIONS.to_a.permutations(LOCATIONS.size).min_of &.each_cons_pair.sum { |(a, b)| route(a, b) }
