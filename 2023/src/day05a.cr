seed_section, *map_sections = File.read("txt/day5").split("\n\n").map(&.strip)
seed_list = seed_section.split(":")[1].strip.split(" ").map(&.to_u64)

maps = map_sections.map &.split("\n")[1..]
  .map { |i| Tuple(UInt64, UInt64, UInt64).from i.split(" ").map &.to_u64 }
  .map { |(dest, source, size)| {(source...(source + size)), dest} }

p seed_list.map { |seed|
  maps.reduce(seed) { |i, m|
    r = m.find &.[0].includes?(i)
    next i if r.nil?
    r[1] + (i - r[0].begin)
  }
}.min
