seed_section, *map_sections = File.read("txt/day5").split("\n\n").map(&.strip)
seed_list = seed_section.split(":")[1].strip.split(" ").map(&.to_u64)
  .in_slices_of(2).map { |i| Tuple(UInt64, UInt64).from i }

maps = map_sections.map &.split("\n")[1..]
  .map { |i| Tuple(UInt64, UInt64, UInt64).from i.split(" ").map &.to_u64 }
  .map { |(dest, source, size)| {(source...(source + size)), dest} }

ch = Channel(UInt64).new

puts "Launching #{seed_list.size} jobs"
seed_list.each { |(seed_start, seed_count)|
  spawn do
    # brute-force
    ch.send (seed_start..(seed_start + seed_count)).min_of { |seed_i|
      maps.reduce(seed_i) { |i, m|
        r = m.find &.[0].includes?(i)
        next i if r.nil?
        r[1] + (i - r[0].begin)
      }
    }
  end
}

responses = [] of UInt64
seed_list.size.times do |count|
  puts "Waiting for result #{count}"
  responses << ch.receive
end

p responses.min
