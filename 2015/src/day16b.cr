SAMPLE = {
  "children"    => 3,
  "cats"        => (7 + 1..),
  "samoyeds"    => 2,
  "pomeranians" => (...3),
  "akitas"      => 0,
  "vizslas"     => 0,
  "goldfish"    => (...5),
  "trees"       => (3 + 1..),
  "cars"        => 2,
  "perfumes"    => 1,
}

p File.read_lines("txt/day16").map_with_index { |line, i|
  line.split(":", limit: 2)[1].strip.split(", ").map { |thing|
    name, count = thing.split(": ")
    {name, count.to_i}
  }.all? { |(thing, count)|
    SAMPLE[thing] === count
  }
}.index!(true) + 1
