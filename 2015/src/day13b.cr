ACTORS = Set(String).new
EFFECTS = Hash({String, String}, Int32).new

File.read_lines("txt/day13").map { |line|
  actor, rest = line.rchop('.').split(" would ")
  negative = rest.starts_with? "lose"
  rest = rest.lchop("gain ").lchop("lose ")
  amount, target = rest.split(" happiness units by sitting next to ")
  amount = negative ? -(amount.to_i) : amount.to_i

  ACTORS << actor << target
  EFFECTS[{actor, target}] = amount
}

ACTORS.each do |pairing|
  EFFECTS[{"I", pairing}] = 0
  EFFECTS[{pairing, "I"}] = 0
end
ACTORS << "I"

p ACTORS.to_a.permutations(ACTORS.size).max_of { |arrangement|
  arrangement << arrangement.first
  arrangement.each_cons_pair.sum { |(a, b)| EFFECTS[{a, b}] + EFFECTS[{b, a}] }
}

