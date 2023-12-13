order, _, *nodes = File.read_lines("txt/day8")
nodes = nodes.map { |node|
  key, value = node.split(" = ")
  left, right = value.strip("()").split(", ")
  {key, {left, right}}
}.to_h

p order.each_char.map { |i| "LR".index!(i).to_u8! }.cycle
  .accumulate("AAA") { |location, choice| nodes[location][choice] }
  .take_while { |location| location != "ZZZ" }
  .size
