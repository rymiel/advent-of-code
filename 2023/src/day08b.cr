order, _, *nodes = File.read_lines("txt/day8")
nodes = nodes.map { |node|
  key, value = node.split(" = ")
  left, right = value.strip("()").split(", ")
  {key, {left, right}}
}.to_h

p nodes.keys.select(&.ends_with? 'A').map { |start_point|
  order.each_char.map { |i| "LR".index!(i).to_u8! }.cycle
    .accumulate(start_point) { |location, choice| nodes[location][choice] }
    .take_while(&.ends_with?('Z').!)
    .size
}.reduce { |a, b| a.to_u64.lcm b.to_u64 }
