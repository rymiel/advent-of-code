
STRING_WIRES = Array({String, String}).new
STRING_NODES = Set(String).new

File.read_lines("txt/day25").each do |line|
  src, rest = line.split(": ")
  dsts = rest.split(" ")
  dsts.each do |dst|
    STRING_WIRES << {src, dst}
    STRING_NODES << src << dst
  end
end

INDEX = Array(String).new
INDEX.concat STRING_NODES

alias Wire = {Int32, Int32}
WIRES = Array(Wire).new
NODES = Set(Int32).new
STRING_WIRES.each do |(a, b)|
  WIRES << {INDEX.index!(a), INDEX.index!(b)}
end
STRING_NODES.each do |i|
  NODES << INDEX.index!(i)
end

class Karger
  getter wires : Array(Wire) = Array(Wire).new
  getter nodes : Set(Int32) = Set(Int32).new
  getter counter = 0
  getter contractions = Hash(Int32, Int32).new

  def reset
    @wires = @initial_wires.dup
    @nodes = @initial_nodes.dup
    @counter = @nodes.size + 1
    @contractions.clear
  end

  def initialize(@initial_wires : Array(Wire), @initial_nodes : Set(Int32))
  end

  def run
    loop do
      break if attempt
    end
    contractions.values.product
  end

  def attempt
    reset
    while nodes.size > 2
      contract
    end
    return wires.size == 3
  end

  def next_name
    @counter += 1
    @counter
  end

  def contract
    edge = wires[Random.rand(0...wires.size)]
    name = next_name
    nodes.delete edge[0]
    nodes.delete edge[1]

    contractions[name] = (contractions.delete(edge[0]) || 1) + (contractions.delete(edge[1]) || 1)

    wires.reject! { |i| i == edge || i.reverse == edge }
    wires.map! { |i|
      left = i[0] == edge[0] || i[0] == edge[1]
      right = i[1] == edge[0] || i[1] == edge[1]
      if left && right
        raise "Should have been removed"
      elsif left
        {name, i[1]}
      elsif right
        {i[0], name}
      else
        i
      end
    }
    nodes << name
  end
end

p Karger.new(WIRES, NODES).run
