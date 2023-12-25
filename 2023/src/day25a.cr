alias Wire = {String, String}
WIRES = Array(Wire).new
NODES = Set(String).new

File.read_lines("txt/day25").each do |line|
  src, rest = line.split(": ")
  dsts = rest.split(" ")
  dsts.each do |dst|
    WIRES << {src, dst}
    NODES << src << dst
  end
end

class Karger
  getter wires : Array(Wire) = Array(Wire).new
  getter nodes : Set(String) = Set(String).new
  getter counter = 0
  getter contractions = Hash(String, Int32).new

  def reset
    @wires = @initial_wires.dup
    @nodes = @initial_nodes.dup
    @count = 0
    @contractions.clear
  end

  def initialize(@initial_wires : Array(Wire), @initial_nodes : Set(String))
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
    "A" + @counter.to_s(base: 36)
  end

  def contract
    edge_num = Random.rand(0...wires.size)
    edge = wires[edge_num]
    name = next_name
    nodes.delete edge[0]
    nodes.delete edge[1]

    contractions[name] = (contractions[edge[0]]? || 1) + (contractions[edge[1]]? || 1)
    contractions.delete edge[0]
    contractions.delete edge[1]

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
