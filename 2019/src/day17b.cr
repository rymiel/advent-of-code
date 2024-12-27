require "./intcode"
alias Coords = {Int32, Int32}

record Map, map : Array(Array(Char)) do
  def newline
    @map << [] of Char
  end

  def <<(x)
    @map.last << x
  end

  def [](x, y) : Char?
    @map[y]?.try &.[x]?
  end

  def [](pos : Coords) : Char?
    return self[*pos]
  end
end

map = Map.new Array(Array(Char)).new
map.newline

start_pos = {0, 0}

Program.new(read_program("txt/day17"),
  ->{ 0i64 },
  ->(o : Int64) {
    if o == 10
      map.newline
    else
      map << o.chr
      if o.chr.in?({'^', '>', 'v', '<'})
        start_pos = {map.map.last.size - 1, map.map.size - 1}
      end
    end
  }
).run

path = [] of String
running = 0

DIRS   = { {0, -1}, {1, 0}, {0, 1}, {-1, 0} }
FACING = {'^', '>', 'v', '<'}

def in_direction(pos : Coords, dir : Int32)
  {pos[0] + DIRS[dir][0], pos[1] + DIRS[dir][1]}
end

start_facing = FACING.index! map[start_pos]

pos = start_pos
dir = start_facing

loop do
  forward = in_direction(pos, dir)

  if map[forward] == '#'
    running += 1
    pos = forward
  else
    if running > 0
      path << running.to_s
    end
    running = 0

    right = in_direction(pos, (dir + 1) % 4)
    left = in_direction(pos, (dir - 1) % 4)

    if map[right] == '#'
      path << "R"
      dir = (dir + 1) % 4
    elsif map[left] == '#'
      path << "L"
      dir = (dir - 1) % 4
    else
      break
    end
  end
end

def indices_of_prefix(length, full)
  prefix = full[0...length]
  full.size.times.select do |start|
    prefix.each_with_index.all? do |c, offset|
      full[start + offset]? == c && !c.in?("A", "B", "C")
    end
  end.to_a
end

def reduce_subsets(path, replacement) : {Array(String), Array(Array(String))}?
  try_initial_length = 0
  try_max_length = 0
  offset = path.index! &.in?("A", "B", "C").!
  view = path[offset..]
  loop do
    try_initial_length += 1

    break if try_initial_length > (path.size - offset)

    prefix = view[0...try_initial_length]
    iop = indices_of_prefix(try_initial_length, view)

    break if iop.size < 1 || prefix.join(",").size > 20

    try_max_length = try_initial_length
  end

  try_length = try_max_length
  loop do
    return nil if try_length == 0
    try_indices = indices_of_prefix(try_length, view)
    reduced_path = path.dup
    try_indices.each_with_index do |raw_start, i|
      start = raw_start - (i * (try_length - 1)) + offset
      reduced_path[start...(start + try_length)] = replacement.to_s
    end

    repl = [] of Array(String)
    if replacement == 'C'
      final = reduced_path.all?(&.in?("A", "B", "C")) ? reduced_path : nil
    else
      result = reduce_subsets reduced_path, (replacement.ord + 1).chr
      if result
        final, repl = result
      end
    end

    return {final, [view[...try_length]] + repl} if final
    try_length -= 1
  end
end

x = reduce_subsets(path, 'A')
raise "Couldn't satisfy input" if x.nil?
robot_input = [x[0], *x[1]].map(&.join(",")).join("\n") + "\nn\n"
input_index = 0

program = read_program("txt/day17")
program[0] = 2
Program.new(program,
  ->{
    c = robot_input[input_index]
    input_index += 1
    c.ord.to_i64
  },
  ->(o : Int64) {
    puts o if o > 256
  }
).run
