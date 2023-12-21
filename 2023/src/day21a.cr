alias Coords = {Int32, Int32}

class StepMap
  getter height : Int32
  getter width : Int32
  getter stones = Set(Coords).new
  getter start_point : Coords
  getter cache = Hash(Coords, Slice(Coords)).new
  getter front = Set(Coords).new

  def initialize
    lines = File.read_lines("txt/day21")
    @height = lines.size
    @width = lines.first.size
    start_point = nil
    lines.each_with_index do |line, y|
      line.each_char_with_index do |c, x|
        @stones << {x, y} if c == '#'
        start_point = {x, y} if c == 'S'
      end
    end
    @start_point = start_point.not_nil!
    @front << @start_point
  end

  def out_of_bounds(point : Coords)
    point[0] < 0 || point[1] < 0 || point[0] >= @width || point[1] >= @height
  end

  def enqueue(points : Slice(Coords))
    @front.concat points
  end

  def step(point : Coords) : Slice(Coords)
    if cached = @cache[point]?
      return cached
    end
    x, y = point

    neighbours = Array(Coords).new 4
    { {-1, 0}, {1, 0}, {0, -1}, {0, 1} }.each do |(dx, dy)|
      d = {x + dx, y + dy}
      next if out_of_bounds(d)
      next if d.in? @stones
      neighbours << d
    end

    slice = Slice(Coords).new(neighbours.size) { |i| neighbours[i] }
    @cache[point] = slice

    return slice
  end

  def step
    queue = @front.to_a
    @front.clear
    queue.each do |point|
      enqueue step point
    end
  end
end

map = StepMap.new
64.times { map.step }
p map.front.size
