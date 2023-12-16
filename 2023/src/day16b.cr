alias Coords = {Int32, Int32}

enum Side
  North
  South
  East
  West

  def to_vector : Coords
    case self
    in .north? then {0, -1}
    in .south? then {0, 1}
    in .east?  then {1, 0}
    in .west?  then {-1, 0}
    end
  end

  def +(other : Coords) : Coords
    {to_vector[0] + other[0], to_vector[1] + other[1]}
  end
end

enum Space
  RightSlash
  LeftSlash
  VerticalSplitter
  HorizontalSplitter

  def collide(dir : Side) : {Side, Side?}
    case self
    in .right_slash? then {[Side::East, Side::West, Side::North, Side::South][dir.to_i], nil}
    in .left_slash?  then {[Side::West, Side::East, Side::South, Side::North][dir.to_i], nil}
    in .vertical_splitter?
      return {dir, nil} if dir.north? || dir.south?
      {Side::North, Side::South}
    in .horizontal_splitter?
      return {dir, nil} if dir.west? || dir.east?
      {Side::West, Side::East}
    end
  end
end

alias Heading = {Coords, Side}

MAP = {} of Coords => Space

LINES = File.read_lines("txt/day16")
LINES.each_with_index do |line, y|
  line.each_char_with_index do |c, x|
    tile = case c
           when '/'  then Space::RightSlash
           when '\\' then Space::LeftSlash
           when '-'  then Space::HorizontalSplitter
           when '|'  then Space::VerticalSplitter
           end
    MAP[{x, y}] = tile if tile
  end
end
WIDTH = LINES.size

SEEN      = Set(Heading).new
ENERGIZED = Set(Coords).new
FRONT     = Set(Heading).new

def out_of_bounds(loc : Coords) : Bool
  loc[0] < 0 || loc[1] < 0 || loc[0] >= WIDTH || loc[1] >= WIDTH
end

def iterate
  return false if FRONT.empty?

  new_front = [] of Heading
  FRONT.each do |(loc, dir)|
    space = MAP[loc]?
    if space.nil?
      new_front << {dir + loc, dir}
    else
      next1, next2 = space.collide dir
      new_front << {next1 + loc, next1}
      if next2
        new_front << {next2 + loc, next2}
      end
    end
  end

  new_front.reject! { |i| out_of_bounds(i[0]) || i.in? SEEN }

  FRONT.clear.concat new_front
  SEEN.concat new_front
  ENERGIZED.concat new_front.map &.[0]

  true
end

def energized_entering_at(loc : Coords, dir : Side) : Int32
  FRONT.clear << {loc, dir}
  SEEN.clear << {loc, dir}
  ENERGIZED.clear << loc

  while iterate
  end

  ENERGIZED.size
end

all_entry_points = {Side::South, Side::East, Side::North, Side::West}.each.flat_map do |dir|
  (0...WIDTH).map { |i|
    start_point = case dir
                  in .south? then {i, 0}
                  in .east?  then {0, i}
                  in .north? then {i, WIDTH - 1}
                  in .west?  then {WIDTH - 1, i}
                  end

    {start_point, dir}
  }
end

p all_entry_points.max_of { |(loc, dir)| energized_entering_at loc, dir }
