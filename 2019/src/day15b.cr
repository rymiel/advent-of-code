require "./intcode"

alias Coords = {Int32, Int32}
DIRS = { {0, -1}, {0, 1}, {-1, 0}, {1, 0} }
position = {0, 0}
pending_pos = {0, 0}
target = nil
walls = Set(Coords).new

traversed = Set(Coords).new
dead_end = Set(Coords).new

def in_direction(pos : Coords, dir : Int32)
  {pos[0] + DIRS[dir - 1][0], pos[1] + DIRS[dir - 1][1]}
end

cpu = Program.new(read_program("txt/day15"),
  ->{
    calc_dirs = ->{
      og_dirs = (1i64..4i64).map { |i| {in_direction(position, i.to_i32), i} }
      dirs = og_dirs.reject { |(pos, dir)| pos.in?(walls) || pos.in?(traversed) || pos.in?(dead_end) }
      {og_dirs, dirs}
    }

    original_dirs, dirs = calc_dirs.call
    if dirs.empty?
      dead_end << position
      traversed.clear

      original_dirs, dirs = calc_dirs.call
    elsif dirs.size == 1
      # Close up dead ends behind us
      if (original_dirs.count(&.[0].in?(dead_end)) + original_dirs.count(&.[0].in?(walls))) == 3
        dead_end << position
      end
    end

    if dirs.empty?
      dead_end << position
      return 0i64 # Forceful termination
    end

    # Yes, it's literally random
    selected = dirs.shuffle.first
    pending_pos = selected[0]
    dir = selected[1]
    dir
  },
  ->(o : Int64) {
    case o
    when 0 then walls << pending_pos
    when 1
      traversed << position
      position = pending_pos
    when 2
      traversed << position
      position = target = pending_pos
    end
  }
).run

def flood_fill(start : Coords, path : Set(Coords))
  filled = Set(Coords).new
  filled << start
  next_filled = Set(Coords).new
  steps = 0

  until filled.size == path.size
    steps += 1
    filled.each do |pos|
      DIRS.each do |dir|
        n = {pos[0] + dir[0], pos[1] + dir[1]}
        next unless n.in? path

        next_filled << n
      end
    end

    filled = next_filled
  end

  return steps
end

p flood_fill(target.not_nil!, dead_end)
