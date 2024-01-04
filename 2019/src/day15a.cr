require "./intcode"

alias Coords = {Int32, Int32}
DIRS = { {0, -1}, {0, 1}, {-1, 0}, {1, 0} }
position = {0, 0}
pending_pos = {0, 0}
target = nil
walls = Set(Coords).new

cpu = Program.new(read_program("txt/day15"),
  ->{
    return 0i64 unless target.nil? # Forceful termination
    # Yes, it's literally random
    dir = Random.rand(1i64..4i64)
    pending_pos = {position[0] + DIRS[dir - 1][0], position[1] + DIRS[dir - 1][1]}
    dir
  },
  ->(o : Int64) {
    case o
    when 0 then walls << pending_pos
    when 1 then position = pending_pos
    when 2 then position = target = pending_pos
    end
  }
).run

def flood_fill(start : Coords, target : Coords, walls : Set(Coords))
  queue = Set({Coords, Int32}).new
  queue << {start, 0}

  min_x, max_x = walls.minmax_of(&.[0])
  min_y, max_y = walls.minmax_of(&.[1])

  until queue.empty?
    front = queue.min_by(&.[1])
    queue.delete front

    pos, steps = front

    if pos == target
      return steps
    end

    DIRS.each do |dir|
      n = {pos[0] + dir[0], pos[1] + dir[1]}
      next if n[0] < min_x || n[0] > max_x || n[1] < min_y || n[1] > max_y
      next if n.in? walls

      queue << {n, steps + 1}
    end
  end
end

p flood_fill({0, 0}, target.not_nil!, walls)
