def rotate_right(dir)
  x, y = dir
  x.zero? ? {-y, -x} : {y, x}
end

def rotate_left(dir)
  x, y = dir
  y.zero? ? {-y, -x} : {y, x}
end

pos = {0, 0}
dir = {1, 0}

SEEN = Set({Int32, Int32}).new

File.read("txt/day1").strip.split(", ").each { |instruction|
  is_right = instruction[0] == 'R'
  steps = instruction[1..].to_i
  dir = is_right ? rotate_right(dir) : rotate_left(dir)
  steps.times do
    pos = {pos[0] + dir[0], pos[1] + dir[1]}

    if pos.in? SEEN
      p pos.sum.abs
      exit
    end
    SEEN << pos
  end
}

