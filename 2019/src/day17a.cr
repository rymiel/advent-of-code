require "./intcode"

map = Array(Array(Char)).new
map << [] of Char

cpu = Program.new(read_program("txt/day17"),
  ->{ 0i64 },
  ->(o : Int64) {
    if o == 10
      map << [] of Char
    else
      map.last << o.chr
    end
  }
).run

alignment = 0

map.each_with_index do |line, y|
  line.each_with_index do |c, x|
    if c == '#'
      n = { {0, -1}, {0, 1}, {-1, 0}, {1, 0} }.map { |dir| map[y + dir[1]]?.try &.[x + dir[0]]? }
      if n.all? '#'
        alignment += (x * y)
      end
    end
  end
end

p alignment
