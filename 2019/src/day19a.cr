require "./intcode"

W = 50
H = 50

def to_xy(n : Int32) : {Int32, Int32}
  {n % W, n // W}
end

map = Set({Int32, Int32}).new

H.times do |y|
  W.times do |x|
    flag = true
    cpu = Program.new(read_program("txt/day19"),
      ->{
        v = flag ? x : y
        flag = !flag

        v.to_i64
      },
      ->(o : Int64) {
        map << {x, y} if o == 1i64
      }
    ).run
  end
end

p map.size
