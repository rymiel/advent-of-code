require "./intcode"

Program.new(read_program("txt/day9"),
  ->{ 1i64 },
  ->(o : Int64) { puts o }
).run

