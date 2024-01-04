require "./intcode"

Program.new(read_program("txt/day9"),
  ->{ 2i64 },
  ->(o : Int64) { puts o }
).run

