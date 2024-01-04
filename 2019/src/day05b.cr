require "./intcode"

cpu = Program.new(read_program("txt/day5"),
  ->{ 5i64 },
  ->(o : Int64) { p o if o != 0 }
).run

