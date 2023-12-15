row, column = File.read("txt/day25").scan(/\d+/).map(&.[0].to_i)

def index(row, column)
  row_start(row) + ((column * (column + 1) // 2) - 1) + ((row - 1) * (column - 1))
end

def row_start(row)
  (row - 1) * (row) // 2 + 1
end

def next_code(value : UInt64) : UInt64
  (value * 252533) % 33554393
end

target_index = index(row, column)

value = 20151125u64
(1..).each do |i|
  if i == target_index
    p value
    break
  end
  value = next_code(value)
end
