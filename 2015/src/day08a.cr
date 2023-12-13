def lengths(str : String) : {Int32, Int32}
  literal = 0
  memory = 0
  reader = Char::Reader.new(str)
  while reader.has_next?
    case reader.current_char
    when '"'
      literal += 1
    when '\\'
      if reader.next_char == 'x'
        reader.next_char
        reader.next_char
        literal += 4
        memory += 1
      else
        literal += 2
        memory += 1
      end
    else
      literal += 1
      memory += 1
    end
    reader.next_char
  end

  {literal, memory}
end

p File.read_lines("txt/day8").map { |str| lengths(str) }.map { |(a, b)| a - b }.sum
