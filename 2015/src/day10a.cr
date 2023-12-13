def look_and_say(str : String) : String
  String.build do |io|
    previous = nil
    i = 1
    str.each_char do |c|
      if c == previous
        i += 1
      elsif previous
        io << i
        io << previous
        i = 1
      end

      previous = c
    end

    io << i
    io << previous
  end
end

p (0...40).reduce(File.read("txt/day10").strip) { |i| look_and_say i }.size
