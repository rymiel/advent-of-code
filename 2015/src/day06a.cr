alias Coord = {Int32, Int32}

LIGHTS = Set(Coord).new

def on(coord : Coord) : Nil
  LIGHTS.add coord
end

def off(coord : Coord) : Nil
  LIGHTS.delete coord
end

def toggle(coord : Coord) : Nil
  coord.in?(LIGHTS) ? off(coord) : on(coord)
end

enum Operation
  TurnOn
  TurnOff
  Toggle
end

File.read_lines("txt/day6").each do |line|
  operation = if chopped = line.lchop? "turn on "
                Operation::TurnOn
              elsif chopped = line.lchop? "turn off "
                Operation::TurnOff
              elsif chopped = line.lchop? "toggle "
                Operation::Toggle
              else
                raise "Invalid operation"
              end

  s, e = chopped.not_nil!.split(" through ").map(&.split(",").map &.to_i)

  (s[0]..e[0]).each do |x|
    (s[1]..e[1]).each do |y|
      case operation
      in .turn_on?  then on({x, y})
      in .turn_off? then off({x, y})
      in .toggle?   then toggle({x, y})
      end
    end
  end
end

p LIGHTS.size
