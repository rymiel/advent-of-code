BOXES = Array(Hash(String, Int32)).new(256) { Hash(String, Int32).new }

# if focal_length is nil, it is a remove (-) instruction
record Instruction, label : String, chash : Int32, focal_length : Int32? do
  def initialize(@label, @focal_length)
    @chash = custom_hash(@label)
  end
end

def custom_hash(str : String) : Int32
  value = 0
  str.each_char do |c|
    value += c.ord
    value *= 17
    value = value % 256
  end

  value
end

File.read("txt/day15").strip.split(",").map { |i|
  if i.ends_with? '-'
    next Instruction.new i.rchop('-'), nil
  end
  label, focal = i.split("=")
  Instruction.new label, focal.to_i
}.each { |i|
  box = BOXES[i.chash]
  if (focal = i.focal_length).nil?
    box.delete(i.label)
  else
    box[i.label] = focal
  end
}

p BOXES.map_with_index { |box, box_i|
  (box_i + 1) * (box.map_with_index { |lens, lens_i|
    (lens_i + 1) * lens[1]
  }.sum)
}.sum
