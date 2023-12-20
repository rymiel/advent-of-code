# This solution is significantly more fucked up compared to part 1 since I thought I could just optimize it enough
# to brute force it
# In the end, I figured out a smarter way but I couldn't be bothered to revert it back to the system part 1 used.

enum Type : UInt8
  Broadcaster
  Flip
  Conjunction
end

record Module, name : String, type : Type, ins : Array(String), outs : Array(String)
record LinearModule, index : UInt8, type : Type, ins : UInt8[8], outs : UInt8[8]

MODULES = File.read_lines("txt/day20").map { |mod|
  input, outputs = mod.split(" -> ")
  if input == "broadcaster"
    name = input
    type = Type::Broadcaster
  elsif input.starts_with? '%'
    name = input[1..]
    type = Type::Flip
  elsif input.starts_with? '&'
    name = input[1..]
    type = Type::Conjunction
  else
    raise "Invalid module"
  end

  {name, Module.new name, type, Array(String).new, outputs.split(", ")}
}.to_h

MODULES.transform_values! { |value|
  value.copy_with(ins: MODULES.values.select { |v| value.name.in? v.outs }.map &.name)
}

KEYS = MODULES.keys
KEYS << "rx"

RX          = KEYS.index!("rx").to_u8!
BROADCASTER = KEYS.index!("broadcaster").to_u8!
NIL         = 0xffu8

LINEAR_MODULES = MODULES.values.map { |v|
  index = KEYS.index!(v.name).to_u8!
  ins_slice = Slice(UInt8).new 8, NIL
  outs_slice = Slice(UInt8).new 8, NIL

  v.ins.each_with_index do |j, i|
    ins_slice[i] = KEYS.index!(j).to_u8!
  end
  v.outs.each_with_index do |j, i|
    outs_slice[i] = KEYS.index!(j).to_u8!
  end

  ins = StaticArray(UInt8, 8).new { |i| ins_slice[i] }
  outs = StaticArray(UInt8, 8).new { |i| outs_slice[i] }

  LinearModule.new index, v.type, ins, outs
}

class LinearState
  getter flip_state = Slice(Bool).new LINEAR_MODULES.size, false
  getter conj_state = Slice(Slice(Bool)).new(LINEAR_MODULES.size) { |i| Slice(Bool).new(LINEAR_MODULES[i].ins.count(&.!= NIL), false) }

  getter queue = Deque({Bool, UInt8, UInt8}).new

  getter vital_conj = 0xffu8
  getter dependency_cycles = [] of UInt64

  def make_pulse(mod : LinearModule, pulse : Bool)
    8.times do |i|
      if (target = mod.outs[i]) != NIL
        queue << {pulse, mod.index, target}
      end
    end
  end

  def prepare
    conj = LINEAR_MODULES.find! &.outs.includes? RX
    dependencies = conj.ins.select(&.!= NIL)
    @dependency_cycles = Array(UInt64).new(dependencies.size) { 0u64 }
    @vital_conj = conj.index
  end

  def evaluate(loop_nr : UInt64) : Bool
    # button press
    queue << {false, NIL, BROADCASTER}

    loop do
      break if queue.empty?
      next_in_queue = queue.shift
      pulse = next_in_queue[0]
      sent_from = next_in_queue[1]
      if next_in_queue[2] == RX
        next
      end
      mod = LINEAR_MODULES[next_in_queue[2]]

      case mod.type
      in .broadcaster?
        make_pulse mod, pulse
      in .flip?
        next if pulse
        next_pulse = flip_state[mod.index] = !flip_state[mod.index]
        make_pulse mod, next_pulse
      in .conjunction?
        state = conj_state[mod.index]
        sent_idx = mod.ins.index!(sent_from)
        state[sent_idx] = pulse
        if pulse && mod.index == @vital_conj
          dependency_cycles[sent_idx] = loop_nr
          if dependency_cycles.none? &.zero?
            return true
          end
        end
        all_high = state.all?
        make_pulse mod, !all_high
      end
    end

    false
  end
end

require "benchmark"

state = LinearState.new
state.prepare
presses = 0u64
loop do
  presses += 1
  all_deps = state.evaluate presses
  if all_deps
    deps = state.dependency_cycles
    p deps.reduce { |a, b| a.lcm b }
    break
  end
end
