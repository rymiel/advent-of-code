enum Type
  Broadcaster
  Flip
  Conjunction
end

record Module, name : String, type : Type, ins : Array(String), outs : Array(String)

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

class State
  property low_pulses = 0
  property high_pulses = 0

  getter flip_state = Hash(String, Bool).new false
  getter conj_state = Hash(String, Hash(String, Bool)).new { |hash, key| hash[key] = Hash(String, Bool).new false }

  getter queue = Deque({Bool, String, String}).new

  def make_pulse(mod : Module, pulse : Bool)
    mod.outs.each do |target|
      queue << {pulse, mod.name, target}
      if pulse
        @high_pulses += 1
      else
        @low_pulses += 1
      end
    end
  end

  def evaluate : Int32
    # button press
    @low_pulses += 1
    queue << {false, "button", "broadcaster"}

    loop do
      break if queue.empty?
      next_in_queue = queue.shift
      pulse = next_in_queue[0]
      sent_from = next_in_queue[1]
      mod = MODULES[next_in_queue[2]]?
      if mod.nil?
        # not a real module, just an output
        next
      end

      case mod.type
      in .broadcaster?
        make_pulse mod, pulse
      in .flip?
        next if pulse
        next_pulse = flip_state[mod.name] = !flip_state[mod.name]
        make_pulse mod, next_pulse
      in .conjunction?
        conj_state[mod.name][sent_from] = pulse
        state = conj_state[mod.name]
        all_high = mod.ins.all? { |k| state[k] }
        make_pulse mod, !all_high
      end
    end

    low_pulses * high_pulses
  end

  def result : Int32
    low_pulses * high_pulses
  end
end

state = State.new
1000.times { state.evaluate }
p state.result
