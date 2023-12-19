flows, _ = File.read("txt/day19").strip.split("\n\n").map(&.split "\n")

record Rule, evaluate : UInt8, gt : Bool, threshold : Int32, output : String, unconditional : Bool do
  def initialize(@evaluate, @gt, @threshold, @output)
    @unconditional = false
  end

  def initialize(@output)
    @unconditional = true
    # don't matter
    @evaluate = 0
    @threshold = 0
    @gt = false
  end
end

PROPS = ["x", "m", "a", "s"]

flows = flows.map { |flow|
  name, rest = flow.split("{")
  rules = rest.rchop("}").split(",").map { |rule|
    if rule.includes? ':'
      m = /(\w+)(<|>)(\d+):(\w+)/.match! rule
      Rule.new(PROPS.index!(m[1]).to_u8!, m[2] == ">", m[3].to_i, m[4])
    else
      Rule.new rule
    end
  }
  {name, rules}
}.to_h

alias PartRange = Array(Range(Int32, Int32))

def split(part : PartRange, rule : Rule) : {PartRange?, PartRange?}
  if rule.unconditional
    return {nil, part}
  end

  continues = part.dup
  splits_off = part.dup
  range = part[rule.evaluate]

  if rule.gt
    left = (range.begin..rule.threshold)
    right = (rule.threshold + 1..range.end)
    continues[rule.evaluate] = left
    splits_off[rule.evaluate] = right
  else
    left = (range.begin..rule.threshold - 1)
    right = (rule.threshold..range.end)
    continues[rule.evaluate] = right
    splits_off[rule.evaluate] = left
  end

  {continues, splits_off}
end

def evaluate(initial_part : PartRange, rules : Hash(String, Array(Rule))) : UInt64
  queue = Deque({String, PartRange}).new
  queue << {"in", initial_part}

  combinations = 0u64

  loop do
    break if queue.empty?
    next_in_queue = queue.shift
    ruleset = rules[next_in_queue[0]]
    running = next_in_queue[1]
    ruleset.each do |rule|
      continues, splits_off = split(running, rule)

      if splits_off
        case rule.output
        when "A" then combinations += splits_off.map(&.size.to_u64!).product
        when "R" then nil # don't care
        else          queue << {rule.output, splits_off}
        end
      end

      break if continues.nil?
      running = continues
    end
  end

  return combinations
end

p evaluate([(1..4000), (1..4000), (1..4000), (1..4000)], flows)
