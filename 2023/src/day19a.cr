flows, parts = File.read("txt/day19").strip.split("\n\n").map(&.split "\n")

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

parts = parts.map { |part|
  part.scan(/\d+/).map &.[0].to_i
}

def evaluate(part : Array(Int32), rule : Rule) : Bool
  return true if rule.unconditional

  prop = part[rule.evaluate]
  if rule.gt
    return prop > rule.threshold
  else
    return prop < rule.threshold
  end
end

def evaluate(part : Array(Int32), rules : Hash(String, Array(Rule))) : Bool
  ruleset = rules["in"]

  loop do
    ruleset.each do |rule|
      if evaluate(part, rule)
        case rule.output
        when "R" then return false
        when "A" then return true
        else          ruleset = rules[rule.output]
        end
        break
      end
    end
  end

  raise "Unreachable"
end

p parts.select { |part| evaluate part, flows }.map(&.sum).sum
