enum Spell : UInt8
  MagicMissile
  Drain
  Shield
  Poison
  Recharge

  def mana_cost
    case self
    in .magic_missile? then 53
    in .drain?         then 73
    in .shield?        then 113
    in .poison?        then 173
    in .recharge?      then 229
    end
  end

  def opportunistic_cost
    case self
    in .magic_missile? then 53
    in .drain?         then 73
    in .shield?        then 113
    in .poison?        then 173
    in .recharge?      then -(101 * 5) + 229
    end
  end
end

enum Outcome
  Win
  LoseInvalidSpell
  LoseDead
  LoseOutOfMana
  LoseOutOfSpells
end

class Fighter
  property hp : Int32
  property mana : Int32
  property dmg : Int32
  property def : Int32 = 0
  property effects = [] of {Int32, Spell}

  def initialize(@hp, @mana, @dmg)
  end

  def cast(spell : Spell, target : Fighter) : Outcome?
    return Outcome::LoseOutOfMana if spell.mana_cost > @mana
    @mana -= spell.mana_cost
    case spell
    in .magic_missile? then target.hp -= 4
    in .drain?
      target.hp -= 2
      self.hp += 2
    in .shield?
      return Outcome::LoseInvalidSpell if self.effects.any? &.[1].shield?
      self.effects << {6, spell}
    in .poison?
      return Outcome::LoseInvalidSpell if target.effects.any? &.[1].poison?
      target.effects << {6, spell}
    in .recharge?
      return Outcome::LoseInvalidSpell if self.effects.any? &.[1].recharge?
      self.effects << {5, spell}
    end

    nil
  end

  def tick_effects
    @def = 0
    @effects = @effects.compact_map do |(timer, effect)|
      case effect
      in .magic_missile?, .drain? then next nil
      in .shield?                 then @def += 7
      in .poison?                 then @hp -= 3
      in .recharge?               then @mana += 101
      end

      next nil if timer == 1
      {timer - 1, effect}
    end
  end

  def fight(cast_sequence : Array(Spell), other : Fighter) : Outcome
    cast_sequence.each do |spell|
      self.tick_effects
      other.tick_effects
      invalid_spell = self.cast spell, other
      return invalid_spell if invalid_spell
      return Outcome::Win if other.dead?

      self.tick_effects
      other.tick_effects
      return Outcome::Win if other.dead?
      other.hit self
      return Outcome::LoseDead if self.dead?
    end

    return Outcome::LoseOutOfSpells
  end

  def hit(other : Fighter)
    damage_by = Math.max(dmg - other.def, 1)
    other.hp -= damage_by
  end

  def dead?
    @hp <= 0
  end
end

boss_hp, boss_dmg = File.read_lines("txt/day22").map(&.split(": ")[1].to_i)
player = Fighter.new 50, 500, 0
boss = Fighter.new boss_hp, 0, boss_dmg

min = Int32::MAX
s = Time.monotonic
search_space = [] of Array(Spell)
search_space << [] of Spell
loop do
  had_cheaper = false
  new_candidates = [] of Array(Spell)
  search_space.each do |prefix|
    Spell.each do |tail|
      sequence = prefix + [tail]
      opportunistic_sum = sequence.sum(&.opportunistic_cost)
      next if opportunistic_sum > player.mana

      next if sequence.each_cons_pair.any? { |(a, b)| a == b && (a.shield? || a.poison? || a.recharge?) }

      sequence_sum = sequence.sum(&.mana_cost)
      next if sequence_sum >= min
      had_cheaper = true
      enemy = boss.dup
      outcome = player.dup.fight sequence, enemy

      next if outcome.lose_invalid_spell? || outcome.lose_out_of_mana? || outcome.lose_dead?

      new_candidates << sequence

      if outcome.win?
        min = sequence_sum
      end
    end
  end

  break if !had_cheaper && min != Int32::MAX

  search_space = new_candidates
end

p min
