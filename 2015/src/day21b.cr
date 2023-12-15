class Fighter
  property hp : Int32
  property dmg : Int32
  property def : Int32

  def initialize(@hp, @dmg, @def)
  end

  def fight(other : Fighter)
    loop do
      self.hit other
      return true if other.dead?
      other.hit self
      return false if self.dead?
    end
  end

  def hit(other : Fighter)
    damage_by = Math.max(dmg - other.def, 1)
    other.hp -= damage_by
  end

  def dead?
    @hp <= 0
  end
end

# name, cost, +atk, +def
alias Item = {String, Int32, Int32, Int32}
WEAPONS = [
  {"Dagger", 8, 4, 0},
  {"Shortsword", 10, 5, 0},
  {"Warhammer", 25, 6, 0},
  {"Longsword", 40, 7, 0},
  {"Greataxe", 74, 8, 0},
]
ARMOR = [
  {"Leather", 13, 0, 1},
  {"Chainmail", 31, 0, 2},
  {"Splintmail", 53, 0, 3},
  {"Bandedmail", 75, 0, 4},
  {"Platemail", 102, 0, 5},
]
RINGS = [
  {"Damage +1", 25, 1, 0},
  {"Damage +2", 50, 2, 0},
  {"Damage +3", 100, 3, 0},
  {"Defense +1", 20, 0, 1},
  {"Defense +2", 40, 0, 2},
  {"Defense +3", 80, 0, 3},
]

boss_hp, boss_dmg, boss_def = File.read_lines("txt/day21").map(&.split(": ")[1].to_i)

boss = Fighter.new boss_hp, boss_dmg, boss_def

max = 0
WEAPONS.each do |weapon|
  ARMOR.each.chain([nil].each).each do |armor|
    (0..2).flat_map { |i| RINGS.each_combination(i) }.each do |rings|
      total_loadout = ([weapon] + [armor] + rings).compact
      total_cost = total_loadout.map(&.[1]).sum
      next if total_cost <= max
      total_atk = total_loadout.map(&.[2]).sum
      total_def = total_loadout.map(&.[3]).sum

      player = Fighter.new 100, total_atk, total_def
      wins = player.fight boss.dup

      max = total_cost unless wins
    end
  end
end
p max
