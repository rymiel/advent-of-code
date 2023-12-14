class Reindeer
  getter distance : Int32 = 0
  getter cooldown : Int32
  getter? flying : Bool = true
  getter points : Int32 = 0

  def initialize(@speed : Int32, @fly_time : Int32, @rest_time : Int32)
    @cooldown = @fly_time
  end

  def step
    if flying?
      @distance += @speed
      @cooldown -= 1
      if @cooldown.zero?
        @flying = false
        @cooldown = @rest_time
      end
    else
      @cooldown -= 1
      if @cooldown.zero?
        @flying = true
        @cooldown = @fly_time
      end
    end
  end

  def award
    @points += 1
  end
end

reindeers = File.read_lines("txt/day14").map { |line|
  speed, fly_time, rest_time = line.scan(/\d+/).map &.[0].to_i
  Reindeer.new speed, fly_time, rest_time
}

2503.times {
 reindeers.each &.step
 reindeers.select(&.distance.== reindeers.max_of(&.distance)).each &.award
}
puts reindeers.max_of &.points
