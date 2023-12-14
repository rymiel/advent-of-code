class Reindeer
  getter distance : Int32 = 0
  getter cooldown : Int32
  getter? flying : Bool = true

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
end

reindeers = File.read_lines("txt/day14").map { |line|
  speed, fly_time, rest_time = line.scan(/\d+/).map &.[0].to_i
  Reindeer.new speed, fly_time, rest_time
}

2503.times { reindeers.each &.step }
puts reindeers.max_of &.distance
