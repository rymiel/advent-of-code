record Vec3, x : Int32, y : Int32, z : Int32

class Cube
  property a : Vec3
  property b : Vec3
  @points : Set(Vec3)?

  def initialize(@a, @b)
  end

  def subcubes
    return @points.not_nil! if @points
    @points = Set(Vec3).new
    (a.x..b.x).each do |x|
      (a.y..b.y).each do |y|
        (a.z..b.z).each do |z|
          @points.not_nil! << Vec3.new x, y, z
        end
      end
    end
    @points.not_nil!
  end

  def floor
    a.z
  end

  def ceiling
    b.z
  end

  def supports_points
    points = [] of Vec3
    (a.x..b.x).each do |x|
      (a.y..b.y).each do |y|
        points << Vec3.new x, y, ceiling + 1
      end
    end
    points
  end

  def drop!(by = 1)
    @a = @a.copy_with(z: @a.z - by)
    @b = @b.copy_with(z: @b.z - by)
    @points = nil
  end

  def includes?(point : Vec3)
    point.x.in?((a.x..b.x)) && point.y.in?((a.y..b.y)) && point.z.in?((a.z..b.z))
  end
end

cubes = File.read_lines("txt/day22").map do |line|
  a, b = line.split("~").map { |i|
    x, y, z = i.split(",").map &.to_i
    Vec3.new x, y, z
  }
  Cube.new(a, b)
end

def drop(cubes : Array(Cube))
  dropped = Set(Vec3).new
  cubes.sort_by(&.floor).each do |cube|
    if cube.floor == 1
      # on the ground
      cube.subcubes.each { |i| dropped << i }
      next
    end

    loop do
      # shift down
      cube.drop!
      break if cube.floor == 0
      break if cube.subcubes.any? &.in? dropped
    end

    # bring back up
    cube.drop! -1
    cube.subcubes.each { |i| dropped << i }
  end
end

drop cubes

def calculate_supporting(cubes : Array(Cube)) : {Hash(Cube, Set(Cube)), Hash(Cube, Set(Cube))}
  supports = Hash(Cube, Set(Cube)).new { |hash, key| hash[key] = Set(Cube).new }
  supported_by = Hash(Cube, Set(Cube)).new { |hash, key| hash[key] = Set(Cube).new }

  cubes.each do |cube|
    supported = Set(Cube).new
    cube.supports_points.each do |support_point|
      cubes.each do |other|
        next if other == cube
        next if other.in? supported

        if support_point.in? other
          supports[cube] << other
          supported_by[other] << cube
        end
      end
    end
  end

  return supports, supported_by
end

supports, supported_by = calculate_supporting cubes

def can_be_disintegrated(cube, supports, supported_by)
  if supports[cube].empty?
    return true
  end
  supports[cube].each do |needs_support|
    if supported_by[needs_support].size == 1
      return false
    end
  end
  return true
end

p cubes.count { |cube| can_be_disintegrated cube, supports, supported_by }
