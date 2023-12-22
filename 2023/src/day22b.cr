record Vec3, x : Int32, y : Int32, z : Int32

class Cube
  property a : Vec3
  property b : Vec3

  def initialize(@a, @b)
  end

  def subcubes
    points = Set(Vec3).new
    (a.x..b.x).each do |x|
      (a.y..b.y).each do |y|
        (a.z..b.z).each do |z|
          points << Vec3.new x, y, z
        end
      end
    end
    points
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

  def intersects?(other : Cube)
    subcubes.intersects? other.subcubes
  end

  def drop!(by = 1)
    @a = @a.copy_with(z: @a.z - by)
    @b = @b.copy_with(z: @b.z - by)
  end

  def any_intersect(cubes : Array(Cube))
    cubes.any? do |other|
      next false if other.ceiling < floor
      next false if other.floor > ceiling
      next false if other == self
      intersects? other
    end
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
  cubes.sort_by(&.floor).each_with_index do |cube, i|
    if cube.floor == 1
      # on the ground
      next
    end

    loop do
      # shift down
      cube.drop!
      break if cube.floor == 0
      break if cube.any_intersect cubes
    end

    # bring back up
    cube.drop! -1
  end
end

drop cubes

class CascadeState
  getter supports = Hash(Cube, Set(Cube)).new { |hash, key| hash[key] = Set(Cube).new }
  getter supported_by = Hash(Cube, Set(Cube)).new { |hash, key| hash[key] = Set(Cube).new }

  def initialize(cubes : Array(Cube))
    cubes.each do |cube|
      supported = Set(Cube).new
      cube.supports_points.each do |support_point|
        cubes.each do |other|
          next if other == cube
          next if other.in? supported

          if support_point.in? other.subcubes
            supports[cube] << other
            supported_by[other] << cube
          end
        end
      end
    end
  end

  def can_be_disintegrated(cube)
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

  def cascade_count(cube)
    return 0 if can_be_disintegrated cube

    dropped = Set(Cube).new
    dropped << cube
    prev_dropped = 0

    until dropped.size == prev_dropped
      prev_dropped = dropped.size
      dropped.each do |d|
        supports[d].each do |s|
          dropped << s if supported_by[s].all? &.in? dropped
        end
      end
    end

    dropped.size - 1
  end
end

cascade = CascadeState.new cubes
p cubes.sum { |cube| cascade.cascade_count cube }
