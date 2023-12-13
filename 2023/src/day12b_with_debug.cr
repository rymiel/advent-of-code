QUESTION_BYTE = '?'.ord.to_u8!
DOT_BYTE      = '.'.ord.to_u8!
HASH_BYTE     = '#'.ord.to_u8!

def status_bytes_to_groups(status : Bytes) : Array(Int32)
  groups = [] of Int32
  previous = DOT_BYTE
  i = 0
  status.each { |c|
    if c == HASH_BYTE && previous == HASH_BYTE
      i += 1
    elsif c == HASH_BYTE && previous == DOT_BYTE
      i = 1
    elsif c == DOT_BYTE && previous == HASH_BYTE
      groups << i
      i = 0
    end

    previous = c
  }

  if previous == HASH_BYTE
    groups << i
  end

  groups
end

def valid(status : Bytes, groups : Array(Int32)) : Bool
  damaged = status_bytes_to_groups status
  return damaged == groups
end

def strip_leading_dots(status : Bytes) : Bytes
  return Bytes.new(0) if status.all? DOT_BYTE
  first_non_dot = status.index &.!= DOT_BYTE
  return status if first_non_dot.nil?
  status[first_non_dot..]
end

CACHE = Hash({Bytes, Array(Int32)}, Int32).new

def count_valid(status : Bytes, groups : Array(Int32), origin)
  cached = CACHE[{status, groups}]?
  if cached
    print "\n#{" " * (origin - status.size)}#{String.new(status)} => #{cached}"
    return cached
  end
  result = count_valid_internal(status, groups, origin)
  CACHE[{status, groups}] = result
  result
end

def count_valid_internal(status : Bytes, groups : Array(Int32), origin)
  print "\n#{" " * (origin - status.size)}#{String.new(status)} "

  if status.empty?
    if groups.empty?
      return 1.tap { print "empty-status-empty-group " }
    end
    return 0.tap { print "empty-status-some-group " }
  end

  f = status.first

  case f
  when DOT_BYTE then return count_valid(strip_leading_dots(status), groups, origin)
  when QUESTION_BYTE
    slice = status.dup
    slice[0] = DOT_BYTE
    dot_count = count_valid(slice.dup, groups, origin)
    slice[0] = HASH_BYTE
    return count_valid(slice, groups, origin) + dot_count
  when HASH_BYTE
    return 0.tap { print "empty-group " } if groups.empty?
    current_group = groups.first
    print "g#{current_group} "
    return 0.tap { print "too-small " } if status.size < current_group

    lookahead = status[...current_group]
    print "lookahead=#{String.new lookahead} "
    return 0.tap { print "not-contiguous " } unless lookahead.none? DOT_BYTE
    next_char = status[current_group]?
    return 0.tap { print "not-isolated " } if next_char == HASH_BYTE

    if next_char == QUESTION_BYTE
      print "valid-group-skip-question "
      return count_valid(strip_leading_dots(status[(current_group + 1)..]), groups[1..], origin)
    end

    print "valid-group "
    return count_valid(strip_leading_dots(status[current_group..]), groups[1..], origin)
  else raise "Invalid character"
  end
end

p File.read_lines("txt/day12").map { |line|
  status, groups = line.split " "
  groups = groups.split(",").map &.to_i

  expanded_status = ([status] * 5).join("?")
  expanded_groups = groups * 5
  puts expanded_groups
  count = count_valid(expanded_status.to_slice, expanded_groups, expanded_status.size)
  puts
  puts count
  puts

  count
}.sum
