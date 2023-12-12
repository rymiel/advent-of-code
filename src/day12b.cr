QUESTION_BYTE = '?'.ord.to_u8!
DOT_BYTE      = '.'.ord.to_u8!
HASH_BYTE     = '#'.ord.to_u8!

def strip_leading_dots(status : Bytes) : Bytes
  return Bytes.new(0) if status.all? DOT_BYTE
  first_non_dot = status.index &.!= DOT_BYTE
  return status if first_non_dot.nil?
  status[first_non_dot..]
end

CACHE = Hash({Bytes, Array(Int32)}, UInt64).new

def count_valid(status : Bytes, groups : Array(Int32)) : UInt64
  cached = CACHE[{status, groups}]?
  return cached if cached
  result = count_valid_internal(status, groups)
  CACHE[{status, groups}] = result
  result
end

def count_valid_internal(status : Bytes, groups : Array(Int32)) : UInt64
  if status.empty?
    if groups.empty?
      # We've consumed both inputs, this is a valid input
      return 1u64
    end
    # Only consumed one input, it's incomplete
    return 0u64
  end

  f = status.first

  case f
  when DOT_BYTE
    # Skip all dots
    return count_valid(strip_leading_dots(status), groups)
  when QUESTION_BYTE
    # Branch, trying both options for hash and dot for the question mark, and sum both sides
    slice = status.dup
    slice[0] = DOT_BYTE
    dot_count = count_valid(slice.dup, groups)
    slice[0] = HASH_BYTE
    return count_valid(slice, groups) + dot_count
  when HASH_BYTE
    # There are no more groups left, so this can't be part of any
    return 0u64 if groups.empty?

    current_group = groups.first

    # There is less input remaining than the current group requires
    return 0u64 if status.size < current_group

    lookahead = status[...current_group]

    # The current group contains dots, so it's not actually a single contiguous group.
    # note that any question marks will just be assumed to be hashes here.
    return 0u64 if lookahead.any? DOT_BYTE

    next_char = status[current_group]?

    # The character right after this group was a hash, so this group is actually longer than expected
    return 0u64 if next_char == HASH_BYTE

    if next_char == QUESTION_BYTE
      # This is a valid group, but there is a question mark right after this group's end. It can't be anything other
      # than a dot, since otherwise the group would be too long, so skip it entirely.
      return count_valid(status[(current_group + 1)..], groups[1..])
    end

    # This is a valid group, continue with the next group
    return count_valid(status[current_group..], groups[1..])
  else raise "Invalid character"
  end
end

p File.read_lines("txt/day12").map { |line|
  status, groups = line.split " "
  groups = groups.split(",").map &.to_i

  expanded_status = ([status] * 5).join("?")
  expanded_groups = groups * 5
  count_valid(expanded_status.to_slice, expanded_groups)
}.sum
