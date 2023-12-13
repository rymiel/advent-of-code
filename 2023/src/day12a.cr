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

def filter_unknown(status, group_sum) : Iterator(Bytes)
  return [status.to_slice].each unless status.includes? '?'

  slice = status.to_slice.dup
  question_locations = [] of Int32
  slice.each_with_index do |c, i|
    question_locations << i if c == QUESTION_BYTE
  end

  question_count = question_locations.size
  combination_count = 2u64 ** question_count
  hash_count = slice.count HASH_BYTE

  (0...combination_count).each.compact_map { |i|
    next if (i.popcount + hash_count) != group_sum
    (0...question_count).reduce(slice) { |s, bit|
      s[question_locations[bit]] = i.bit(bit).zero? ? DOT_BYTE : HASH_BYTE
      s
    }
  }
end

p File.read_lines("txt/day12").map { |line|
  status, groups = line.split " "
  groups = groups.split(",").map &.to_i
  group_sum = groups.sum
  count = filter_unknown(status, group_sum).count { |filtered| valid(filtered, groups) }

  count
}.sum
