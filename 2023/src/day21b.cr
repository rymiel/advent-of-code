# Bad problems deserve bad solutions
i = 327
s0 = 92548u64
s1 = 59182u64
while i != 26501365
  i += 131
  s1 += 29546
  s0 += s1
end
p s0
