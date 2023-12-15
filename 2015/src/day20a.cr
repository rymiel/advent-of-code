def divisor_sum(n : Int32) : Int32
  sum = 0
  (1..Math.sqrt(n).to_i).each do |i|
    if n % i == 0
      sum += i
      sum += (n // i) if i != n // i
    end
  end

  return sum
end

need = File.read("txt/day20").strip.to_i
p (1..).each.map { |i| divisor_sum(i) * 10 }.index! { |i| i >= need } + 1
