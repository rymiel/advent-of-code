win_count = File.read_lines("txt/day4").map(&.split(':')[1].split('|').map(&.strip)).map { |(win, actual)|
  win = win.split(" ", remove_empty: true).map(&.strip.to_i)
  actual = actual.split(" ", remove_empty: true).map(&.strip.to_i)
  actual.count(&.in? win)
}
extra_cards = Array.new(win_count.size, 1)
win_count.each_with_index { |card, i|
  ((i + 1)..(i + card)).each do |j|
    next if j > extra_cards.size
    extra_cards[j] += extra_cards[i]
  end
}
p extra_cards.sum
