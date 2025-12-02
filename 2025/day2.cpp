#include "main.hpp"
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <sstream>
#include <unordered_set>
#include <utility>
#include <vector>

auto parse() {
  std::vector<std::pair<uint64_t, uint64_t>> ranges;
  uint64_t a, b;
  char _;
  while (std::cin >> a >> _ >> b) {
    ranges.push_back({a, b});
    std::cin >> _; // comma
  }
  return ranges;
}

auto round_up(uint64_t i, int b = 2) {
  int digits = std::to_string(i).size();
  if (digits % b == 0)
    return i;
  auto new_digits = (digits / b + 1) * b;

  return ipow(10ul, new_digits - 1);
}

auto round_down(uint64_t i, int b = 2) {
  int digits = std::to_string(i).size();
  if (digits % b == 0)
    return i;
  if (digits < b)
    return 0ul;
  auto new_digits = (digits / b) * b;

  return ipow(10ul, new_digits) - 1;
}

auto repeat(uint64_t n, int b = 2) {
  std::stringstream ss{};
  for (int i = 0; i < b; i++)
    ss << n;
  return std::stoul(ss.str());
}

auto top_half(uint64_t n, int b = 2) {
  auto s = std::to_string(n);
  return std::stoul(s.substr(0, s.size() / b));
}

void part1() {
  auto ranges = parse();

  uint64_t sum = 0;
  for (auto [x, y] : ranges) {
    x = round_up(x);
    y = round_down(y);
    if (x > y)
      continue;
    auto hx = top_half(x);
    auto hy = top_half(y);

    for (auto i = hx; i <= hy; i++) {
      auto r = repeat(i);
      if (r >= x && r <= y)
        sum += r;
    }
  }

  std::cout << sum << "\n";
}

void part2() {
  auto ranges = parse();

  std::unordered_set<uint64_t> invalid = {};
  for (int b = 2; b <= 8; b++) {
    for (auto [x, y] : ranges) {
      x = round_up(x, b);
      y = round_down(y, b);
      if (x > y)
        continue;
      auto hx = top_half(x, b);
      auto hy = top_half(y, b);

      for (auto i = hx; i <= hy; i++) {
        auto r = repeat(i, b);
        if (r >= x && r <= y)
          invalid.emplace(r);
      }
    }
  }

  std::cout << std::ranges::fold_left(invalid, 0, std::plus<>{}) << "\n";
}
