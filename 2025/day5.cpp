#include "main.hpp"
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <ranges>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

auto parse() {
  auto lines = read_lines();
  auto split = std::ranges::find(lines, "");
  auto ranges = std::vector<std::pair<uint64_t, uint64_t>>{};
  auto nums = std::vector<uint64_t>{};
  for (const auto& line : std::ranges::subrange(lines.begin(), split)) {
    auto stream = std::stringstream{line};
    uint64_t a, b;
    stream >> a;
    stream.ignore(1, '-');
    stream >> b;
    ranges.push_back({a, b});
  }
  for (const auto& line : std::ranges::subrange(split + 1, lines.end()))
    nums.push_back(std::stoull(line));

  return std::pair{ranges, nums};
}

void part1() {
  auto [ranges, nums] = parse();

  int count = 0;
  for (auto num : nums) {
    for (auto [low, high] : ranges) {
      if (num >= low && num <= high) {
        count++;
        break;
      }
    }
  }

  std::cout << count << "\n";
}

void part2() {
  auto [ranges, _] = parse();

loop:
  for (auto x = ranges.begin(); x != ranges.end(); x++) {
    for (auto y = x + 1; y != ranges.end(); y++) {
      if (x->first <= y->second && y->first <= x->second) {
        auto z_first = std::min(x->first, y->first);
        auto z_second = std::max(x->second, y->second);
        ranges.erase(y);
        *x = {z_first, z_second};
        goto loop;
      }
    }
  }

  uint64_t count = 0;
  for (auto [low, high] : ranges)
    count += high - low + 1;

  std::cout << count << "\n";
}
