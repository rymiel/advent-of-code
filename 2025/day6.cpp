#include "main.hpp"
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <ranges>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

auto parse_part1() {
  auto lines = read_lines();
  auto nums = std::vector<std::vector<int>>{};
  auto ops = std::vector<char>{};
  for (const auto& line : lines) {
    auto ss = std::stringstream{line};
    if (ss.peek() == '*' || ss.peek() == '+') {
      char c;
      while (ss >> c)
        ops.push_back(c);
      break;
    } else {
      auto n = std::vector<int>{};
      int i;
      while (ss >> i)
        n.push_back(i);
      nums.push_back(n);
    }
  }

  return std::pair{nums, ops};
}

void part1() {
  auto [nums, ops] = parse_part1();

  uint64_t sum = 0;
  int width = nums[0].size();
  for (int i = 0; i < width; i++) {
    if (ops[i] == '+') {
      for (const auto& n : nums)
        sum += n[i];
    } else {
      uint64_t product = 1;
      for (const auto& n : nums)
        product *= n[i];
      sum += product;
    }
  }

  std::cout << sum << "\n";
}

void part2() {
}
