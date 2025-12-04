#include "main.hpp"
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <string>

void part1() {
  auto lines = read_lines();

  int sum = 0;
  for (const auto& s : lines) {
    auto digit1 = std::max_element(s.begin(), s.end() - 1);
    auto digit2 = std::max_element(digit1 + 1, s.end());
    sum += (*digit1 - '0') * 10 + (*digit2 - '0');
  }

  std::cout << sum << "\n";
}

void part2() {
  auto lines = read_lines();

  uint64_t sum = 0;
  for (const auto& s : lines) {
    uint64_t num = 0;
    auto iter = s.begin();
    for (int i = 11; i >= 0; i--) {
      auto digit = std::max_element(iter, s.end() - i);
      iter = digit + 1;
      num = num * 10 + (*digit - '0');
    }
    sum += num;
  }

  std::cout << sum << "\n";
}
