#include "main.hpp"
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

auto parse() {
  std::vector<std::string> lines;
  std::string s;
  while (std::getline(std::cin, s)) {
    lines.push_back(s);
  }
  return lines;
}

void part1() {
  auto lines = parse();

  int sum = 0;
  for (const auto& s : lines) {
    auto digit1 = std::max_element(s.begin(), s.end() - 1);
    auto digit2 = std::max_element(digit1 + 1, s.end());
    sum += (*digit1 - '0') * 10 + (*digit2 - '0');
  }

  std::cout << sum << "\n";
}

void part2() {
  auto lines = parse();

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
