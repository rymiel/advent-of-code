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
  int width = ops.size();
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

auto parse_part2() {
  auto lines = read_lines();
  auto ops = std::vector<char>{};
  auto offsets = std::vector<int>{};

  // read operations on last line
  {
    auto ss = std::stringstream(lines.back());
    char c;
    while (ss >> c) {
      ops.push_back(c);
      offsets.push_back(int(ss.tellg()) - 1);
    }
  }

  // offsets to the end of each column
  auto width = lines[0].size();
  std::rotate(offsets.begin(), offsets.begin() + 1, offsets.end());
  offsets.back() = width + 1;

  // read by columns
  auto num_lines = std::ranges::subrange(lines.begin(), lines.end() - 1);
  auto columns = std::vector<std::vector<std::string>>{};
  int i = 0;
  for (auto offset : offsets) {
    auto col = std::vector<std::string>{};
    for (auto line : num_lines)
      col.push_back(line.substr(i, offset - i - 1));
    i = offset;
    columns.push_back(col);
  }

  // read top-to-bottom in each column
  auto nums = std::vector<std::vector<int>>{};
  for (auto col : columns) {
    int width = col[0].size();
    auto n = std::vector<int>{};
    for (int i = 0; i < width; i++) {
      auto z = std::string{};
      for (auto s : col)
        z.push_back(s[i]);
      n.push_back(std::stoi(z));
    }
    nums.push_back(n);
  }

  return std::pair{nums, ops};
}

void part2() {
  auto [nums, ops] = parse_part2();

  uint64_t sum = 0;
  int width = ops.size();
  for (int i = 0; i < width; i++) {
    if (ops[i] == '+') {
      for (auto n : nums[i])
        sum += n;
    } else {
      uint64_t product = 1;
      for (auto n : nums[i])
        product *= n;
      sum += product;
    }
  }

  std::cout << sum << "\n";
}
