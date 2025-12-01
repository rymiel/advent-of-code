#include "main.hpp"
#include <iostream>
#include <vector>

auto parse() {
  std::vector<int> commands;
  char c;
  int n;
  while (std::cin >> c >> n) {
    if (c == 'L')
      n = -n;
    commands.push_back(n);
  }
  return commands;
}

int mod(int x, int y) {
  int m = x % y;
  return m + (m < 0 ? y : 0);
}

void part1() {
  auto commands = parse();

  int count = 0;
  int pointer = 50;
  for (int c : commands) {
    pointer = mod((pointer + c), 100);
    if (pointer == 0)
      count++;
  }

  std::cout << count << "\n";
}

void part2() {
  auto commands = parse();

  int count = 0;
  int pointer = 50;
  for (int c : commands) {
    int neg_diff = 0 - pointer;
    if (neg_diff == 0)
      neg_diff = -100;
    int pos_diff = 100 - pointer;
    pointer = mod((pointer + c), 100);

    if (c < 0 && c <= neg_diff)
      count += -(c - neg_diff) / 100 + 1;
    else if (c > 0 && c >= pos_diff)
      count += (c - pos_diff) / 100 + 1;
  }

  std::cout << count << "\n";
}
