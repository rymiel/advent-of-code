#include "lib.hpp"
#include "main.hpp"
#include <ranges>
#include <sstream>

void part1() {
  int count = 0;
  auto lines = read_lines();
  auto last_empty = std::find(lines.rbegin(), lines.rend(), "");
  for (auto line : std::ranges::subrange{lines.rbegin(), last_empty}) {
    std::stringstream ss{line};
    int x, y, i, sum = 0;
    (ss >> x).ignore(1, 'x');
    (ss >> y).ignore(1, ':');
    while (ss >> i)
      sum += i;
    x -= x % 3;
    y -= y % 3;
    int area = (x / 3) * (y / 3);
    if (area >= sum)
      count++;
  }
  std::cout << count << "\n";
}

void part2() {}
