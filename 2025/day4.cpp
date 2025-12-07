#include "main.hpp"
#include "map2d.hpp"
#include <algorithm>
#include <iostream>
#include <vector>

auto parse() {
  return Map2D<bool>::from_lines(read_lines(), [](char c) { return c == '@'; });
}

void part1() {
  auto map = parse();

  int sum = 0;
  for (auto p : map.points()) {
    if (!map.get_or(p, false))
      continue;
    int count = std::ranges::count_if(Point::neighbours, [&](Point d) { return map.get_or(p + d, false); });
    if (count < 4)
      sum++;
  }

  std::cout << sum << "\n";
}

void part2() {
  auto map = parse();

  int sum = 0;
  std::vector<Point> remove{};
  while (true) {
    remove.clear();
    for (auto p : map.points()) {
      if (!map.get_or(p, false))
        continue;
      int count = std::ranges::count_if(Point::neighbours, [&](Point d) { return map.get_or(p + d, false); });
      if (count < 4)
        remove.push_back(p);
    }
    if (remove.size() == 0)
      break;
    for (Point p : remove)
      map[p] = false;
    sum += remove.size();
  }

  std::cout << sum << "\n";
}
