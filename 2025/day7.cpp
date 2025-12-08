#include "main.hpp"
#include "map2d.hpp"
#include <iostream>
#include <map>
#include <set>

auto parse() { return Map2D<char>::from_lines(read_lines()); }

Point left = {-1, 0};
Point right = {1, 0};
Point down = {0, 1};

void part1() {
  auto map = parse();

  // initial beam
  for (auto p : map.points())
    if (map[p] == 'S')
      map[p + down] = '|';

  std::set<Point> splits;
  while (true) {
    auto prev = map; // copy
    for (auto p : map.points()) {
      if (map[p] == '|') {
        switch (map.get_or(p + down, '\0')) {
        case '^':
          map[p + down + left] = '|';
          map[p + down + right] = '|';
          splits.insert(p + down);
          break;
        case '.': map[p + down] = '|';
        }
      }
    }
    if (map == prev)
      break;
  }

  map.print();
  std::cout << splits.size() << "\n";
}

Point cast_down(Map2D<char>& map, Point s) {
  while (true) {
    s = s + down;
    if (map.get_or(s, '\0') != '.')
      return s;
  }
}

void part2() {
  auto map = parse();
  auto splits = std::map<Point, uint64_t>{};

  // out of bounds states
  for (int x = 0; x < map.width; x++)
    splits[{x, map.height}] = 1;

  // bottom up
  for (int y = map.height - 1; y >= 0; y--) {
    for (int x = 0; x < map.width; x++) {
      Point p = {x, y};
      if (map[p] == '^') {
        uint64_t splits_left = splits.at(cast_down(map, p + left));
        uint64_t splits_right = splits.at(cast_down(map, p + right));
        splits[p] = splits_left + splits_right;
      }
    }
  }

  Point start{};
  for (auto p : map.points())
    if (map[p] == 'S')
      start = p;

  std::cout << splits.at(cast_down(map, start)) << "\n";
}
