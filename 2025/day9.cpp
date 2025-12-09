#include "main.hpp"
#include "map2d.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

auto parse() {
  std::vector<Point> points{};
  Point p{};
  while (std::cin >> p)
    points.push_back(p);
  return points;
}

void part1() {
  auto points = parse();
  int size = points.size();

  long best = 0;
  for (int i = 0; i < size; i++) {
    auto& u = points.at(i);
    for (int j = i + 1; j < size; j++) {
      auto& v = points.at(j);
      long xdiff = std::abs(u.x - v.x) + 1;
      long ydiff = std::abs(u.y - v.y) + 1;
      best = std::max(best, xdiff * ydiff);
    }
  }

  std::cout << best << "\n";
}

struct Rect {
  Point a, b;

  int left() { return std::min(a.x, b.x); }
  int right() { return std::max(a.x, b.x); }
  int top() { return std::min(a.y, b.y); }
  int bottom() { return std::max(a.y, b.y); }

  Rect shrink() { return Rect{{left() + 1, top() + 1}, {right() - 1, bottom() - 1}}; }
};

bool collide(Rect a, Rect b) {
  return a.left() <= b.right() && a.right() >= b.left() && a.bottom() >= b.top() && a.top() <= b.bottom();
}

void part2() {
  auto points = parse();
  int size = points.size();

  std::vector<Rect> lines{};

  for (int i = 0; i < size; i++) {
    auto& a = points.at(i);
    auto& b = points.at((i + 1) % size);
    lines.push_back(Rect{a, b});
  }

  long best = 0;
  for (int i = 0; i < size; i++) {
    auto& u = points.at(i);
    for (int j = i + 1; j < size; j++) {
      auto& v = points.at(j);
      long xdiff = std::abs(u.x - v.x) + 1;
      long ydiff = std::abs(u.y - v.y) + 1;
      long area = xdiff * ydiff;
      if (area > best) {
        auto rect = Rect{u, v}.shrink();
        bool collides = std::ranges::any_of(lines, [&rect](const auto& line) { return collide(line, rect); });
        if (!collides)
          best = area;
      }
    }
  }

  std::cout << best << "\n";
}
