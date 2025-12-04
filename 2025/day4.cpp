#include "main.hpp"
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

struct Point {
  int x, y;

  auto operator+(const Point& other) const -> Point { return {x + other.x, y + other.y}; }
};

std::array<Point, 8> neighbours = {Point{-1, -1}, Point{-1, 0}, Point{-1, 1}, Point{0, -1},
                                   Point{0, 1},   Point{1, -1}, Point{1, 0},  Point{1, 1}};

template <typename T> class Map2D {
public:
  int width;
  int height;

private:
  std::vector<T> m_data;

public:
  Map2D(int width, int height) : width{width}, height{height}, m_data{std::vector<T>(width * height)} {}

  auto in_bounds(Point p) -> bool { return p.x >= 0 && p.y >= 0 && p.x < width && p.y < height; }

  auto operator[](Point p) -> std::vector<T>::reference { return m_data.at(p.y * width + p.x); }

  auto get_or(Point p, T def) {
    if (!in_bounds(p))
      return def;
    return T(operator[](p));
  }

  static auto from_lines(std::vector<std::string> lines, const std::invocable<char> auto& fn) -> Map2D<T> {
    int width = lines.at(0).size();
    int height = lines.size();
    auto map = Map2D<T>(width, height);
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        int c = lines[y][x];
        map[{x, y}] = fn(c);
      }
    }
    return map;
  }
};

auto parse() {
  return Map2D<bool>::from_lines(read_lines(), [](char c) { return c == '@'; });
}

void part1() {
  auto map = parse();

  int sum = 0;
  for (int y = 0; y < map.height; y++) {
    for (int x = 0; x < map.width; x++) {
      Point p{x, y};
      if (!map.get_or(p, false))
        continue;
      int count = std::ranges::count_if(neighbours, [&](Point d) { return map.get_or(p + d, false); });
      if (count < 4)
        sum++;
    }
  }

  std::cout << sum << "\n";
}

void part2() {
  auto map = parse();

  int sum = 0;
  std::vector<Point> remove{};
  while (true) {
    remove.clear();
    for (int y = 0; y < map.height; y++) {
      for (int x = 0; x < map.width; x++) {
        Point p{x, y};
        if (!map.get_or(p, false))
          continue;
        int count = std::ranges::count_if(neighbours, [&](Point d) { return map.get_or(p + d, false); });
        if (count < 4)
          remove.push_back(p);
      }
    }
    if (remove.size() == 0)
      break;
    for (Point p : remove)
      map[p] = false;
    sum += remove.size();
  }

  std::cout << sum << "\n";
}
