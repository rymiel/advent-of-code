#pragma once

#include <functional>
#include <iostream>
#include <ostream>
#include <vector>

struct Point {
  int x, y;

  auto operator+(const Point& other) const -> Point { return {x + other.x, y + other.y}; }
  auto operator<=>(const Point& other) const = default;

  friend std::istream& operator>>(std::istream& io, Point& p) {
    std::ws(io) >> p.x;
    return io.ignore(1, ',') >> p.y;
  }

  friend std::ostream& operator<<(std::ostream& s, const Point& p) { return s << '(' << p.x << ',' << p.y << ')'; }

  static std::array<Point, 8> neighbours;
  static std::array<Point, 4> cardinals;
};

inline std::array<Point, 8> Point::neighbours = {Point{-1, -1}, Point{-1, 0}, Point{-1, 1}, Point{0, -1},
                                                 Point{0, 1},   Point{1, -1}, Point{1, 0},  Point{1, 1}};
inline std::array<Point, 4> Point::cardinals = {Point{-1, 0}, Point{0, -1}, Point{0, 1}, Point{1, 0}};

class PointIota {
  Point bound;

public:
  class Iterator {
    int n;
    int w;

  public:
    Iterator(int n, int w) : n{n}, w{w} {}

    using iterator_category = std::forward_iterator_tag;
    using value_type = Point;

    value_type operator*() const {
      auto [q, r] = std::div(n, w);
      return Point{r, q};
    }

    Iterator& operator++() {
      n++;
      return *this;
    }

    Iterator operator++(int) {
      Iterator tmp = *this;
      ++(*this);
      return tmp;
    }

    friend bool operator==(const Iterator& a, const Iterator& b) { return a.n == b.n && a.w == b.w; };
    friend bool operator!=(const Iterator& a, const Iterator& b) { return a.n != b.n || a.w != b.w; };
  };

  explicit PointIota(Point bound) : bound{bound} {}

  Iterator begin() { return Iterator(0, bound.x); }
  Iterator end() { return Iterator(bound.x * bound.y, bound.x); }
};

template <typename T> class Map2D {
public:
  int width;
  int height;

private:
  std::vector<T> m_data;

public:
  Map2D(int width, int height) : width{width}, height{height}, m_data{std::vector<T>(width * height)} {}

  auto in_bounds(Point p) const -> bool { return p.x >= 0 && p.y >= 0 && p.x < width && p.y < height; }

  auto operator[](Point p) -> std::vector<T>::reference { return m_data.at(p.y * width + p.x); }

  auto operator[](Point p) const -> std::vector<T>::const_reference { return m_data.at(p.y * width + p.x); }

  auto operator<=>(const Map2D<T>& other) const = default;

  auto get_or(Point p, T def) const {
    if (!in_bounds(p))
      return def;
    return T(operator[](p));
  }

  auto points() const { return PointIota{{width, height}}; }

  void print(std::ostream& os = std::cout) const {
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++)
        os << operator[]({x, y});
      os << '\n';
    }
  }

  template <std::invocable<char, Point> Fn>
  static auto from_lines(const std::vector<std::string>& lines, const Fn& fn) -> Map2D<T> {
    int width = lines.at(0).size();
    int height = lines.size();
    auto map = Map2D<T>(width, height);
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        int c = lines[y][x];
        map[{x, y}] = fn(c, Point{x, y});
      }
    }
    return map;
  }

  template <std::invocable<char> Fn = std::identity>
  static auto from_lines(const std::vector<std::string>& lines, const Fn& fn = {}) -> Map2D<T> {
    return from_lines(lines, [&fn](char c, Point) { return fn(c); });
  }
};
