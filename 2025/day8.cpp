#include "main.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <istream>
#include <numeric>
#include <vector>

struct Point3D {
  int x, y, z;

  auto distance_squared(const Point3D& q) const -> double {
    return std::pow(x - q.x, 2) + std::pow(y - q.y, 2) + std::pow(z - q.z, 2);
  }

  friend std::istream& operator>>(std::istream& io, Point3D& p) {
    std::ws(io);
    io >> p.x;
    io.ignore(1, ',');
    io >> p.y;
    io.ignore(1, ',');
    io >> p.z;
    return io;
  }

  friend std::ostream& operator<<(std::ostream& s, const Point3D& p) {
    return s << '(' << p.x << ',' << p.y << ',' << p.z << ')';
  }
};

struct Edge {
  double distance;
  int id_u, id_v;

  auto operator<=>(const Edge&) const = default;
};

int repr(std::vector<int>& p, int id) {
  if (p[id] == id)
    return id;
  int x = repr(p, p[id]);
  p[id] = x;
  return x;
}

auto parse() {
  std::vector<Point3D> points{};
  Point3D p{};
  while (std::cin >> p)
    points.push_back(p);
  return points;
}

auto make_edges(const std::vector<Point3D>& points) {
  auto edges = std::vector<Edge>{};
  int size = points.size();
  auto p = std::vector<int>(size);
  for (int i = 0; i < size; i++) {
    p[i] = i;
    auto& u = points.at(i);
    for (int j = i + 1; j < size; j++) {
      auto& v = points.at(j);
      edges.push_back(Edge{u.distance_squared(v), i, j});
    }
  }
  std::ranges::sort(edges);

  return std::pair{edges, p};
}

void part1() {
  auto points = parse();
  int size = points.size();
  auto [edges, p] = make_edges(points);

  int connections_to_make = size < 100 ? 10 : 1'000; // use 10 for test input
  for (int i = 0; i < connections_to_make; i++) {
    auto& edge = edges.at(i);
    int ru = repr(p, edge.id_u);
    int rv = repr(p, edge.id_v);
    if (ru != rv)
      p[ru] = rv;
  }

  for (int i = 0; i < size; i++)
    p[i] = repr(p, i);

  std::vector<int> sizes(size, 0);
  for (int i = 0; i < size; i++)
    sizes[i] = std::ranges::count(p, i);

  std::ranges::sort(sizes, std::greater<>{});

  auto result = std::accumulate(sizes.begin(), sizes.begin() + 3, 1ull, std::multiplies<>{});
  std::cout << result << "\n";
}

void part2() {
  auto points = parse();
  auto [edges, p] = make_edges(points);

  Edge last_connection{};
  for (auto& edge : edges) {
    int ru = repr(p, edge.id_u);
    int rv = repr(p, edge.id_v);
    if (ru != rv) {
      p[ru] = rv;
      last_connection = edge;
    }
  }

  auto& u = points.at(last_connection.id_u);
  auto& v = points.at(last_connection.id_v);
  std::cout << long(u.x) * v.x << "\n";
}
