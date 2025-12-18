#include "lib.hpp"
#include "map2d.hpp"
#include <cstdint>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <utility>

using Maze = Map2D<bool>;

struct PQNode {
  Point id;
  int64_t priority;

  auto operator<=>(const PQNode& other) const -> std::strong_ordering { return priority <=> other.priority; }
};

auto dijkstra(const Maze& maze, Point start, Point goal) -> int {
  std::priority_queue<PQNode, std::vector<PQNode>, std::greater<>> queue;
  std::map<Point, int> dist;

  dist.emplace(start, 0);
  queue.emplace(start, 0);

  while (!queue.empty()) {
    auto u = queue.top();
    queue.pop();

    if (u.id == goal)
      return dist.at(u.id);

    for (auto c : Point::cardinals) {
      auto v = u.id + c;
      if (maze.get_or(v, true))
        continue;

      auto alt = dist.at(u.id) + 1;
      auto find_v = dist.find(v);
      auto best = find_v == dist.end() ? std::numeric_limits<int64_t>::max() : find_v->second;
      if (alt < best) {
        dist[v] = alt;
        queue.emplace(v, alt);
      }
    }
  }

  std::terminate();
}

int main() {
  Point start;
  Point goal;
  Maze maze = Maze::from_lines(read_lines(), [&start, &goal](char c, Point p) {
    switch (c) {
    case '#': return true;
    case 'L': start = p; break;
    case 'V': goal = p; break;
    }
    return false;
  });

  auto r = dijkstra(maze, start, goal);

  std::cout << r << "\n";
}
