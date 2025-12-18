#include <cstdint>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <set>
#include <utility>

using Coord = std::pair<int, int>;

using Walls = std::set<Coord>;

struct PQNode {
  Coord id;
  int64_t priority;

  auto operator<=>(const PQNode& other) const -> std::strong_ordering { return priority <=> other.priority; }
};

struct Result {
  std::map<Coord, int> dist;
};

auto neighbours(Coord c) {
  auto [x, y] = c;
  return std::array{Coord{x + 1, y}, Coord{x, y + 1}, Coord{x - 1, y}, Coord{x, y - 1}};
}

auto dijkstra(const Walls& walls, Coord start, Coord end, Coord bound) -> int {
  std::priority_queue<PQNode, std::vector<PQNode>, std::greater<>> queue;
  std::map<Coord, int> dist;

  dist.emplace(start, 0);
  queue.emplace(start, 0);

  while (!queue.empty()) {
    auto u = queue.top();
    queue.pop();

    if (u.id == end)
      return dist.at(u.id);

    auto n = neighbours(u.id);
    for (auto v : n) {
      if (v.first < 0 || v.second < 0 || v.first >= bound.first || v.second >= bound.second)
        continue;
      if (walls.contains(v))
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
  Coord start;
  Coord goal;
  Walls walls;

  int y = 0;
  int width;
  std::string s;
  while (std::getline(std::cin, s)) {
    width = s.size();
    for (int x = 0; x < s.size(); x++) {
      char c = s.at(x);
      if (c == 'L')
        start = {x, y};
      if (c == '#')
        walls.insert({x, y});
      if (c == 'V')
        goal = {x, y};
    }
    y++;
  }
  int height = y;

  auto r = dijkstra(walls, start, goal, Coord{width, height});

  std::cout << r << "\n";
}
