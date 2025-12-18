#include "priority.hpp"
#include <cstdint>
#include <iostream>
#include <limits>
#include <sstream>
#include <unordered_map>

using Id = std::string;

using Graph = std::unordered_multimap<Id, Id>;

struct Result {
  std::unordered_map<Id, int> dist;
  std::unordered_map<Id, Id> came_from;
};

auto dijkstra(const Graph& graph, Id start) -> Result {
  GreaterPriorityQueue<Id> queue;
  std::unordered_map<Id, int> dist;
  std::unordered_map<Id, Id> came_from;

  dist.emplace(start, 0);
  queue.emplace(start, 0);

  while (!queue.empty()) {
    auto u = queue.top();
    queue.pop();

    auto [begin, end] = graph.equal_range(u.id);
    for (auto it = begin; it != end; ++it) {
      auto v = it->second;
      auto alt = dist.at(u.id) + 1;
      auto find_v = dist.find(v);
      auto best = find_v == dist.end() ? std::numeric_limits<int64_t>::max() : find_v->second;
      if (alt < best) {
        dist[v] = alt;
        came_from[v] = u.id;
        queue.emplace(v, alt);
      }
    }
  }

  return {dist, came_from};
}

int main() {
  Graph G{};

  std::string s, a, b;
  while (std::getline(std::cin, s)) {
    std::stringstream ss{s};
    std::getline(ss, a, ':');
    while (std::getline(ss >> std::ws, b, ','))
      G.insert({a, b});
  }

  auto r = dijkstra(G, "New York");

  std::cout << r.dist.at("Tartu") - 1 << "\n";
}
