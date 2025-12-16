#include <algorithm>
#include <cstdint>
#include <iostream>
#include <limits>
#include <queue>
#include <sstream>
#include <unordered_map>

using Id = std::string;

struct Edge {
  Id id;
  int time;
  std::vector<int> out;
};

using Graph = std::unordered_multimap<Id, Edge>;

struct PQNode {
  Id id;
  int64_t priority;

  auto operator<=>(const PQNode& other) const -> std::strong_ordering { return priority <=> other.priority; }
};

struct Result {
  std::unordered_map<Id, int> dist;
  std::unordered_map<Id, Id> came_from;
};

auto dijkstra(const Graph& graph, Id start) -> Result {
  std::priority_queue<PQNode, std::vector<PQNode>, std::greater<>> queue;
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
      auto time = (dist.at(u.id) + 720) % 3600;
      auto next = std::ranges::lower_bound(v.out, time);
      auto save = next == v.out.end() ? 3600 + *v.out.begin() - time : *next - time;
      auto alt = dist.at(u.id) + v.time + save;
      auto find_v = dist.find(v.id);
      auto best = find_v == dist.end() ? std::numeric_limits<int64_t>::max() : find_v->second;
      if (alt < best) {
        dist[v.id] = alt;
        came_from[v.id] = u.id;
        queue.emplace(v.id, alt);
      }
    }
  }

  return {dist, came_from};
}

int main() {
  Graph G{};

  std::string s;
  while (std::getline(std::cin, s)) {
    std::stringstream ss{s};
    std::string a, b, _;
    std::ws(ss);
    std::getline(ss, a, '@');
    a.erase(a.size() - 1);
    std::ws(ss);
    std::getline(ss, b, '@');
    b.erase(b.size() - 1);

    int h, m;
    ss >> h;
    ss.ignore();
    ss >> m;
    ss.ignore();
    ss >> _ >> _;
    std::vector<int> out;
    int i, j;
    while (ss >> i) {
      j = i * 60;
      ss.ignore(1, ':');
      ss >> i;
      out.push_back(j + i);
    }

    G.insert({a, Edge{b, h * 60 + m, out}});
  }

  auto r = dijkstra(G, "Tartu");

  Id w = "Tallinn";
  std::vector<Id> path{w};
  while (w != "Tartu") {
    w = r.came_from[w];
    path.push_back(w);
  }

  std::reverse(path.begin(), path.end());

  for (auto i : path)
    std::cout << i << " ";
  std::cout << r.dist.at("Tallinn");
  std::cout << "\n";
}
