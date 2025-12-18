
#include <compare>
#include <cstdint>
#include <functional>
#include <queue>

template <typename T> struct PQNode {
  T id;
  int64_t priority;

  auto operator<=>(const PQNode& other) const -> std::strong_ordering { return priority <=> other.priority; }
};

template <typename T>
using GreaterPriorityQueue = std::priority_queue<PQNode<T>, std::vector<PQNode<T>>, std::greater<>>;
