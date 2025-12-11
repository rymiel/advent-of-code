#include "lib.hpp"
#include "main.hpp"
#include <cassert>
#include <cstdint>
#include <deque>
#include <ranges>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>

uint32_t small_string(const std::string& s) {
  assert(s.size() == 3);
  return uint32_t(s[0] << 16) | (s[1] << 8) | (s[2]);
}

auto parse() {
  std::unordered_multimap<uint32_t, uint32_t> graph{};
  std::unordered_multimap<uint32_t, uint32_t> rev_graph{};

  auto lines = read_lines();
  for (auto line : lines) {
    std::stringstream ss{line};
    std::string key, value;
    std::getline(ss, key, ':');
    uint32_t key_id = small_string(key);
    while (ss >> value) {
      uint32_t value_id = small_string(value);
      graph.insert({key_id, value_id});
      rev_graph.insert({value_id, key_id});
    }
  }

  return std::pair{graph, rev_graph};
}

void part1() {
  auto [graph, rev_graph] = parse();

  std::unordered_map<uint32_t, uint64_t> count{};
  std::deque<uint32_t> queue{};

  uint32_t you = small_string("you");
  uint32_t out = small_string("out");
  queue.push_back(you);

  while (!queue.empty()) {
    auto top = queue.front();
    std::erase(queue, top);

    count[top] = top == you ? 1 : 0;
    auto [parents_begin, parents_end] = rev_graph.equal_range(top);
    for (auto parent : std::ranges::subrange{parents_begin, parents_end})
      count[top] += count[parent.second];

    auto [children_begin, children_end] = graph.equal_range(top);
    for (auto child : std::ranges::subrange{children_begin, children_end})
      queue.push_back(child.second);
  }

  std::cout << count[out] << "\n";
}

struct Counter {
  uint64_t none, dac, fft, both;

  auto operator+(const Counter& q) const -> Counter {
    return {.none = none + q.none, .dac = dac + q.dac, .fft = fft + q.fft, .both = both + q.both};
  }

  auto mark_dac() const -> Counter { return {.none = 0, .dac = none + dac, .fft = 0, .both = fft + both}; }
  auto mark_fft() const -> Counter { return {.none = 0, .dac = 0, .fft = none + fft, .both = dac + both}; }
};

void part2() {
  auto [graph, rev_graph] = parse();

  std::unordered_map<uint32_t, Counter> count{};
  std::deque<uint32_t> queue{};

  uint32_t svr = small_string("svr");
  uint32_t out = small_string("out");
  uint32_t fft = small_string("fft");
  uint32_t dac = small_string("dac");
  queue.push_back(svr);

  while (!queue.empty()) {
    auto top = queue.front();
    std::erase(queue, top);

    count[top] = top == svr ? Counter{.none = 1} : Counter{};
    auto [parents_begin, parents_end] = rev_graph.equal_range(top);
    for (auto parent : std::ranges::subrange{parents_begin, parents_end})
      count[top] = count[top] + count[parent.second];

    if (top == fft)
      count[top] = count[top].mark_fft();
    if (top == dac)
      count[top] = count[top].mark_dac();

    auto [children_begin, children_end] = graph.equal_range(top);
    for (auto child : std::ranges::subrange{children_begin, children_end})
      queue.push_back(child.second);
  }

  std::cout << count.at(out).both << "\n";
}
