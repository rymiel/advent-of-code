#include "lib.hpp"
#include "main.hpp"
#include <algorithm>
#include <bitset>
#include <iostream>
#include <istream>
#include <numeric>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "z3++.h"

using Node = std::bitset<16>;
using Toggles = std::vector<std::set<int>>;
using Joltage = std::vector<int>;

using Line = std::tuple<Node, Toggles, Joltage>;
auto parse_line(const std::string& s) -> Line {
  std::stringstream ss{s};
  ss.ignore(-1u, '[');
  std::string lights_string{};
  std::getline(ss, lights_string, ']');
  Node lights;
  for (size_t i = 0; i < lights_string.size(); i++)
    if (lights_string.at(i) == '#')
      lights.set(i);

  std::ws(ss);
  Toggles toggles;
  int i;
  while (ss.peek() == '(') {
    std::set<int> toggle;
    ss.ignore(1, '(');
    while (ss.peek() != ')') {
      if (ss.peek() == ',')
        ss.ignore(1, ',');
      ss >> i;
      toggle.insert(i);
    }
    toggles.push_back(toggle);
    ss.ignore(1, ')');
    std::ws(ss);
  }

  ss.ignore(-1u, '{');
  std::vector<int> joltage;
  while (ss.peek() != '}') {
    if (ss.peek() == ',')
      ss.ignore(1, ',');
    ss >> i;
    joltage.push_back(i);
  }

  return std::tuple{lights, toggles, joltage};
}

auto parse() {
  auto lines = read_lines();
  std::vector<Line> results;
  for (const auto& line : lines)
    results.emplace_back(parse_line(line));

  return results;
}

int INF = 1'000'000'000;

int solve_part1(const Node& lights, const Toggles& t, size_t n) {
  if (n == t.size())
    return lights.none() ? 0 : INF;

  int no_n = solve_part1(lights, t, n + 1);
  Node new_lights = lights;
  for (int i : t[n])
    new_lights.flip(i);
  int yes_n = 1 + solve_part1(new_lights, t, n + 1);
  return std::min(no_n, yes_n);
}

void part1() {
  auto data = parse();

  int count = 0;
  for (const auto& [lights, toggles, _] : data) {
    auto res = solve_part1(lights, toggles, 0);
    count += res;
  }

  std::cout << count << "\n";
}

void part2() {
  auto data = parse();

  int count = 0;
  for (const auto& [_, toggles, joltage] : data) {
    z3::context ctx;
    z3::optimize opt(ctx);

    std::vector<z3::expr> variables{};
    for (unsigned i = 0; i < toggles.size(); i++) {
      auto name = std::string{char('a' + i)};
      auto expr = ctx.int_const(name.c_str());
      opt.add(expr >= 0);
      variables.push_back(expr);
    }
    auto min_sum = opt.minimize(std::accumulate(variables.begin(), variables.end(), ctx.int_val(0)));

    for (unsigned i = 0; i < joltage.size(); i++) {
      z3::expr sum = ctx.int_val(0);
      for (unsigned j = 0; j < toggles.size(); j++)
        if (toggles[j].contains(i))
          sum = sum + variables[j];
      opt.add(sum == joltage[i]);
    }

    std::cout << opt.check() << ": " << opt.lower(min_sum) << "\n";
    count += opt.lower(min_sum).as_int64();
  }

  std::cout << count << "\n";
}
