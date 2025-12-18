#pragma once

#include <concepts>
#include <iostream>
#include <string>
#include <vector>

template <std::integral T> T ipow(T base, int exp) {
  T result = 1;
  for (;;) {
    if (exp & 1)
      result *= base;
    exp >>= 1;
    if (!exp)
      break;
    base *= base;
  }

  return result;
}

template <std::integral T> T mod(T x, T y) {
  T m = x % y;
  return m + (m < 0 ? y : 0);
}

inline auto read_lines() -> std::vector<std::string> {
  std::vector<std::string> lines;
  std::string s;
  while (std::getline(std::cin, s))
    lines.push_back(s);
  return lines;
}
