#include "lib.hpp"
#include "priority.hpp"
#include <cstdint>
#include <iostream>

#include "z3++.h"

int main() {
  int n, x, y;
  std::cin >> n >> x >> y;

  z3::context ctx;
  z3::optimize opt(ctx);

  auto n_sum = ctx.int_val(0);
  auto min_val = ctx.int_val(0);
  for (int i = 1; i < 20; i++) {
    uint64_t t = 2 * x + i * y + ipow(2ull, i) - 1;

    auto name = "a" + std::to_string(i);
    auto expr = ctx.int_const(name.c_str());
    opt.add(expr >= 0);
    n_sum = n_sum + (expr * i);
    min_val = min_val + (expr * ctx.int_val(t));
  }

  opt.add(n_sum == n);
  auto min = opt.minimize(min_val);

  std::cout << opt.check() << "\n";
  std::cout << opt.lower(min).as_uint64() << "\n";
}
