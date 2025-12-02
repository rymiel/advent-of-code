#include <concepts>

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
