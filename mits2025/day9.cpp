#include <iostream>

int main() {
  int s, n;
  std::cin >> s >> n;

  int i, j;
  while (std::cin >> i >> j)
    if (s == i)
      s = j;
    else if (s == j)
      s = i;

  std::cout << s << "\n";
}
