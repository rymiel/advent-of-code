#include <iostream>
#include <numeric>
#include <vector>

int main() {
  int x, y, z, n;
  std::cin >> x >> y >> z >> n;
  std::vector<int> a(n);
  for (int i = 0; i < n; i++)
    std::cin >> a[i];

  int w = std::accumulate(a.begin(), a.end(), y);
  int rem = x - w;
  std::cout << rem / z << "\n";
}
