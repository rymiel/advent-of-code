#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>

int main() {
  std::string s;
  std::getline(std::cin, s);
  std::stringstream ss{s};
  int i;
  std::vector<int> v;
  while (ss >> i)
    v.push_back(i);
  int k;
  std::cin >> k;

  uint64_t count = 0;
  for (int i = 0; i < v.size(); i++) {
    for (int j = i + 1; j <= v.size(); j++) {
      auto r = std::accumulate(v.begin() + i, v.begin() + j, 0, std::bit_xor<>{});
      if (r == k)
        count++;
    }
  }
  std::cout << count << "\n";
}
