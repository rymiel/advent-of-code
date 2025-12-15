#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <vector>

int main() {
  std::vector<uint8_t> nums;
  int i;
  while (std::cin >> i)
    nums.push_back(i);

  int w = nums.size();
  std::vector<uint8_t> puu(w * 2 - 1, 0);

  for (size_t j = w - 1; j < puu.size(); j++)
    puu.at(j) = nums.at(j - w + 1);

  for (int j = puu.size() - 1; j >= 0; j--)
    if (puu.at(j) == 1)
      puu.at((j - 1) / 2) = 1;

  std::vector<int> viga{};
  for (int j = puu.size() - 1; j >= 0; j--)
    if (puu.at(j) == 0 && puu.at((j - 1) / 2) == 1)
      viga.push_back(j);

  std::ranges::sort(viga);
  for (auto i : viga)
    std::cout << i << " ";
  std::cout << "\n";
}
