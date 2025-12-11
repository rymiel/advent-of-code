#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

int main() {
  std::vector<std::string> lines;
  std::unordered_map<std::string, int> count{};
  std::string s;
  while (std::getline(std::cin, s))
    count[s]++;

  int rem = 0;
  for (auto [k, v] : count) {
    rem += v % 2;
  }

  std::cout << rem / 2 << "\n";
}
