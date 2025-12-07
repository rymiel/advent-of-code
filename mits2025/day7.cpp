#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

int main() {
  int n;
  std::unordered_map<std::string, int> p{};
  std::cin >> n;
  for (int i = 0; i < n; i++) {
    std::string s;
    int k;
    std::cin.ignore(100, '\n');
    std::getline(std::cin, s, ':');
    std::cin >> k;
    p.emplace(s, k);
  }
  int count = 0;
  std::cin >> n;
  for (int i = 0; i < n; i++) {
    std::cin.ignore(100, ' ');
    std::string s;
    std::getline(std::cin, s);
    std::stringstream ss{s};
    std::string z;
    int skoor = 0;
    while (std::getline(ss, z, ',')) {
      skoor += p.at(z);
      ss.ignore(100, ' ');
    }
    if (skoor > 10)
      count++;
  }

  std::cout << count << "\n";
}
