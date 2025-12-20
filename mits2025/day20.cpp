#include <algorithm>
#include <iomanip>
#include <iostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

bool palin(const std::string& s) {
  auto r = s;
  std::reverse(r.begin(), r.end());
  return r == s;
}

int main() {
  std::cin.ignore(1, '[');
  std::string s;
  std::vector<std::string> v;
  while (std::cin >> std::quoted(s)) {
    std::cin.ignore(1, ',') >> std::ws;
    v.push_back(s);
  }

  std::set<std::pair<std::string, std::string>> res;
  for (size_t i = 0; i < v.size(); i++) {
    for (size_t j = i + 1; j < v.size(); j++) {
      auto [a, b] = std::minmax(v[i], v[j]);
      if (palin(a + b) || palin(b + a))
        res.insert({a, b});
    }
  }

  bool flag = false;
  std::cout << '[';
  for (auto [a, b] : res) {
    if (flag)
      std::cout << ", ";
    std::cout << '[' << std::quoted(a) << ", " << std::quoted(b) << ']';
    flag = true;
  }
  std::cout << "]\n";
}
