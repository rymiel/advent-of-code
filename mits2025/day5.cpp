#include <iostream>
#include <string>
#include <utility>
#include <vector>

int main() {
  std::vector<std::string> read;

  std::string s;
  while (std::getline(std::cin, s))
    read.push_back(s);

  std::vector<std::pair<int, int>> tulemus = {};
  for (int rida = 0; rida < read.size(); rida++) {
    std::string s = read[rida];
    for (int veerg = 0; veerg < s.size(); veerg++) {
      char c = s[veerg];
      if (c == '0') {
        if (veerg == 0 || s[veerg - 1] == '0' || s[veerg - 1] == ' ') {
          if (veerg == s.size() - 1 || s[veerg + 1] == '0' || s[veerg + 1] == ' ') {
            int tegelik_veerg = veerg + 1;
            if (tegelik_veerg > 9)
              tegelik_veerg--;
            if (tegelik_veerg > 4)
              tegelik_veerg--;
            tulemus.push_back({rida + 1, tegelik_veerg});
          }
        }
      }
    }
  }

  std::cout << '[';
  bool flag = false;
  for (auto [x, y] : tulemus) {
    if (flag)
      std::cout << ", ";
    std::cout << '(' << x << ", " << y << ')';
    flag = true;
  }
  std::cout << "]\n";
}
