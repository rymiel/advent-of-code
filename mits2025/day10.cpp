#include <algorithm>
#include <iostream>
#include <string>

int main() {
  std::string s;
  std::getline(std::cin, s);

  std::string cmd;
  char c;
  int i;
  while (std::cin >> cmd) {
    if (cmd == "EEMALDA") {
      std::cin >> c;
      std::erase(s, c);
    } else if (cmd == "LISA") {
      std::cin >> c;
      s.push_back(c);
    } else if (cmd == "LIIGUTA") {
      std::cin >> cmd >> i;
      if (cmd == "VASAKULE")
        i *= -1;
      i = i % int(s.size());

      if (i > 0) {
        std::rotate(s.rbegin(), s.rbegin() + i, s.rend());
      } else {
        std::rotate(s.begin(), s.begin() - i, s.end());
      }
    }
  }

  std::cout << s << "\n";
}
