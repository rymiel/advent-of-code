#include <cctype>
#include <iostream>
#include <string>

int valid(const std::string& s) {
  bool upper = std::isupper(s[0]);
  for (char c : s) {
    if (upper != bool(std::isupper(c)))
      return false;
    upper = !upper;
  }
  return true;
}

int main() {
  std::string s;
  std::cin >> s;

  int parim = 0;
  int parim_idx = 0;
  for (int i = 0; i < s.size(); i++) {
    for (int j = parim + 1; (i + j) < s.size(); j++) {
      if (valid(s.substr(i, j))) {
        parim = j;
        parim_idx = i;
      } else {
        break;
      }
    }
  }

  std::cout << s.substr(parim_idx, parim) << "\n";
}
