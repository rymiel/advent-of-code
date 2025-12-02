#include <iostream>

int main() {
  std::string s;
  std::cin >> s;

  int count = 0;
  char prev = '.';
  for (auto c : s) {
    if (c == '*' && prev == '.')
      count++;
    prev = c;
  }

  std::cout << count << "\n";
}
