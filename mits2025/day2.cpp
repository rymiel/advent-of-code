#include <iostream>
#include <sstream>
#include <string>

int main() {
  std::string a, b;
  std::getline(std::cin, a);
  std::getline(std::cin, b);

  std::stringstream as{a};
  std::stringstream bs{b};

  int sum = 0;
  int x = 0, y = 0;
  while (as >> x && bs >> y) {
    std::cout << x << ", " << y << "\n";
    sum += x * y;
  }

  std::cout << sum << "\n";
}
