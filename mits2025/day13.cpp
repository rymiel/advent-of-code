#include <iostream>
#include <vector>

int main() {
  char c;
  long count = 0;
  long i = 1;
  std::cin >> c;
  while (true) {
    std::vector<char> a(i);
    std::vector<char> b(i);

    for (long j = 0; j < i; j++)
      std::cin >> a[j];
    for (long j = 0; j < i; j++)
      std::cin >> b[j];

    for (size_t i = 0; i < a.size(); i++)
      if (a[i] != b[b.size() - 1 - i])
        count++;

    if (std::cin.peek() != '0' && std::cin.peek() != '1')
      break;

    i *= 2;
  }

  std::cout << count << "\n";
}
