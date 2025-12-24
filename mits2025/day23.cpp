#include <iostream>
#include <string>
#include <vector>

struct Swap {
  int start_a, start_b;
  bool pos_a, pos_b;
  int size;
};

int main() {
  std::string str;
  std::cin >> str;
  int n;
  std::cin >> n;
  std::vector<Swap> swaps;
  for (int i = 0; i < n; i++) {
    int a, b, c, d;
    std::cin >> a;
    std::cin.ignore(1, '-') >> b;
    std::cin >> c;
    std::cin.ignore(1, '-') >> d;

    bool a_pos = b > a;
    bool b_pos = d > c;
    swaps.push_back(Swap{a, c, a_pos, b_pos, std::abs(b - a) + 1});
  }

  int count = 0;
  for (auto s : swaps) {
    for (int i = 0; i < s.size; i++) {
      auto& a = str.at(s.start_a + (s.pos_a ? 1 : -1) * i - 1);
      auto& b = str.at(s.start_b + (s.pos_b ? 1 : -1) * i - 1);
      std::swap(a, b);
      if (a == 'B')
        count++;
      if (b == 'B')
        count++;
    }
  }
  std::cout << count << "\n";
}
