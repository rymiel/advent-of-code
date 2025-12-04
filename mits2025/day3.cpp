#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

int main() {
  std::string _;
  int max;
  std::cin >> _ >> _ >> max;
  std::cin >> _;
  std::vector<int> nums;
  int i;

  while (std::cin >> i) {
    nums.push_back(i);
    std::cin.ignore(); // comma
  }

  std::ranges::sort(nums);

  int count = 0;
  int sum = 0;
  for (int n : nums) {
    if (sum + n < max) {
      sum += n;
      count += 1;
    }
  }

  std::cout << count << "\n";
}
