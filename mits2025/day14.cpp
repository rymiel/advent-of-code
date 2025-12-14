#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <vector>

int main() {
  std::vector<int> nums;
  int i;
  std::cin.ignore(1, '[');
  while (std::cin >> i) {
    nums.push_back(i);
    std::cin.ignore(1, ',');
    if (std::cin.peek() == ']')
      break;
  }

  int best = 0;
  for (size_t i = 0; i < nums.size(); i++) {
    for (size_t j = i + 1; j < nums.size(); j++) {
      for (size_t k = j + 1; k < nums.size(); k++) {
        for (size_t l = k + 1; l < nums.size(); l++) {
          if (nums[i] + nums[j] == nums[k] + nums[l])
            best = std::max(best, nums[i] + nums[j]);
          if (nums[i] + nums[k] == nums[j] + nums[l])
            best = std::max(best, nums[i] + nums[k]);
          if (nums[i] + nums[l] == nums[k] + nums[l])
            best = std::max(best, nums[i] + nums[l]);
          if (nums[j] + nums[k] == nums[i] + nums[l])
            best = std::max(best, nums[j] + nums[k]);
        }
      }
    }
  }

  std::cout << best << "\n";
}
