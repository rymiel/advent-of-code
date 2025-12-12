#include <iostream>
#include <vector>
std::vector<int> arr;
std::vector<bool> has_memo;
std::vector<std::vector<int>> memo;

std::vector<int> lis(int x) {
  if (has_memo[x])
    return memo[x];

  std::vector<int> ans = {};

  for (int i = 0; i < x; i++) {
    if (arr[i] <= arr[x]) {
      auto sub = lis(i);
      if (sub.size() > ans.size())
        ans = sub;
    }
  }
  ans.push_back(x);

  has_memo[x] = true;
  memo[x] = ans;
  return ans;
}

int main() {
  int i;
  arr = std::vector<int>{};
  while (std::cin >> i)
    arr.push_back(i);

  has_memo = std::vector<bool>(arr.size(), false);
  memo = std::vector<std::vector<int>>(arr.size(), std::vector<int>{});

  std::vector<int> ans = {};
  for (int i = 0; i < arr.size(); i++) {
    auto j = lis(i);
    if (j.size() > ans.size())
      ans = j;
  }

  std::cout << arr.size() - ans.size() << '\n';
}
