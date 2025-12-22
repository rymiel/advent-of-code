#include "lib.hpp"
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

struct Kott {
  uint64_t kaal, ruumala, hind;
};

struct Suveniir {
  uint64_t kaal, ruumala;
  auto operator+(const Suveniir& other) const -> Suveniir { return {kaal + other.kaal, ruumala + other.ruumala}; }
};

uint64_t INF = 1'000'000'000;

int main() {
  auto lines = read_lines();
  auto marker = std::find(lines.begin(), lines.end(), "Suveniirid:");

  std::vector<Kott> kotid;
  std::vector<Suveniir> suveniirid;
  for (auto it = lines.begin() + 1; it != marker; it++) {
    std::stringstream ss{*it};
    Kott kott;
    ss >> kott.kaal;
    ss.ignore(1, ',') >> kott.ruumala;
    ss.ignore(1, ',') >> kott.hind;
    kotid.push_back(kott);
  }

  for (auto it = marker + 1; it != lines.end(); it++) {
    std::stringstream ss{*it};
    Suveniir kink;
    ss >> kink.kaal;
    ss.ignore(1, ',') >> kink.ruumala;
    suveniirid.push_back(kink);
  }

  const size_t n = suveniirid.size();
  std::vector<uint64_t> dp(n + 1, INF);
  dp[0] = 0;

  for (size_t i = 1; i <= n; i++) {
    for (size_t j = 0; j < i; j++) {
      auto kogu = std::accumulate(suveniirid.begin() + j, suveniirid.begin() + i, Suveniir{});

      auto it = std::lower_bound(kotid.begin(), kotid.end(), kogu, [](const Kott& k, const Suveniir& s) {
        return k.kaal < s.kaal || k.ruumala < s.ruumala;
      });

      auto hind = INF;
      if (it != kotid.end())
        hind = it->hind;

      dp[i] = std::min(dp[i], dp[j] + hind);
    }
  }

  std::cout << dp[n] << "\n";
}
