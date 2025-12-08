#include <iostream>
#include <string>
#include <vector>

int main() {
  std::vector<std::string> lines;
  std::string s;
  while (std::getline(std::cin, s))
    lines.push_back(s);

  int width = lines[0].size();
  int height = lines.size();
  int parim = 0;
  for (int y = 0; y < height; y++) {
    for (int i = 0; i < width; i++) {
      for (int j = parim + 1; (i + j) < width; j++) {
        if (!lines[y].substr(i, j).contains('*')) {
          parim = j;
        } else {
          break;
        }
      }
    }
  }
  for (int x = 0; x < width; x++) {
    std::string col;
    for (int y = 0; y < height; y++) {
      col.push_back(lines[y][x]);
    }
    for (int i = 0; i < height; i++) {
      for (int j = parim + 1; (i + j) < height; j++) {
        if (!col.substr(i, j).contains('*')) {
          parim = j;
        } else {
          break;
        }
      }
    }
  }

  std::cout << parim << "\n";
}
