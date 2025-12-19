#include <cstdio>
#include <cstring>
#include "lib.hpp"

void part1();
void part2();

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "Please provide a part number\n");
    return 2;
  }

  if (strcmp(argv[1], "1") == 0)
    part1();
  else if (strcmp(argv[1], "2") == 0)
    part2();
  else
    return 3;
}
