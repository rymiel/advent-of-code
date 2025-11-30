#include <cstring>

void part1();
void part2();

int main(int argc, char* argv[]) {
  if (argc < 2) return 2;

  if (strcmp(argv[1], "a") == 0) {
    part1();
  } else if (strcmp(argv[1], "b") == 0) {
    part2();
  } else {
    return 3;
  }
}
