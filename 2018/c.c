#include <stdio.h>

void solve(int b) {
  long int s = 0;
  int lo = 1;
  int hi;
  for (hi = 1; hi <= b; hi++) {
    s += hi;
    while (s > b) {
      s  -= lo;
      lo += 1;
    }
    if (s == b) {
      printf("%d %d\n", lo, hi - lo + 1);
      return;
    }
  }
  printf("ERROR\n");
}

int main()
{
  int b;
  for (;;) {
    scanf("%d", &b);
    if (b == 0) { break; }
    solve(b);
  }
  return 0;
}
