#include <stdio.h>

#define MAX_N  1000

void solve(int n, int m, int a[])
{
  int i, j, max;
  max = 0;
  for (i = 0; i < n; i++) {
    for (j = i + 1; j < n; j++) {
      int p = a[i] + a[j];
      if (p <= m && max < p) {
        max = p;
      }
    }
  }
  if (max > 0) {
    printf("%d\n", max);
  } else {
    printf("NONE\n");
  }
}

int main()
{
  static int a[MAX_N];

  for (;;) {
    int n, m, i;
    scanf("%d%d", &n, &m);
    if (n == 0 && m == 0) { break; }
    for (i = 0; i < n; i++) {
      scanf("%d", &a[i]);
    }
    solve(n, m, a);
  }
  return 0;
}
