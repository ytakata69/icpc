#include <stdio.h>
#define MAX_M 200

int bestN(int nmin, int nmax, int score[])
{
  int n, gap;
  int maxgap = 0, bestn = 0;
  for (n = nmin; n <= nmax; n++) {
    gap = score[n - 1] - score[n];
    if (maxgap <= gap) {
      maxgap = gap;
      bestn  = n;
    }
  }
  return bestn;
}

int main()
{
  static int score[MAX_M];
  int m, nmin, nmax, i;
  for (;;) {
    scanf("%d%d%d", &m, &nmin, &nmax);
    if (m == 0 && nmin == 0 && nmax == 0) break;
    for (i = 0; i < m; i++) {
      scanf("%d", &score[i]);
    }
    printf("%d\n", bestN(nmin, nmax, score));
  }
  return 0;
}
