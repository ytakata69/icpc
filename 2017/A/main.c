/*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * A: Taro's shopping
 */
#include <stdio.h>

#define MAX_N  1000

/**
 * @param n  商品数
 * @param m  金額
 * @param a  各商品の価格
 */
void solve(int n, int m, int a[])
{
  int i, j, max;
  max = 0;
  /* すべての組(i,j)を調べる */
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
