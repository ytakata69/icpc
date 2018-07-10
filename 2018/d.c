#include <stdio.h>

#define MAX_N  9
#define MAX_MATCH  (MAX_N * (MAX_N - 1) / 2)

#define None  (-1)

struct Match {
  int x, y;
} match[MAX_MATCH];

int count;
int table[MAX_N][MAX_N];

int valid(int n, int x)
{
  int n0, n1;
  int y;
  n0 = n1 = 0;
  for (y = 0; y < n; y++) {
    if (table[x][y] == 0) { n0++; }
    if (table[x][y] == 1) { n1++; }
  }
  return n0 <= (n-1)/2 && n1 <= (n-1)/2;
}

void search(int n, int pos)
{
  int x, y, i;
  if (pos >= n * n) {
    count += 1;
    return;
  }
  x = pos / n;
  y = pos % n;
  if (x == y || table[x][y] != None) {
    search(n, pos + 1);
    return;
  }

  for (i = 0; i <= 1; i++) {
    table[x][y] = 1 - i;
    table[y][x] = i;
    if (valid(n, x) && valid(n, y)) {
      search(n, pos + 1);
    }
    table[x][y] = None;
    table[y][x] = None;
  }
}

int solve(int n, struct Match match[], int m)
{
  int i, j;
  count = 0;
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) {
      table[i][j] = None;
    }
  }
  for (i = 0; i < m; i++) {
    table[match[i].x-1][match[i].y-1] = 1;
    table[match[i].y-1][match[i].x-1] = 0;
  }
  for (i = 0; i < n; i++) {
    if (! valid(n, i)) { return 0; }
  }
  search(n, 0);
  return count;
}

int main()
{
  int i, n, m, x, y;
  for (;;) {
    scanf("%d", &n);
    if (n == 0) { break; }
    scanf("%d", &m);
    for (i = 0; i < m; i++) {
      scanf("%d%d", &x, &y);
      match[i].x = x;
      match[i].y = y;
    }
    printf("%d\n", solve(n, match, m));
  }
  return 0;
}
