#include <stdio.h>
#include <math.h>

#define dist(x, y) (sqrt((x) * (x) + (y) * (y)))

double solve(int D, int E)
{
  int x, y;
  double min, e;
  min = INFINITY;
  for (x = 0; x <= D; x++) {
    y = D - x;
    e = fabs(dist(x, y) - E);
    if (min > e) min = e;
  }
  return min;
}

int main()
{
  int D, E;
  for (;;) {
    scanf("%d%d", &D, &E);
    if (D == 0 && E == 0) break;
    printf("%.10f\n", solve(D, E));
  }
  return 0;
}
