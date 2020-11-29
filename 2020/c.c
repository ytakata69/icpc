/*
 * Problem C - Luggage
 */

#include <stdio.h>
#include <math.h>

/*
def solve(p):
    for w in range(1, p + 1):
        if w ** 3 > p:
            break
        if p % w != 0:
            continue
        for h in range(floor(sqrt(p // w)), 0, -1):
            if (p // w) % h == 0:
                yield w + h + (p // w // h)
                break
*/

#define min(a, b)  ((a) <= (b) ? (a) : (b))

long solve(long p)
{
  long m = p + 2;
  long w, h;
  for (w = 1; w * w * w <= p; w++) {
    if (p % w != 0) continue;
    for (h = (long)sqrt(p / w); h >= 1; h--) {
      if (p / w % h == 0) {
        m = min(m, w + h + (p / w / h));
        break;
      }
    }
  }
  return m;
}

int main()
{
  long p;
  for (;;) {
    scanf("%ld", &p);
    if (p == 0) break;
    printf("%ld\n", solve(p));
  }
  return 0;
}
