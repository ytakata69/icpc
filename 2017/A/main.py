#!/usr/bin/env python3

# ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
# A: Taro's shopping

def solve(n, m, a):
    return max([a[i] + aj for  i in range(n)
                          for aj in a[i + 1:] if a[i] + aj <= m],
               default="NONE")

while True:
    n, m = map(int, input().split())
    if n == 0 and m == 0:
        break
    a = list(map(int, input().split()))
    print(solve(n, m, a))
