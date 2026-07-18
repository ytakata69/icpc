#!/usr/bin/env python3

# ICPC Asia Japan 2026 Online First-Round Contest
# Problem D: Frequency sequence

import math

# s,
# 1, 1, 2, 1, 3, 1, ..., s-1, 1, s,   -- 2 * s - 1
# 2, 2, 3, 2, 4, 2, ..., s-1, 2, s,   -- 2 * (s-1) - 1
# 3, 3, 4, 3, 5, 3, ..., s-1, 3, s,   -- 2 * (s-2) - 1
# ...,
#            s-2, s-2, s-1, s-2, s,   -- 2 * 3 - 1
#                      s-1, s-1, s,   -- 2 * 2 - 1
#                                s,   -- 2 * 1 - 1
# s+1, 1, s+1, 2, ..., s+1, s,
# s+2, 1, s+2, 2, ..., s+1, s,
# ...

def solve(s, k):
    # 前半部の長さ
    l1 = 1 + s * s
    if k == 1 or k == l1:
        return s

    # 前半部
    if k <= l1:
        kl = kr = 0
        for i in range(s):
            row = i
            kl = kr
            kr = kl + 2 * (s - i) - 1
            if k - 2 < kr:
                break
        col = k - 2 - kl
        return row + 1 + (col // 2 if col % 2 == 0 else 0)
    # 後半部
    k -= l1
    row = (k - 1) // (s * 2)
    col = (k - 1) %  (s * 2)
    return s + row + 1 if col % 2 == 0 else col // 2 + 1

while True:
    s, k = map(int, input().split())
    if s == 0:
        break
    print(solve(s, k))
