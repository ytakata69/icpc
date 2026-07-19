#!/usr/bin/env python3

# ICPC Asia Japan 2026 Online First-Round Contest
# Problem D: Frequency sequence

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
    if k == 1:
        return s

    # 前半部
    l1 = 1 + s * s  # 前半部の長さ
    if k <= l1:
        k -= 2
        row = 0
        while k >= 2 * (s - row) - 1:
            k -= 2 * (s - row) - 1
            row += 1
        return row + 1 + (k // 2 if k % 2 == 0 else 0)
    # 後半部
    k -= l1 + 1
    row = k // (s * 2)
    col = k %  (s * 2)
    return s + row + 1 if col % 2 == 0 else col // 2 + 1

while True:
    s, k = map(int, input().split())
    if s == 0:
        break
    print(solve(s, k))
