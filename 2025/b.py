#!/usr/bin/env python3

def solve(s):
    n = len(s)
    # 接頭辞と等しい最長の接尾辞
    for j in range(n - 1, 0, -1):
        if s[:j] == s[-j:]:
            return s + s[j:]
    return s + s

while True:
    n = int(input())
    if n == 0:
        break
    s = input()
    assert len(s) == n
    a = solve(s)
    print(a)
