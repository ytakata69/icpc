#!/usr/bin/env python3
# -*- coding: utf-8 -*-

while True:
    h, w = map(int, input().split())
    if h == w == 0: break

    kbd = {}
    for y in range(h):
        r = input()
        for x, c in enumerate(r):
            kbd[c] = (x, y)

    s = input()    

    cnt = 0
    px, py = 0, 0
    for c in s:
        x, y = kbd[c]
        cnt += abs(px - x) + abs(py - y) + 1
        px, py = x, y
    print(cnt)
