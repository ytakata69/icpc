#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

while True:
    h, w = map(int, input().split())
    if h == w == 0: break

    kbd = {}
    for y in range(h):
        r = input()
        for x, c in enumerate(r):
            kbd[c] = (x, y)

    s = input()    
    p = [kbd[c] for c in s]
    cnt = sum(dist(p1, p2) + 1 for p1, p2 in zip([(0, 0)] + p, p))

    print(cnt)
