#!/usr/bin/env pypy3

# Problem B - Leave No One Behind

def solve(pos):
    n = len(pos)
    pos = [set(c) if c[0] != c[1] else set() for c in pos]
    remain = sum(len(c) for c in pos)

    def next(p):
        while len(pos[p]) <= 0:
            p = (p + 1) % n
        return p

    count = 0
    p = 0
    while remain > 0:
        p = next(p)
        p2 = next((p + 1) % n)
        c = min(pos[p])
        pos[p].remove(c)
        if c in pos[p2]:
            pos[p2].remove(c)
            remain -= 2
        else:
            pos[p2].add(c)
        count += 1
        p = p2
    return count

while True:
    n = int(input())
    if n == 0:
        break
    pos = [tuple(map(int, input().split())) for i in range(n)]
    print(solve(pos))
