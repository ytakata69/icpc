#!/usr/bin/env python3

# Problem B - Contact Tracer

def solve(p, ab):
    infect = {p}
    for a, b in ab:
        if a in infect or b in infect:
            infect.update((a, b))
    return len(infect)

while True:
    m, n, p = map(int, input().split())
    if m == 0:
        exit()
    ab = []
    for i in range(n):
        a, b = map(int, input().split())
        ab.append((a, b))
    print(solve(p, ab))
