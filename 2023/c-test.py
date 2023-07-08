#!/usr/bin/env python3

# Tester for Problem C
# Usage:
# $ ./c.py < C1 | ./c-test.py C1

import sys

infile = open(sys.argv[1])

def dist(p1, p2):
    y1, x1 = p1
    y2, x2 = p2
    return abs(x1 - x2) + abs(y1 - y2)

def test(n, A, B):
    posB = {}  # id -> (row, col)
    for i, row in enumerate(B):
        for j, b in enumerate(row):
            posB[b] = (i, j)

    for i, row in enumerate(A):
        for j, a in enumerate(row):
            for di, dj in [(-1, 0), (0, -1), (1, 0), (0, 1)]:
                if 0 <= i + di < n and 0 <= j + dj < n:
                    adj = A[i + di][j + dj]
                    if dist(posB[a], posB[adj]) < n // 2:
                        return False
    return True

def load(file, n):
    A = [list(map(int, file.readline().split())) for _ in range(n)]
    assert all(len(a) == n for a in A)
    return A

while True:
    n = int(infile.readline().strip())
    if n == 0:
        if sys.stdin.read(1) == '':  # at the end of file
            break
        else:
            raise RuntimeError("too long output")
    Ai = load(infile, n)
    Ao = load(sys.stdin, n)
    if test(n, Ai, Ao) != True:
        raise RuntimeError(f"test failed")
