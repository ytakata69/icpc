#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def solve(n, match):
    global count, table, n01
    count = 0
    table = [[None for a in range(n)] for t in range(n)]
    for x, y in match:
        table[x-1][y-1] = 1
        table[y-1][x-1] = 0 
    n01 = [[sum(1 for y in range(n) if table[x][y] == i) for x in range(n)]
           for i in range(2)]
    search(0)
    return count

def valid(x):
    global table, n01
    n = len(table)
    return n01[0][x] <= (n-1)//2 and n01[1][x] <= (n-1)//2

def search(pos):
    global count, table, n01
    n = len(table)
    x = pos // n
    y = pos %  n
    if pos >= n * n:
        count += 1
        return
    if x == y or table[x][y] != None:
        search(pos + 1)
        return

    for i in range(2):
        table[x][y] = 1 - i
        table[y][x] = i
        n01[1-i][x] += 1
        n01[  i][y] += 1
        if valid(x) and valid(y):
            search(pos + 1)
        n01[1-i][x] -= 1
        n01[  i][y] -= 1
        table[x][y] = None
        table[y][x] = None

while True:
    n = int(input())
    if n == 0: break
    m = int(input())
    match = []
    for i in range(m):
        x, y = [int(t) for t in input().split()]
        match.append((x, y))
    print(solve(n, match))
