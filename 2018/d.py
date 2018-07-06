#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def solve(n, match):
    global count, n0, n1
    count = 0
    table = [[None for a in range(n)] for t in range(n)]
    for x, y in match:
        table[x-1][y-1] = 1
        table[y-1][x-1] = 0 
    n0 = [sum(1 for y in range(n) if table[x][y] == 0) for x in range(n)]
    n1 = [sum(1 for y in range(n) if table[x][y] == 1) for x in range(n)]
    search(table)
    return count

def emptyCell(table):
    n = len(table)
    for y in range(n):
        for x in range(n):
            if x != y and table[x][y] == None:
                return x, y
    return -1, -1

def valid(table, x):
    n = len(table)
    return n0[x] <= (n-1)//2 and n1[x] <= (n-1)//2

def search(table):
    global count
    x, y = emptyCell(table)
    if x < 0:
        count += 1
        return

    table[x][y] = 1
    table[y][x] = 0
    n1[x] += 1
    n0[y] += 1
    if valid(table, x) and valid(table, y):
        search(table)
    n1[x] -= 1
    n0[y] -= 1
    table[x][y] = 0
    table[y][x] = 1
    n0[x] += 1
    n1[y] += 1
    if valid(table, x) and valid(table, y):
        search(table)
    n0[x] -= 1
    n1[y] -= 1
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
