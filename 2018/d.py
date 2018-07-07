#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# 2018 ACM-ICPC国内予選
# Problem D: 全チームによるプレーオフ

def solve(n, match):
    '''param n チーム数
       param match 実施済試合結果
       return 全プレーオフになる組合せ数'''
    global table, nwon, nlost
    table = [[None for a in range(n)] for t in range(n)]
    for x, y in match:
        table[x-1][y-1] = 1  # won
        table[y-1][x-1] = 0  # lost
    nwon, nlost = [[sum(1 for y in range(n) if table[x][y] == i)
                    for x in range(n)] for i in range(2)]
    return search(pos=0)

def valid(x):
    '''チームxの勝数・負数が(n-1)//2以下か'''
    global table, nwon, nlost
    n = len(table)
    return nwon[x] <= (n-1)//2 and nlost[x] <= (n-1)//2

def search(pos):
    '''param pos table中の次に埋める位置
       return 全プレーオフになる組合せ数'''
    global table, nwon, nlost
    n = len(table)
    x = pos // n
    y = pos %  n
    if pos >= n * n:
        return 1
    if x == y or table[x][y] != None:
        return search(pos + 1)

    count = 0  # 組合せ数
    for i in range(2):
        table[x][y] = 1 - i
        table[y][x] = i
        nwon [(x, y)[i]] += 1
        nlost[(y, x)[i]] += 1
        if valid(x) and valid(y):
            count += search(pos + 1)
        nwon [(x, y)[i]] -= 1
        nlost[(y, x)[i]] -= 1
        table[x][y] = None
        table[y][x] = None
    return count

while True:
    n = int(input())
    if n == 0: break
    m = int(input())
    match = []
    for i in range(m):
        x, y = [int(t) for t in input().split()]
        match.append((x, y))
    print(solve(n, match))
