#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# 2018 ACM-ICPC国内予選
# Problem D: 全チームによるプレーオフ

def product(m, n):
    p = 1
    for i in range(m, n+1): p *= i
    return p
def choose(n, k):
    return product(n-k+1, n) // product(1, k)

def solve(n, match):
    '''param n チーム数
       param match 実施済試合結果
       return 全プレーオフになる組合せ数'''
    global table, nwon, nlost, nmatch
    # 実施済試合数
    nmatch = [0] * n
    for x, y in match:
        nmatch[x] += 1
        nmatch[y] += 1
    # 実施済試合数の多い順にチーム番号を付け直す
    teams = sorted(range(n), key=lambda x: -nmatch[x])
    trans = [0] * n
    for i in range(n): trans[teams[i]] = i
    nmatch = [nmatch[teams[i]] for i in range(n)]
    # 星取表
    table = [[None for a in range(n)] for t in range(n)]
    for x, y in match:
        u, v = trans[x], trans[y]
        table[u][v] = 1  # won
        table[v][u] = 0  # lost
    nwon, nlost = [[sum(1 for y in range(n) if table[x][y] == i)
                    for x in range(n)] for i in range(2)]
    if not all(valid(x) for x in range(n)):
        return 0  # 初期配置が条件を満たさない
    return search(pos=0)

def valid(x):
    '''チームxの勝数・負数が(n-1)//2以下か'''
    global table, nwon, nlost
    n = len(table)
    return nwon[x] <= (n-1)//2 and nlost[x] <= (n-1)//2

def search(pos):
    '''param pos table中の次に埋める位置
       return 全プレーオフになる組合せ数'''
    global table, nwon, nlost, nmatch
    n = len(table)
    x = pos // n
    y = pos %  n
    if pos >= n * n:
        return 1
    if x == y or table[x][y] != None:
        return search(pos + 1)

    # 1試合もしていないチームは1パタンだけ試して組合せ倍する
    if x == 0 and 0 < y and nmatch[y] == 0:
        i = 0 if nwon[x] < (n-1)//2 else 1
        table[x][y] = 1 - i
        table[y][x] = i
        nwon [(x, y)[i]] += 1
        nlost[(y, x)[i]] += 1
        count = search(pos + 1)
        nwon [(x, y)[i]] -= 1
        nlost[(y, x)[i]] -= 1
        table[x][y] = None
        table[y][x] = None
        if nmatch[y-1] == 0:
            return count
        else:
            # 1試合もしていないチームのうちxが勝つチームの選び方
            c = choose(n - y, (n-1)//2 - nwon[x])
            return c * count

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
        match.append((x-1, y-1))
    print(solve(n, match))
