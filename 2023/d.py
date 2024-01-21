#!/usr/bin/env python3

# Problem D - Efficient Problem Set

# 7要素あれば100以下のすべての部分和を構成可能
max_n_elem = 7

def subset_sums(ls):
    """
    lsの部分和の集合を返す.
    """
    bag = set([0])
    for x in ls:
        bag |= set([s + x for s in bag])
    return bag

def permut(n, n_elem, min_elem=1, acc=[]):
    """
    和がnであるn_elem個の要素 (min_elem以上の値) の列を返す.
    """
    if n_elem <= 1:
        yield acc + [n]
    else:
        for i in range(min_elem, n // 2 + 1):
            for p in permut(n - i, n_elem - 1, i, acc + [i]):
                yield p

def solve(n, s):
    s = set(i for i, c in enumerate(s) if c == 'o') # setに変換

    # 小さい要素数から順に試す (max_n_elem個あれば必ずカバー可能)
    for n_elem in range(1, max_n_elem):
        # for each 和がnであるn_elem個の要素の列
        for perm in permut(n, n_elem):
            if s <= subset_sums(perm):  # sをカバーした
                return n_elem
    return max_n_elem

while True:
    n = int(input())
    if n == 0:
        break
    s = input()
    assert len(s) == n + 1
    print(solve(n, s))
