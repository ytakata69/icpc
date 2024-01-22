#!/usr/bin/env python3

# Problem D - Efficient Problem Set

def ceil_log2(n):
    """
    2 ** (i - 1) < n <= 2 ** i である i を返す.
    """
    i, p = 0, 1
    while p < n:
        i, p = i + 1, p << 1
    return i

def subset_sums(ls):
    """
    lsの部分和の集合を返す.
    """
    bag = set([0])
    for x in ls:
        bag |= set([s + x for s in bag])
    return bag

def permut(n, n_elem, min_elem=1, acc=[], max_min=None):
    """
    和がnであるn_elem個の要素 (min_elem以上の値) の列を返す.
    最小要素はmax_min以下.
    """
    if n_elem <= 1:
        yield acc + [n]
    else:
        max_min = n // n_elem if max_min is None else min(max_min, n // n_elem)
        for i in range(max_min, min_elem - 1, -1):
            for p in permut(n - i, n_elem - 1, i, acc + [i]):
                yield p

def solve(n, s):
    s = set(i for i, c in enumerate(s) if c == 'o') # setに変換
    s |= set(n - i for i in s) # 対称化
    mn = sorted(s)[1]  # 0の次に小さいsの要素

    max_n_elem = ceil_log2(n + 1)  # 0以上n以下のすべての和を構成可能な要素数
    min_n_elem = ceil_log2(len(s)) # sの要素を構成するのに最小限必要な要素数

    # 小さい要素数から順に試す (max_n_elem個あれば必ずカバー可能)
    for n_elem in range(min_n_elem, max_n_elem):
        # for each 和がnであるn_elem個の要素の列
        for perm in permut(n, n_elem, max_min=mn):
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
