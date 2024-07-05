#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem B - Overtaking

def accum(a):
    """累積和のリスト"""
    ac = [a[0]]
    for i in range(1, len(a)):
        ac.append(ac[-1] + a[i])
    return ac

def sign(ai, bi):
    return -1 if ai < bi else 1

def solve(a, b):
    acc_a = accum(a)  # 累積和のリスト
    acc_b = accum(b)  # 〃

    # 差の符号のリスト（0を除去）
    signs = [sign(ai, bi) for ai, bi in zip(acc_a, acc_b) if ai != bi]

    # 符号が変化する位置の個数
    return sum(1 for i in range(1, len(signs)) if signs[i-1] != signs[i])

while True:
    n = int(input())
    if n == 0:
        break
    a = list(map(int, input().split()))
    b = list(map(int, input().split()))
    assert len(a) == len(b) == n
    print(solve(a, b))
