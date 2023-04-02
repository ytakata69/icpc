#!/usr/bin/env python3

# Problem D - Audience Queue

modulus = 998244353

def div(n, d):
    return n * pow(d, modulus - 2, modulus) % modulus

def solve(s, t, k):
    """s[i]=人iの座席番号, t[j]=j番目に入場すべき座席番号, k=ゲート数"""

    n = len(s)  # 人数
    entrank = {tj: j for j, tj in enumerate(t)}  # 座席番号 -> 入場順

    # 必ず切断すべき位置 (= 入場順が逆転する位置)
    cut = set(i for i in range(1, n) if entrank[s[i-1]] > entrank[s[i]])

    # ゲートが足りなければ失敗
    if len(cut) + 1 > k:
        return 0

    # group[x] = 切断位置で区間に分けたときの座席xのグループ番号
    group = {s[0]: 0}  # 人0のグループ番号は0
    for i in range(1, n):
        group[s[i]] = group[s[i-1]] + (1 if i in cut else 0)

    # mx[j] = max(t[:j])
    mx = [0]
    for j in range(1, n):
        mx.append(max(mx[-1], t[j - 1]))

    # 切断不可能位置 (= 座席番号が逆転)
    notcut = set(j for j in range(n) if t[j] < mx[j])

    # グループ間で座席番号が逆転していたら失敗
    if any(group[t[j]] != group[mx[j]] for j in notcut):
        return 0

    # m個の切断可能位置のうち最大l箇所を切る
    m = n - 1 - len(cut) - len(notcut)
    l = k - 1 - len(cut)
    l = min(l, m)

    # \sum_{i=0}^{l} combination(m, i)
    comb = 1  # combination(m, 0)
    combsum = comb
    for i in range(1, l + 1):
        comb = div(comb * (m - i + 1), i)
        combsum = (combsum + comb) % modulus

    return combsum

while True:
    n, k = map(int, input().split())
    if n == k == 0:
        break
    s = list(map(int, input().split()))
    t = list(map(int, input().split()))
    assert len(s) == len(t) == n
    print(solve(s, t, k))
