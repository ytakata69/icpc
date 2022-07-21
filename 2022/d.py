#!/usr/bin/env python3

# Problem D - Audience Queue

modulus = 998244353

def div(n, d):
    return n * pow(d, modulus - 2, modulus) % modulus

def solve(s, t, k):
    """s[i]=人iの座席番号, t[j]=j番目に入場すべき座席番号, k=ゲート数"""

    n = len(s)  # 人数
    t = {tj: j for j, tj in enumerate(t)}  # 座席番号 -> 入場順

    # 必ず切断すべき位置 (= 入場順が逆転する位置)
    cut = set(i for i in range(1, n) if t[s[i-1]] > t[s[i]])

    # ゲートが足りなければ失敗
    if len(cut) + 1 > k:
        return 0

    # group[i] = 切断位置で区間に分けたときの人iのグループ番号
    group = [0]  # 人0のグループ
    for i in range(1, n):
        group.append(group[-1] + (1 if i in cut else 0))

    s = list(zip(s, group))  # (座席番号, グループ番号)
    s.sort(key=lambda x: t[x[0]])  # 入場順に整列

    # mx[j] = max(s[:j])
    mx = [(0, 0)]
    for j in range(1, n):
        mx.append(max(mx[-1], s[j - 1]))

    # 切断不可能位置 (= 座席番号が逆転)
    notcut = set(j for j in range(n) if s[j] < mx[j])

    # グループ間で座席番号が逆転していたら失敗
    if any(s[j][1] != mx[j][1] for j in notcut):
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
