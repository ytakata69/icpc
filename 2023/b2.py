#!/usr/bin/env python3

# Problem B - Amidakuji
# O(n + m)

def solve(n, x, p, q):
    pos = list(range(1, n + 1))  # 位置 (0..n-1) -> 人 (1..n)
    ppos = p - 1  # pの位置 (0..n-1)

    # swap[q] = (x, y) ... 横線(x, y)を加えたらpとqが入れ替わる
    swap = {}

    def update_swap(i):
        if ppos > 0     and pos[ppos - 1] not in swap:
            swap[pos[ppos - 1]] = (ppos,     i)
        if ppos + 1 < n and pos[ppos + 1] not in swap:
            swap[pos[ppos + 1]] = (ppos + 1, i)

    for i, xi in enumerate(x):
        update_swap(i)
        pos[xi - 1], pos[xi] = pos[xi], pos[xi - 1]
        if ppos == xi - 1:
            ppos = xi
        elif ppos == xi:
            ppos = xi - 1
    update_swap(len(x))

    q = pos[q - 1]  # 位置qに来た人
    if q == p:
        return "OK"
    elif q in swap:
        return ' '.join(map(str, swap[q]))
    else:
        return "NG"

while True:
    n, m, p, q = map(int, input().split())
    if n == m == p == q == 0:
        break
    x = list(map(int, input().split()))
    assert len(x) == m
    print(solve(n, x, p, q))
