#!/usr/bin/env python3

def flip(c):
    return ''.join(['.' if ci == '#' else '#' for ci in c])

def solve(c):
    flipped = flip(c[0])  # 1行目を白黒反転
    code = [0]  # 1行目を0, その白黒反転を1として符号化
    for i in range(1, len(c)):
        if c[i] == c[0]:
            code.append(0)
        elif c[i] == flipped:
            code.append(1)
        else:
            return -1  # どちらでもなければ矛盾
    # codeのランレングス
    runlen = [1]
    for i in range(1, len(code)):
        if code[i - 1] == code[i]:
            runlen[-1] += 1
        else:
            runlen.append(1)
    # ランが3つ以上あれば正方形の1辺の長さが確定
    if len(runlen) >= 3:
        rl = runlen[1]  # 1辺の長さ
        # どの正方形も同じ長さ. 端はそれ以下の長さ
        if (max(runlen[0], runlen[-1]) <= rl and
            all(runlen[i] == rl for i in range(2, len(runlen) - 1))):
            return rl
        else:
            return -1
    else:
        return ("uncertain", max(runlen))

def transpose(ls):
    newls = []
    for i in range(len(ls[0])):
        newls.append(''.join([ls[j][i] for j in range(len(ls))]))
    return newls

while True:
    n, m = map(int, input().split())
    if n == m == 0:
        break
    c = [input() for _ in range(n)]
    assert all(len(ci) == m for ci in c)
    ans1 = solve(c)
    ans2 = solve(transpose(c))
    if ans1 == -1 or ans2 == -1:
        print(-1)
    elif type(ans1) == tuple and type(ans2) == tuple:
        print(0)
    elif type(ans1) == tuple:
        print(ans2 if ans2 >= ans1[1] else -1)
    elif type(ans2) == tuple:
        print(ans1 if ans1 >= ans2[1] else -1)
    else:
        print(ans1 if ans1 == ans2 else -1)

