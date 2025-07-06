#!/usr/bin/env python3

# Problem F: 犬の芸

from collections import Counter

def solve(s, t):
    n = len(s)
    # a, bの個数
    sa = Counter(s)['a']
    ta = Counter(t)['a']
    # 個数が異なっていたら目標状態にできない
    if sa != ta:
        return None

    # 共通接尾辞を除く
    while s[n - 1] == t[n - 1]:
        if s[n - 1] == 'a':
            sa -= 1
        n -= 1
    rest = {'a': sa, 'b': n - sa}

    # 最も右の目標と異なる文字
    tail = s[n - 1]
    # tailだけからなるsの接頭辞の長さ
    prefix = 0
    while s[prefix] == tail:
        prefix += 1

    # rest[tail] - prefix回指示を出すと, tailが先頭に集まる
    command = [t[n - 1].upper()] * (rest[tail] - prefix)
    tail = t[n - 1]

    while n > 0:
        # 変更しなくてよい接尾辞をスキップ
        while n > 0 and t[n - 1] == tail:
            rest[t[n - 1]] -= 1
            n -= 1
        if n > 0:
            command.extend([t[n - 1].upper()] * rest[tail])
            tail = t[n - 1]

    return ''.join(command)

while True:
    n = int(input())
    if n == 0:
        break
    s = input()
    t = input()
    assert len(s) == len(t) == n

    answer = solve(s, t)
    if answer is None:
        print("no")
    else:
        print("yes")
        print(answer)
