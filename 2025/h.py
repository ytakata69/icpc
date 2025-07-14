#!/usr/bin/env python3

# Problem H: カッコ

from collections import Counter

def solve(s):
    s = list(s)
    n = len(s)

    # 孤立点の記号を変える
    for i in range(n):
        if (i <= 0     or s[i - 1] != s[i]) and \
           (i >= n - 1 or s[i + 1] != s[i]):
            s[i] = '<' if s[i] == '(' else '>'

    # 各位置について, それより左・右の非孤立点を求める
    left  = [None] * n
    right = [None] * n
    last = -1
    for i in range(n):
        if s[i] in "()":
            last = i
        left[i] = last
    last = n
    for i in range(n - 1, -1, -1):
        if s[i] in "()":
            last = i
        right[i] = last

    # 各位置 i は left[i] + 1 〜 right[i] - 1 の孤立点列に属する.
    # ')'で挟まれた孤立点列中の'<'から始まるとその孤立点列中で終わる.
    # '('で挟まれた孤立点列中の'>'で終わるのはその孤立点列中の'<'からのみ.

    def fenced(i, c):
        """位置 i の孤立点は文字 c に挟まれている"""
        return ((left[i]  <  0 or s[left[i]] == c) and
                (right[i] >= n or s[right[i]] == c))

    # '('で挟まれていない偶数・奇数位置の')', '>'の個数
    nclose = [0, 0]
    for i in range(n):
        if s[i] == ')' or (s[i] == '>' and not fenced(i, '(')):
            nclose[i % 2] += 1

    # 各 '(' について, それから始まる組の個数を求める
    count = 0
    for i in range(n):
        if s[i] == '(':
            count += nclose[(i + 1) % 2]

            # 左隣の孤立点列の分を加算
            if i > 0 and s[i-1] == '>' and fenced(i-1, '('):
                l = 1 if left[i-1] < 0 and s[0] == '<' else left[i-1]
                count += (i - l + 1) // 2  # 左の'>'
            # 右隣の孤立点列の分を加算
            if i < n - 1 and s[i+1] == '>' and fenced(i+1, '('):
                r = n - 2 if right[i+1] >= n and s[n - 1] == '<' else right[i+1]
                count += (r - i + 1) // 2  # 右の'>'
        elif s[i] == '<':
            if not fenced(i, ')'):
                count += nclose[(i + 1) % 2]

            # 同じ孤立点列の分を加算
            if fenced(i, ')') or fenced(i, '('):
                # 孤立点列の左端と右端 (孤立点列の隣まで含める)
                l = left[i]  if fenced(i, ')') and left[i] >= 0 else left[i] +1
                r = right[i] if fenced(i, ')') and right[i] < n else right[i]-1
                count += (i - l + 1) // 2  # 左の'>'
                count += (r - i + 1) // 2  # 右の'>'

    return count

while True:
    n = int(input())
    if n == 0:
        break
    s = input()
    assert len(s) == n

    answer = solve(s)
    print(answer)
