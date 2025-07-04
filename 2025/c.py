#!/usr/bin/env python3

def solve(m, a):
    if m % 7 == 6:
        m += 1  # 土曜日で終わる場合は日曜終わりに変更
    weeks = m // 7
    days = weeks * 5 + (m % 7)

    a = set(a)  # 重複を削除
    for ai in a:
        # 平日かつ期間内
        if 0 < ai % 7 < 6 and ai <= m:
            days -= 1
    return days

while True:
    n, m = map(int, input().split())
    if n == m == 0:
        break
    a = list(map(int, input().split()))
    assert len(a) == n
    answer = solve(m, a)
    print(answer)
