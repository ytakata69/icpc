#!/usr/bin/env python3

# Problem A - Count Down Up 2020

L2020 = '2 0 2 0'.split()

def solve(ds):
    return list(ds[i : i + 4] == L2020
                for i in range(len(ds))).count(True)

while True:
    n = int(input())
    if n == 0:
        break
    ds = input().split()
    assert len(ds) == n
    print(solve(ds))
