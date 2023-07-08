#!/usr/bin/env python3

# Problem C - Changing the Sitting Arrangement

def swap_row(A, indices):
    return [A[i] for i in indices]

def swap_col(A, indices):
    return [[row[j] for j in indices] for row in A]

def solve(n, A):
    if n % 2 == 0:
        indices = list(range(1, n, 2)) + list(range(0, n, 2))
    else:
        indices = list(range(0, n, 2)) + list(range(1, n, 2))
    B = swap_row(A, indices)
    return swap_col(B, indices)

while True:
    n = int(input())
    if n == 0:
        break
    A = [list(map(int, input().split())) for _ in range(n)]
    assert all(len(a) == n for a in A)
    B = solve(n, A)
    for row in B:
        print(' '.join(map(str, row)))
