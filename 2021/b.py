#!/usr/bin/env python3

# Problem B - Hundred-Cell Calculation Puzzles

def solve(w, h, entries):
    adj = {u: [] for u in range(1, w + h + 1)}
    for x, y, _ in entries:
        adj[x].append(y + w)
        adj[y + w].append(x)

    # depth-first search from the leftmost of the top
    visited = set()
    stack = [1]  # the leftmost of the top
    while len(stack) > 0:
        u = stack.pop()
        if u not in visited:
            visited.add(u)
            stack.extend(adj[u])

    return all(u in visited for u in range(1, w + h + 1))

while True:
    w, h = map(int, input().split())
    if w == h == 0:
        break
    k = w + h - 1
    entries = [tuple(map(int, input().split())) for i in range(k)]
    print("YES" if solve(w, h, entries) else "NO")
