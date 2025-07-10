#!/usr/bin/env python3

# Problem E: 航路廃止

def solve(n, adj):
    # 頂点1を根とする根付き木に変換
    edge = []
    parent = [None] * (n + 1)
    stack = [1]
    visited = set([1])
    while len(stack) > 0:
        u = stack.pop()
        for e, v in adj[u]:
            if v not in visited:
                visited.add(v)
                parent[v] = u
                edge.append((e, v))  # 廃止日と目的地
                stack.append(v)

    # 廃止が早い順
    edge.sort()

    visited = set([1])
    path = []
    for e, v in edge:
        # 廃止の早い辺から根に向かって移動
        newpath = []
        p = v
        while p not in visited:
            newpath.append(p)
            visited.add(p)
            p = parent[p]
        newpath.reverse()
        path.extend(newpath)
        # 廃止に間に合わなかった
        if len(path) - 1 > e:
            return None
    return path

while True:
    n = int(input())
    if n == 0:
        break
    adj = {i: set() for i in range(1, n + 1)}
    for i in range(2, n + 1):
        p, e = map(int, input().split())
        adj[i].add((e, p))
        adj[p].add((e, i))

    answer = solve(n, adj)
    if answer is None:
        print("no")
    else:
        print("yes")
        print(" ".join(map(str, answer)))
