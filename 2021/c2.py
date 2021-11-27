#!/usr/bin/env python3

# Problem C - Tree Transformation Puzzle

class Node:
    def __init__(self, op, children):
        self.op = op
        self.children = children
        # 左右の入れ替えのみ行う場合の最大値・最小値
        if op == '+':
            self.maxv = sum(c.maxv for c in children)
            self.minv = sum(c.minv for c in children)
        else:
            minSum = sum(c.minv for c in children)
            maxSum = sum(c.maxv for c in children)
            self.maxv = max(c.maxv + c.minv - minSum for c in children)
            self.minv = min(c.minv + c.maxv - maxSum for c in children)
        self.rootv = None  # この頂点を根にしたときの最大値

    def chroot(self, parentMin=0, parentMax=0):
        """各頂点のrootvを計算"""
        if self.op == '+':
            self.rootv = self.maxv + parentMax
        else:
            minSum = sum(c.minv for c in self.children)
            maxSum = sum(c.maxv for c in self.children)
            self.rootv = max(self.maxv - parentMin, parentMax - minSum)

        for c in self.children:
            # c以外の子と親を合わせてできる最大値・最小値
            if self.op == '+':
                pMax = self.maxv - c.maxv + parentMax
                pMin = self.minv - c.minv + parentMin
            else:
                maxNc = max(d.maxv + d.minv for d in self.children if c != d)
                minNc = min(d.minv + d.maxv for d in self.children if c != d)
                pMax = max(maxNc - parentMin, parentMax) - minSum + c.minv
                pMin = min(minNc - parentMax, parentMin) - maxSum + c.maxv
                if len(self.children) == 3:
                    pMin = minNc - maxSum + c.maxv
            c.chroot(pMin, pMax)

    def maxRootValue(self):
        return max(self.rootv, max(c.maxRootValue() for c in self.children))

    def __str__(self):
        return '(' + self.op + ', [' \
            + ', '.join(map(str, self.children)) + '], ' \
            + ', '.join(map(str, [self.rootv])) + ')'

class Leaf(Node):
    def __init__(self, v):
        self.minv = self.maxv = v
    def __str__(self):
        return str(self.maxv)

    def chroot(self, parentMin, parentMax):
        pass
    def maxRootValue(self):
        return -float('inf')

def parse_non_root(s, pos):
    if '0' <= s[pos] <= '9':
        return Leaf(int(s[pos])), pos + 1
    else:
        assert s[pos] == '('
        e1, pos = parse_non_root(s, pos + 1)
        op = s[pos]
        e2, pos = parse_non_root(s, pos + 1)
        assert s[pos] == ')'
        return Node(op, [e1, e2]), pos + 1

def parse_root(s):
    e1, pos = parse_non_root(s, 0)
    op = s[pos]
    e2, pos = parse_non_root(s, pos + 1)
    assert op == s[pos]
    e3, pos = parse_non_root(s, pos + 1)
    assert pos == len(s)
    return Node(op, [e1, e2, e3])

def solve(e):
    e.chroot()
    return e.maxRootValue()

while True:
    s = input()
    if s == '-1':
        break
    e = parse_root(s)
    print(solve(e))
