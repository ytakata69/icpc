#!/usr/bin/env python3

# Problem C - Tree Transformation Puzzle

from itertools import chain

class Node:
    def __init__(self, op, children):
        self.op = op
        self.children = children
        self.adj = children[:]  # adj == children + [parent]
        self.max = None         # max value when self is the root
        self.completed = {}     # completed neighbors

        for c in children:
            c._setParent(self)

    def _setParent(self, parent):
        self.parent = parent
        self.adj.append(parent)
        assert len(self.adj) == 1 or len(self.adj) == 3

    def receive(self, sender, mn, mx):
        """sender: one of the neighbors of self.
           mn: min value of sender when sender is not the root.
           mx: max value of sender when sender is not the root."""
        for c in self.completed:
            cmin, cmax = self.completed[c]
            third = list(set(self.adj) - {sender, c})[0]
            if self.op == '+':
                third.receive(self, mn + cmin, mx + cmax)
            else:
                third.receive(self, min(mn - cmax, cmin - mx),
                                    max(mx - cmin, cmax - mn))
        self.completed[sender] = (mn, mx)

        if len(self.completed) >= len(self.adj):
            if self.op == '+':
                smax = sum(self.completed[c][1] for c in self.adj)
                self.max = smax
            else:
                smin = sum(self.completed[c][0] for c in self.adj)
                self.max = max(
                    self.completed[c][1] - (smin - self.completed[c][0])
                    for c in self.adj)

    def leaves(self):
        return chain.from_iterable(c.leaves() for c in self.children)
    def innernodes(self):
        return chain([self],
                   chain.from_iterable(c.innernodes() for c in self.children))

    def __str__(self):
        return '(' + self.op + ', [' \
            + ', '.join(map(str, self.children)) + '], ' \
            + str(self.max) + ')'

class Leaf(Node):
    def __init__(self, v):
        self.v = v
        self.adj = []

    def receive(self, child, mn, mx):
        pass
    def leaves(self):
        return [self]
    def innernodes(self):
        return []

    def __str__(self):
        return str(self.v)

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
    for l in e.leaves():
        l.parent.receive(l, l.v, l.v)
    return max(n.max for n in e.innernodes())

while True:
    s = input()
    if s == '-1':
        break
    e = parse_root(s)
    print(solve(e))
