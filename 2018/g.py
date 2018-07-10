#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
sys.setrecursionlimit(1010)

class Parser:
    def __init__(self, s):
        self.str = s
        self.pos = 0
    def parse(self):
        exp = Exp()
        while self.pos < len(self.str):
            c = self.str[self.pos]
            if c == '(':
                self.pos += 1
                e = self.parse()
                exp.append(e)
                assert self.str[self.pos] == ')'
            elif '1' <= c <= '9':
                exp.append(int(c))
            elif c == '*':
                pass
            elif c == '+':
                exp.closeterm()
            elif c == ')':
                break  # self.posは変更しない
            else:
                raise
            self.pos += 1
        return exp

class Exp:
    '''式を表すオブジェクト. 式はtermの列. termはfactorの列.'''
    def __init__(self):
        self.terms = [[]]
        self.lastT = self.terms[-1]
        self.val   = None
    def append(self, f):
        '''因子fを追加'''
        self.lastT.append(f)
        self.val = None
    def closeterm(self):
        '''最後のtermを閉じて新しいtermを追加'''
        self.lastT = []
        self.terms.append(self.lastT)
    def __str__(self):
        return str([[f if type(f) is int else str(f) for f in t]
                    for t in self.terms])
    def value(self):
        '''この式を計算した値'''
        if self.val == None:
            self.termval = [0] * len(self.terms)
            for i in range(len(self.terms)):
                a = 1
                for f in self.terms[i]:
                    a *= f if type(f) is int else f.value()
                self.termval[i] = a
            self.val = sum(self.termval)
        return self.val
    def nsubexp(self, v):
        '''値がvと等しい部分式の個数'''
        if self.value() < v:
            return 0
        # 尺取法
        count = 0
        lt, lf = 0, 0  # term, factor
        left  = 1  # loが含まれるtermの値
        mid   = 0  # loとhiの中間のtermの値
        right = 0  # hiが含まれるtermの値
        n1 = None  # 連続する1の個数

        # hiを動かす
        for ht in range(len(self.terms)):
            for hf in range(len(self.terms[ht])):
                if hf == 0 and lt < ht:  # 項が変わった
                    mid  += right
                    right = 1

                x = self.terms[ht][hf]
                if type(x) is Exp:
                    count += x.nsubexp(v)
                if lt == ht:
                    left  *= Exp.factor(x)
                else:
                    right *= Exp.factor(x)

                # loを動かす
                while left + mid + right > v and (lt < ht or lf < hf):
                    left //= Exp.factor(self.terms[lt][lf])
                    nlt, nlf = self.nextfactor(lt, lf)
                    if lt < nlt:
                        assert left == 1
                        if nlt < ht:
                            left = self.termval[nlt]
                            mid -= left
                        else:
                            left  = right
                            right = 0
                    lt, lf = nlt, nlf
                    n1 = None  # キャッシュを消す

                # vと一致していればcountを増やす
                if left + mid + right == v:
                    count += 1
                    if Exp.factor(self.terms[lt][lf]) == 1:
                        # 1が連続している場合はその個数分増やす
                        if n1 == None:
                            n1 = 0
                            for i in range(lf, len(self.terms[lt])):
                                if Exp.factor(self.terms[lt][i]) != 1: break
                                n1 += 1
                        n1x = hf - lf if lt == ht and hf < lf + n1 else n1
                        # 末尾のfactorが1の場合，それは削除できない
                        if lf + n1x >= len(self.terms[lt]): n1x -= 1
                        count += n1x
        return count

    @staticmethod
    def factor(f):
        '''因子fの値'''
        return f if type(f) is int else f.value()
    def nextfactor(self, t, f):
        '''第t項第f因子の次の因子の番号'''
        f += 1
        if f >= len(self.terms[t]):
            t += 1
            f  = 0
        return (t, f)

while True:
    n = int(input())
    if n == 0: break
    s = input()
    parser = Parser(s)
    exp = parser.parse()
    print(exp.nsubexp(n))
