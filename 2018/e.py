#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from math import ceil

class F:
    '''浮動小数点数'''
    M_LEN = 52
    E_LEN = 12
    def __init__(self, b):
        assert len(b) == F.M_LEN
        self.e = 0
        self.m = int('1' + b, 2)
    def addntimes(self, n, f):
        '''selfにfをn回加える'''
        while n > 0:
            a = f.m >> (self.e - f.e)  # 実際に足す数
            if a == 0: break
            remain = (1 << (F.M_LEN + 1)) - self.m  # 桁上りするまでの残り
            k = ceil(remain / a)  # 桁上りまでに何回足せるか
            k = min(k, n)
            self.m += a * k
            if self.m >= (1 << (F.M_LEN + 1)):
                self.e += 1
                self.m >>= 1
            n -= k
    def __str__(self):
        m = format(self.m, 'b')[-F.M_LEN:]
        e = format(self.e, '0' + str(F.E_LEN) + 'b')
        return e + m

def solve(n, b):
    '''ビット列bをn回加算'''
    a = F(b)
    s = F(b)
    s.addntimes(n, a)
    return s

while True:
    n = int(input())
    if n == 0: break
    b = input()
    assert len(b) == 52
    print(solve(n, b))
