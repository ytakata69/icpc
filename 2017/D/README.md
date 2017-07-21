# Problem D: Making Lunch Boxes

- Main.java - n ≦ m かどうかで総当り法と動的計画法を切り替える。
- Main2.java - 常に動的計画法を使う。
    - 配列の代わりにHashMapを使うことで，mが大きい場合にも対応。

動的計画法の計算量は，配列を使う場合，O(n・2^m)
(int同士のxorはO(1)と仮定)．
HashMapを使う場合，O(n・m・min(2^n, 2^m))
(mビットのxorはO(m)と仮定，
HashMapの追加・検索はO(1)と仮定)．
