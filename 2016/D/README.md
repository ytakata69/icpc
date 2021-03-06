# Problem D: ダルマ落とし

<http://icpc.iisf.or.jp/past-icpc/domestic2016/problems/all_ja.html#section_D>

<dl>
  <dt>D.java</dt>
  <dd>消去可能区間をすべて求める →
      重み付き区間スケジューリング問題として解く</dd>
  <dt>D2.java</dt>
  <dd>直接，動的計画法で最適解を求める (こっちの方がスマート)</dd>
</dl>

## あぁ動的計画法...

動的計画法ってなに? という人への簡単な解説。

(詳しくはアルゴリズムの本を読んでね。
以下では「時間がない人のための」的な解説を書いてみる。)

### 分割統治法, 動的計画法, 貪欲法

「小さい問題に分解してそれぞれ解き，それらの解をくっつける」
という方法で解ける問題はけっこうたくさんある。

そのとき使える主なアルゴリズムは以下の3通り:

1. 分割統治法 (divide-and-conquer)
2. 動的計画法 (dynamic programming)
3. 貪欲法 (greedy algorithm)

どれが使えるかは問題の性質による。ので，3つとも覚えておいて，その問題にはどれが使えるか吟味した上で使う必要がある
(ICPC的には動的計画法の出番が多いと思うけど)。

### 分割統治法

以下のような疑似コードで表されるアルゴリズム。(本当は，問題が小さいときは再帰しないようにして再帰を終わらせる必要があるけど，簡単のため省いている。)

```
solve(問題) {
  1: 問題を前半と後半に分ける.
  2: a1 = solve(前半);
  3: a2 = solve(後半);
  4: a1とa2を使って問題全体の解を求める.
}
```

例えばクイックソートだと，1.で適当な基準値より小さい要素の列と基準値以上の要素の列に分ける。
4.では特に何もしなくてよい。  
マージソートだと，1.では単に前半分と後ろ半分に分けるだけ，
4.ではa1とa2に対してマージ処理を行う。

### 部分構造最適性

以降，最適解を求める問題 (何かを最大または最小にする問題) についてだけ考える。

上記の3つのアルゴリズム (分割統治法, 動的計画法, 貪欲法)
はどれも，問題が**部分構造最適性** (optimal substructure) を持つ場合に限って適用できる。

<dl>
  <dt>部分構造最適性 (または最適性原理ともいう)</dt>
  <dd>全体の最適解から一部を抜き出すと，それは部分問題の最適解になっている。</dd>
</dl>

言い換えると「問題全体の最適解は，部分問題の最適解をくっつけたものに限られる」。

例えば，最短経路の問題がわかりやすいかな。
A地点からB地点までの最短経路 P をカーナビが教えてくれたとする。
Pの途中の地点を一つ選んでCとする。

```
AからBまでの最短経路P   A -------->C--------> B
```

PをCの前後に区切ったとき，Pの前半は**AからCまでの最短経路**になっている。
Pの後半は**CからBまでの最短経路**になっている。当たり前デスネ。これが部分構造最適性。最適解の一部を抜き出すと，それもやはり最適解。

ダルマ落としも実は部分構造最適性を持っている。例えば下記は，10個のブロックの最適な消し方 (どのブロックをペアにして消すか) を示しているが，ブロック1〜6の区間だけ見ても，その区間の最適な消し方になっている。ブロック7〜10の区間を見ても，その区間の最適な消し方になっている。

```
   +----+
10 | 5  |-+
   +----+ |
 9 | 6  |-+
   +----+
 8 | 4  |
   +----+
 7 | 10 |
   +----+
 6 | 1  |---+ 
   +----+   |  
 5 | 4  |-+ |  
   +----+ | |  
 4 | 5  |-+ |  
   +----+   |  
 3 | 3  |-+ |  
   +----+ | |  
 2 | 4  |-+ |  
   +----+   | 
 1 | 1  |---+  
   +----+     
```

### 分割統治法ではだめな問題

例えばダルマ落としで，もし「好きなところで問題を2分割して前半と後半の最適解をそれぞれ求めれば，それが全体の最適解になっている」のだとしたら，分割統治法で解くことができる。長さ半分の区間 (上の例なら区間1〜5と6〜10) に分割して，それぞれの解を求めればよい。しかし，上の例で区間1〜5の最適解と区間6〜10の最適解を求めてそれらをくっつけても，全体の最適解にはならない (その解には「ブロック1と6のペア」が含まれないから)。

「最適解におけるペアを離ればなれにしない分割」をしないといけないが，それは事前にはわからない。

ではどうするか? というと，いろんな分割を試して一番よいものを選べばよい。例えば分割統治法を少し変更して以下のようにすればよい (ある数値を最大化する問題と仮定している) (このままだと計算量が大きすぎるが，その話は後回し)。単純に，分割の仕方を全部試して，一番よいものを選ぶだけ。

```
// アルゴリズム A
solve(問題) {
  1: opt = -∞
  2: for (m : 問題の分割位置) {
  3:   m の位置で問題を前半と後半に分ける
  4:   a1 = solve(前半)
  5:   a2 = solve(後半)
  6:   a = a1とa2から求めるこの分け方での問題の解
  7:   opt = max(opt, a)  // 最適値を更新
     }
  8: return opt
}
```

どういう分割を試せば十分か? は，その問題をよく見て考える必要がある。ダルマ落としの場合，区間を2つに分けるのだから，おそらく「1個とn&minus;1個」「2個とn&minus;2個」…というn&minus;1通りの分割を試せばよいのでは? とまず考える。もし，これらのうちのどの分割でも「最適解においてペアにすべきブロックが離ればなれになる」としたら，それは「ブロック 1 と n をペアにすべき」場合だけ。そこでもう1つ，「ブロック2〜n&minus;1の区間とブロック1とn」という分割も加える。
これらの各分割から求まる解の中に，全体の最適解が必ずある。

ちょっと難しい?  
でもまぁこの部分が一番頭を使うところで，これ (問題が部分構造最適性を持っている，最適解はこれこれの分割の中から必ず見つかる) が発見できれば，その問題は解けたも同然。

###動的計画法

上に書いた「分割候補の中から一番よいものを探す」アルゴリズム A は計算量が大きすぎるのだけど，問題が以下の性質を持っていれば，計算量を改善できる。

<dl>
<dt>部分問題重複性 (overlapping subproblems)</dt>
<dd>部分問題の総数があまり多くない。</dd>
</dl>

例えばダルマ落としだと，
部分問題は「ブロック i から j までの区間」で，
その総数は i と j の組み合わせの総数だから
n<sup>2</sup> 程度。

すべての部分問題の解が求まれば全体の解もわかるので，じゃあ小さい問題から順に，結果を保存しながら全部解いていきましょう，というのが**動的計画法** (dynamic programming)。疑似コードで書くと以下のような感じ。

```
// アルゴリズム DP
solve(問題) {
  1: for (p : 大きさ 1 の部分問題) {
  2:   opt[p] = pの最適解
     }
  3: for (s : 2..n) {
  4:   for (p : 大きさ s の部分問題) {
  5:     opt[p] = -∞
  6:     for (m : pの分割位置) {
  7:       m の位置で p を前半と後半に分ける
  8:       a1 = opt[前半]
  9:       a2 = opt[後半]
 10:       a = a1とa2から求めるこの分け方での p の解
 11:       opt[p] = max(opt[p], a)  // 最適値を更新
         }
       }
     }
 12: return opt[問題]
}
```

行5〜11に，前述のアルゴリズムAがほぼそのまま入っている。
ただし，アルゴリズムAでは再帰呼び出しだった箇所が，上記アルゴリズム DP では配列の参照になっている。部分問題 p を解くときには p より小さい部分問題はすべて解き終わっているので，配列 opt に保存されている結果を見るだけでよい。全体の計算量は「部分問題の総数 × 一つの問題の分割候補数」だから，ダルマ落としだと n<sup>3</sup> 程度。

図的なイメージでいうと，部分問題の総数だけ要素を持つ配列 opt があって，その中身をどこかの端から順番に全部埋めていく，という感じ。「要素 opt[p] を計算する際に必要な要素はそれまでに計算済み」になるように，埋めていく順番をうまく決めてやればよい。opt[p] を計算するのに高々 m 個の要素を調べる必要があれば，全体の計算量は「部分問題の総数 × m」程度になる。

#### メモ化

先のアルゴリズム A と同じ形 (トップダウン形) で，ただし一度計算した部分問題の解は配列に保存しておいて再計算しないようにする，という方法もある。これが**メモ化** (memoization)。結局は「すべての部分問題について分割候補をすべて試す」「小さい問題から順に解が求まっていく」ことになるので，上のアルゴリズム DP (ボトムアップ形) と本質的にはほぼ同じ。

メモ化だと「『要素 opt[p] を計算する際に必要な要素はそれまでに計算済み』になるような順番」を考えなくてよい。必要になったときに，計算済みなら保存されている結果を使うし，まだ計算されていなければそこで計算する。その代わり，再帰呼び出しのために実行制御スタックを余分に消費する (つまりボトムアップの方が少し効率がよい。でも実際はほとんど差はないでしょう)。