import java.util.*;

class D {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int n = scanner.nextInt();
      if (n == 0) break;
      int[] w = new int[n];
      for (int i = 0; i < n; i++) {
        w[i] = scanner.nextInt();
      }
      System.out.println(solve(w));
    }
  }

  static int solve(int[] w) {
    // 消去可能な区間を求める
    Interval[] interval = erasable(w);

    if (interval.length == 0) return 0; // どこも消去できない

    // 合計長最大の, 交差しない区間集合を求める
    // (区間長を重みとする重み付き区間スケジューリング問題)
    int[] opt = new int[interval.length]; // interval[0..i]の最大重み和
    opt[0] = interval[0].v();
    for (int i = 1; i < interval.length; i++) {
      int p; // interval[i]と交差しない終了ブロック番号最大の区間
      for (p = i - 1; p >= 0; p--) {
        if (interval[p].f < interval[i].s) break;
      }
      opt[i] = Math.max(interval[i].v() + (p < 0 ? 0 : opt[p]), opt[i - 1]);
    }

    return opt[interval.length - 1];
  }

  /**
   * 動的計画法で消去可能な区間を求める.
   * @return 消去可能な区間の配列 (終了ブロック番号の昇順)
   */
  static Interval[] erasable(int[] w) {
    int n = w.length;

    // erasable[i][j]==true <=> 区間[i,j]を消去可能
    boolean[][] erasable = new boolean[n][n];

    // 区間集合
    Set<Interval> set = new TreeSet<>(); // TreeSetを使って整列

    // l: 区間の長さ
    for (int l = 1; l < n; l += 2) {
      // 区間[i, i + l]が消去可能か
      for (int i = 0; i < n - l; i++) {
        // 中央部を消した後両端を消せる
        if ((l == 1 || erasable[i + 1][i + l - 1]) &&
            Math.abs(w[i] - w[i + l]) <= 1)
        {
          erasable[i][i + l] = true;
        }
        // 2つに区切ってそれぞれ消去可能
        for (int m = 1; m < l; m += 2) {
          if (erasable[i][i + m] && erasable[i + m + 1][i + l]) {
            erasable[i][i + l] = true;
            break;
          }
        }
        // 区間集合に追加
        if (erasable[i][i + l]) {
          set.add(new Interval(i, i + l));
        }
      }
    }
    return set.toArray(new Interval[0]); // 配列に変換
  }

  /**
   * 区間.
   * 終了ブロック番号の順に整列可能.
   */
  static class Interval implements Comparable<Interval> {
    int s; // 開始ブロック番号
    int f; // 終了ブロック番号

    Interval(int s, int f) {
      this.s = s;
      this.f = f;
    }

    /** 重み (== 区間の長さ) */
    int v() { return f - s + 1; }

    /** 終了ブロック番号による比較 */
    @Override
    public int compareTo(Interval that) {
      return this.f == that.f ? this.s - that.s : this.f - that.f;
    }
    @Override
    public boolean equals(Object obj) {
      if (obj == null || ! (obj instanceof Interval)) return false;
      Interval that = (Interval)obj;
      return this.s == that.s && this.f == that.f;
    }
    @Override
    public String toString() {
      return "[" + s + "," + f + "]";
    }
  }

}
