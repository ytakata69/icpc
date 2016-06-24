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
      System.out.println(new Dharma(w).solve());
    }
  }

  static class Dharma {
    public Dharma(int[] w) {
      this.w = w;
    }
    int[] w;
    Map<BitSet,Integer> memo = new HashMap<>();

    /**
     * 深さ優先探索し, 取り除けるブロック数の最大を返す.
     */
    public int solve() {
      return solve(new BitSet(w.length));
    }

    /**
     * @param removed 削除済みブロックを表すビット列
     */
    private int solve(BitSet removed) {
      Integer x = memo.get(removed);
      if (x != null) return x.intValue();

      int max = 0;
      int prev = -1;
      for (int i = 0; i < w.length; i++) {
        if (removed.get(i)) continue; // ブロックiは削除済み
        // 差が1以下
        if (prev >= 0 && Math.abs(w[prev] - w[i]) <= 1) {
          // prevとiを削除したコピーを作る
          BitSet removedX = (BitSet)removed.clone();
          removedX.set(prev);
          removedX.set(i);
          int score = 2 + solve(removedX); // 2個叩き出す + その後の得点
          max = Math.max(max, score);
        }
        prev = i;
      }
      memo.put(removed, max); // メモ化
      return max;
    }

  }

}
