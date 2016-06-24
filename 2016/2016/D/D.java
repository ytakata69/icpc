import java.util.Scanner;

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
    public Dharma() {}

    public Dharma(int[] w) {
      this.w = w;
    }
    int[] w;

    /**
     * 深さ優先探索し, 取り除けるブロック数の最大を返す.
     */
    public int solve() {
      int max = 0;
      for (int i = 1; i < w.length; i++) {
        // 差が1以下
        if (Math.abs(w[i-1] - w[i]) <= 1) {
          Dharma next = kickOut(i-1); // 叩き出した後を表すオブジェクト
          int score = 2 + next.solve(); // 2個叩き出す + その後の得点
          max = Math.max(max, score);
        }
      }
      return max;
    }

    /**
     * i, i+1番ブロックを叩き出した後を表す新しいオブジェクトを返す.
     */
    public Dharma kickOut(int i) {
      Dharma next = new Dharma();
      next.w = new int[this.w.length - 2];
      for (int j = 0; j < i; j++) {
        next.w[j] = this.w[j];
      }
      for (int j = i; j < next.w.length; j++) {
        next.w[j] = this.w[j + 2];
      }
      return next;
    }
  }

}
