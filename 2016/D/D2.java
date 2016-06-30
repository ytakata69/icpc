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

  /**
   * U先生のアルゴリズム. 簡潔だ...
   *
   * @param  w ブロックの重みの配列
   * @return 消せるブロック数の最大値
   */
  static int solve(int[] w) {
    int n = w.length; // ブロック数

    // cost[i][j]: 区間[i,j]における最小ブロック残数
    int[][] cost = new int[n][n];

    for (int i = 0; i < n; i++) {
      cost[i][i] = 1; // 1ブロックだけの区間は消せないのでコスト1
    }

    for (int l = 1; l < n; l++) { // 区間の長さ
      // cost[i][i + l]を求める
      for (int i = 0; i < n - l; i++) {
        // 中央部を消した後両端を消せる
        if ((l == 1 || cost[i + 1][i + l - 1] == 0) &&
            Math.abs(w[i] - w[i + l]) <= 1)
        {
          cost[i][i + l] = 0;
          continue;
        }
        // 2つに区切ったときの最小コスト
        int min = Integer.MAX_VALUE;
        for (int m = 0; m < l; m++) {
          min = Math.min(min, cost[i][i + m] + cost[i + m + 1][i + l]);
        }
        cost[i][i + l] = min;
      }
    }
    return n - cost[0][n-1]; // 消せるブロック数
  }

}
