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

    // erasable[i][j]: 区間[i,j]における最大消去可能ブロック数
    int[][] erasable = new int[n][n];

    for (int i = 0; i < n; i++) {
      erasable[i][i] = 0; // 1ブロックだけの区間は消せない
    }

    for (int l = 1; l < n; l++) { // 区間の長さ
      // erasable[i][i + l]を求める
      for (int i = 0; i < n - l; i++) {
        // 中央部を消した後両端を消せる
        if ((l == 1 || erasable[i + 1][i + l - 1] == l - 1) &&
            Math.abs(w[i] - w[i + l]) <= 1)
        {
          erasable[i][i + l] = l + 1;
          continue;
        }
        // 2つに区切ったときの最大消去可能数
        int max = 0;
        for (int m = 0; m < l; m++) {
          max = Math.max(max, erasable[i][i + m] + erasable[i + m + 1][i + l]);
        }
        erasable[i][i + l] = max;
      }
    }
    return erasable[0][n-1];
  }

}
