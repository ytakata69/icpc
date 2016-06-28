import java.util.Scanner;

class C {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int m = scanner.nextInt();
      int n = scanner.nextInt();
      if (m == 0 && n == 0) break;
      System.out.println(solve(m, n));
    }
  }

  final static int MAX_SCORE = 7368791;

  /**
   * n個の区画に寿命m年以上の竹を植えたとき,
   * m年以降, 初めて花が咲かない年の最大値を答える.
   *
   * m年以降, x年後に初めて花が咲かないとする.
   * このとき, m以上x未満のxの約数kは存在しない
   *  (k年に咲いているならx年も咲いている. k年に咲いていないなら
   *   xが初めて花が咲かない年であることに矛盾).
   * 従って, 残り区画があるならx年竹を植えなければならない.
   *  (植えなければ答はx，植えれば答はxより大きい.)
   *
   * @param m 竹の寿命の最小値
   * @param n 区画の数
   */
  public static int solve(int m, int n) {
    boolean[] bloom = new boolean[MAX_SCORE + 1]; // 花の咲く年

    for (int x = m; ; x++) {
      if (! bloom[x]) { // x年は花が咲かない
        if (n == 0) return x; // もう区画がなければxが答
        n--; // (x年竹を植えて) 残り区画を減らす
        // xの倍数年は花が咲く
        for (int mx = x; mx < bloom.length; mx += x) {
          bloom[mx] = true;
        }
      }
    }
  }

}
