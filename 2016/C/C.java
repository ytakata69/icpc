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
   * m以上の整数n個からなる集合Mを考える.
   * Mの要素の倍数ではないm以上の整数のうち最小のものが, Mの得点.
   * 得点が最大のMを見つけ, その得点を返せばよい.
   *
   * Mは必ずmを含む. そうでなければ得点が m (最小).
   * Mの要素の倍数はMに加える必要がない (加えても得点は増えない).
   *
   * アルゴリズム:
   * n個の区画0..(n-1)に順にm以上の整数を割り当てる.
   * 区画0にはmを割り当てる.
   * 区画iには, それまでに割り当てた数の倍数ではない最小の数を割り当てる.
   *
   * @param m 竹の寿命の最小値
   * @param n 区画の数
   */
  public static int solve(int m, int n) {
    boolean[] bloom = new boolean[MAX_SCORE + 1]; // 花の咲く年
    int x;
    for (x = m; true; x++) {
      if (! bloom[x]) { // xは花が咲かない年
        if (n == 0) return x; // もう区画がなければxが答
        n--; // 次の区画にx年竹を植えて残り区画を減らす
        // xの倍数を候補から除外
        for (int mx = x; mx < bloom.length; mx += x) {
          bloom[mx] = true;
        }
      }
    }
  }

}
