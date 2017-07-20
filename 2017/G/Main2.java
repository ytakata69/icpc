import java.util.*;

/**
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * Problem G: Go around the Labyrinth
 *
 * 再帰呼び出し版
 */
class Main2 {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int N = scanner.nextInt();
      int M = scanner.nextInt();
      if (N == 0 && M == 0) { break; }
      Explorer exp = new Explorer(N, M);
      for (int y = 0; y < N; y++) {
        String line = scanner.next();
        for (int x = 0; x < M; x++) {
          exp.setMap(x, y, line.charAt(x));
        }
      }
      System.out.println(exp.solve() ? "YES" : "NO");
    }
  }
}

class Explorer {
  int[][] map;
  int N;  // 南北幅
  int M;  // 東西幅

  Explorer(int N, int M) {
    map = new int[N][M];
    this.N = N;
    this.M = M;
  }

  void setMap(int x, int y, char c) {
    map[y][x] = (c == '.' ? 0 : -1);
  }

  final int[] vx = { 1, 0, -1,  0 }; // 東南西北
  final int[] vy = { 0, 1,  0, -1 };
  int[] goalX; // 宝 (と出口) の位置
  int[] goalY;

  enum Result { YES, NO, BACKTRACK }; // 探索の結果

  boolean solve() {
    goalX = new int[] { M-1, M-1,   0, 0 }; // 宝 (と出口) の位置
    goalY = new int[] {   0, N-1, N-1, 0 };

    // 北西隅から東に向かって探索開始
    return explore(0, 0, 0, 0) == Result.YES;
  }

  /**
   * 指定した位置から左手の法則に従って深さ優先探索する。
   * @param x      現在位置のX座標
   * @param y      現在位置のY座標
   * @param dir    現在の進行方向 (0=東, 1=南, 2=西, 3=北)
   * @param target どの宝 (または出口) に向かっているか
   * @return 探索の結果 (YES=ゴールした, NO=ゴール不可能, BACKTRACK=戻る)
   */
  Result explore(int x, int y, int dir, int target) {
    map[y][x] = 1; // 通った場所に色を塗る (0でなければ何でも)

    // 左→前→右の順に試す (左手の法則)
    for (int d = -1; d <= 1; d++) {
      int dd = (dir + d + 4) % 4;  // dd = dir + d (mod 4)
      int xx = x + vx[dd];
      int yy = y + vy[dd];
      if (xx < 0 || xx >= M || yy < 0 || yy >= N) { continue; }
      if (map[yy][xx] == -1) { continue; }

      if (xx == goalX[target] && yy == goalY[target]) {
        target++; // 次の宝を目指す
        if (target == 4) { return Result.YES; }
      }
      // 壁または通過済み
      if (map[yy][xx] != 0) { continue; }

      // 一歩進む (結果がわかったら探索打ち切り)
      switch (explore(xx, yy, dd, target)) {
      case YES: return Result.YES;
      case NO:  return Result.NO;
      }
    }
    // 前の宝まで戻ってしまったら失敗
    if (target > 0 && x == goalX[target-1] && y == goalY[target-1]) {
      return Result.NO;
    }
    return Result.BACKTRACK;
  }

}
