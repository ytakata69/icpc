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
    map[y][x] = c;
  }

  final int[] vx = { 1, 0, -1,  0 }; // 東南西北
  final int[] vy = { 0, 1,  0, -1 };

  boolean solve() {
    // 宝
    map[0]  [M-1] = '$';
    map[N-1][M-1] = '$';
    map[N-1][0]   = '$';

    // 北西隅から東に向かって探索開始
    return explore(0, 0, 0, 0);
  }

  /**
   * 指定した位置から左手の法則に従って深さ優先探索する。
   * @param x    現在位置のX座標
   * @param y    現在位置のY座標
   * @param dir  現在の進行方向 (0=東, 1=南, 2=西, 3=北)
   * @param gold 持っている宝の数
   */
  boolean explore(int x, int y, int dir, int gold) {
    if (map[y][x] == '$') { gold++; } // お宝get

    map[y][x] = 0; // 通った場所に色を塗る ('.'でなければ何でも)

    // 左→前→右の順に試す (左手の法則)
    for (int d = -1; d <= 1; d++) {
      int dd = (dir + d + 4) % 4;  // dd = dir + d (mod 4)
      int xx = x + vx[dd];
      int yy = y + vy[dd];
      if (xx < 0 || xx >= M || yy < 0 || yy >= N) { continue; }
      // 出発地点に戻った
      if (xx == 0 && yy == 0) { return gold >= 3; }
      if (map[yy][xx] == '#') { continue; } // 壁
      if (map[yy][xx] == 0)   { continue; } // 通過済み

      // 一歩進む (成功したら探索打ち切り)
      if (explore(xx, yy, dd, gold)) { return true; }
    }
    return false;
  }

}
