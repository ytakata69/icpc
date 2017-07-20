import java.util.*;

/**
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * Problem G: Go around the Labyrinth
 */
class Main {
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

  /**
   * 北西隅から左手の法則に従って時計回りに探索する。
   * (最初に南東隅に行くと，北東と南西の両方に行くことは不可能。
   *  ある順路が存在すれば逆にたどることも可能なので，最後に南東隅から
   *  北西出口に行くことも不可能。
   *  逆にたどることが可能なので，反時計回りの順路があるなら時計回りの
   *  順路もある。)
   * (なるべく外周に近いところを通る方が，通過不可能になる面積が小さくなる。)
   */
  boolean solve() {
    final int[] vx = { 1, 0, -1,  0 }; // 東南西北
    final int[] vy = { 0, 1,  0, -1 };
    final int[] goalX = { 0, M-1, M-1,   0, 0 }; // 宝 (と出口) の位置
    final int[] goalY = { 0,   0, N-1, N-1, 0 };

    // 北西隅から東に向かって探索開始
    int x = 0;     // 現在位置のX座標 (東西座標)
    int y = 0;     // 現在位置のY座標 (南北座標)
    int dir = 0;   // 進行方向。東南西北 = 0,1,2,3
    int color = 1; // 地図に塗る色。どの宝 (または出口) に向かっているかを表す。
    while (true) {
      // 通った場所に色を塗る (どの宝へ向かっているか & 何回目)
      map[y][x] = (color << 3) | ((map[y][x] & 7) + 1);
      int d;
      for (d = -1; d <= 2; d++) { // 左→前→右→後の順に試す (左手の法則)
        int dd = (dir + d + 4) % 4;  // dd = dir + d (mod 4)
        int xx = x + vx[dd];
        int yy = y + vy[dd];
        if (xx < 0 || xx >= M || yy < 0 || yy >= N) { continue; }
        if (map[yy][xx] == -1) { continue; } // 壁
        if (xx == goalX[color] && yy == goalY[color]) {
          if (color == 4) { return true; }
          color++; // 次の宝を目指す
        }
        // 古い色で塗られていたら通過済み
        if (0 < map[yy][xx] && map[yy][xx] < color << 3) { continue; }
        // 同じ色でも4回通っていたらやめる
        if ((map[yy][xx] & 7) >= 4) { continue; }
        x = xx;
        y = yy;
        dir = dd;
        break;
      }
      if (d > 2) { return false; }
    }
  }

}
