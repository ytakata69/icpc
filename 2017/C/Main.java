import java.util.*;

/**
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * C: A Garden with Ponds
 */
class Main {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int d = scanner.nextInt();
      int w = scanner.nextInt();
      if (d == 0 && w == 0) { break; }
      int[][] e = new int[d][w];
      for (int y = 0; y < d; y++) {
        for (int x = 0; x < w; x++) {
          e[y][x] = scanner.nextInt();
        }
      }
      System.out.println(solve(e));
    }
  }

  static int solve(int[][] e) {
    int maxPond = 0;
    // 北西隅の座標(x,y)と東西幅w, 南北幅dの組合せをすべて調べる
    for (int y = 0; y < e.length; y++) {
      for (int d = 3; y + d <= e.length; d++) {
        for (int x = 0; x < e[y].length; x++) {
          for (int w = 3; x + w <= e[y].length; w++) {
            int f = fence(e, x, y, w, d);    // 外輪の高さ
            int p = pond (e, x, y, w, d, f); // 水量
            maxPond = Math.max(maxPond, p);
          }
        }
      }
    }
    return maxPond;
  }

  /** 池の水量 */
  static int pond(int[][] e, int ox, int oy, int w, int d, int fence) {
    int sum = 0;
    for (int y = oy + 1; y < oy + d - 1; y++) {
      for (int x = ox + 1; x < ox + w - 1; x++) {
        int diff = fence - e[y][x];  // このセルの水量
        if (diff <= 0) { return 0; } // 外輪の高さ以上だった
        sum += diff;
      }
    }
    return sum;
  }

  /** 外輪の高さ (最小値) */
  static int fence(int[][] e, int ox, int oy, int w, int d) {
    int min = Integer.MAX_VALUE;  // ∞ (最小値演算の単位元)
    min = Math.min(min, minInSegment(e[oy], ox, w));         // 北辺
    min = Math.min(min, minInSegment(e[oy + d - 1], ox, w)); // 南辺
    for (int y = oy; y < oy + d; y++) {
      min = Math.min(min, e[y][ox]);         // 西辺
      min = Math.min(min, e[y][ox + w - 1]); // 東辺
    }
    return min;
  }

  /** 東西方向の線分の高さ (最小値) */
  static int minInSegment(int[] e, int x, int w) {
    int m = e[x];
    for (int i = 1; i < w; i++) {
      m = Math.min(m, e[x + i]);
    }
    return m;
  }
}
