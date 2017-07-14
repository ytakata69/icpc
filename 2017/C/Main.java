import java.util.*;

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
    // System.out.println("solve: " + e.length + "," + e[0].length);
    int maxPond = 0;
    for (int y = 0; y < e.length; y++) {
      for (int d = 3; y + d <= e.length; d++) {
        for (int x = 0; x < e[y].length; x++) {
          for (int w = 3; x + w <= e[y].length; w++) {
            int f = fence(e, x, y, w, d);
            int p = pond (e, x, y, w, d, f);
            // System.out.print(x + "," + y + "," + w + "," + d);
            // System.out.println(" fence=" + f + " pond=" + p);
            maxPond = Math.max(maxPond, p);
          }
        }
      }
    }
    return maxPond;
  }

  static int pond(int[][] e, int x, int y, int w, int d, int fence) {
    int sum = 0;
    for (int y_ = y + 1; y_ < y + d - 1; y_++) {
      for (int x_ = x + 1; x_ < x + w - 1; x_++) {
        int diff = fence - e[y_][x_];
        if (diff <= 0) { return 0; }
        sum += diff;
      }
    }
    return sum;
  }

  static int fence(int[][] e, int x, int y, int w, int d) {
    int min = Integer.MAX_VALUE;
    min = Math.min(min, minInSegment(e[y], x, w));
    min = Math.min(min, minInSegment(e[y + d - 1], x, w));
    for (int y_ = y; y_ < y + d; y_++) {
      min = Math.min(min, e[y_][x]);
      min = Math.min(min, e[y_][x + w - 1]);
    }
    return min;
  }

  static int minInSegment(int[] e, int x, int w) {
    int m = e[x];
    for (int i = 0; i < w; i++) {
      m = Math.min(m, e[x + i]);
    }
    return m;
  }
}
