import java.util.Scanner;

class B {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int n = scanner.nextInt();
      if (n == 0) break;
      char[] c = new char[n];
      for (int i = 0; i < n; i++) {
        c[i] = scanner.next().charAt(0);
      }
      System.out.println(solve(n, c));
    }
  }

  public static String solve(int n, char[] c) {
    int[] score = new int['Z' + 1];  // (-'A')は面倒なので省略
    char top    = 'A'; // 現時点の1位
    char second = 'B'; // 現時点の2位
    for (int i = 0; i < c.length; i++) {
      score[c[i]]++; // c[i]の得票数
      // topとsecondを更新
      if (c[i] != top && score[c[i]] > score[second]) {
        second = c[i];
      }
      if (score[c[i]] > score[top]) {
        second = top;
        top = c[i];
      }
      n--; // 残り票数

      // 残り票がすべてsecondに投票されてもtopに勝てないなら決定
      if (score[top] > score[second] + n) {
        return "" + top + " " + (i + 1);
      }
    }
    // ここに到達したならscore[top]==score[second]である
    return "TIE";
  }

}
