import java.util.Scanner;
import java.util.Arrays;
import java.util.Comparator;

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
    Character[] rank = new Character['Z' - 'A' + 1];
    for (char p = 'A'; p <= 'Z'; p++) {
      rank[p - 'A'] = p;
    }

    for (int i = 0; i < c.length; i++) {
      score[c[i]]++; // c[i]の得票数
      Arrays.sort(rank, new Comparator<Character>() { // 並べ直し
        public int compare(Character a, Character b) {
          return score[b] - score[a];
        }
      });
      n--; // 残り票数

      // 残り票がすべて第2位に投票されても第1位に勝てないなら決定
      if (score[rank[0]] > score[rank[1]] + n) {
        return "" + rank[0] + " " + (i + 1);
      }
    }
    // ここに到達したなら第1位と第2位は同点
    return "TIE";
  }

}
