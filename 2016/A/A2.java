import java.util.Scanner;
import java.util.Arrays;

class A {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int n = scanner.nextInt();
      if (n == 0) break;
      int[] a = new int[n];
      for (int i = 0; i < n; i++) {
        a[i] = scanner.nextInt();
      }
      System.out.println(solve(a));
    }
  }

  public static int solve(int[] a) {
    Arrays.sort(a);
    for (int i = 0; i < a.length - 1; i++) {
      a[i] = a[i + 1] - a[i];  // 差分
    }
    Arrays.sort(a, 0, a.length - 1); // 最終要素を除いて整列
    return a[0];
  }
}
