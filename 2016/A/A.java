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
    int min = a[1] - a[0];
    for (int i = 2; i < a.length; i++) {
      min = Math.min(min, a[i] - a[i-1]);
    }
    return min;
  }
}
