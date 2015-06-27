import java.util.*;

class Main {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int n = scanner.nextInt();
      if (n == 0) break;
      Deque<Pair> list = new LinkedList<Pair>();
      for (int i = 0; i < n; i++) {
        String line = scanner.next();
        Pair pair = parse(line);
        list.add(pair);
      }
      System.out.println(compute(list));
    }
  }

  static Pair parse(String line) {
    int i;
    for (i = 0; i < line.length() && line.charAt(i) == '.'; i++) {}
    return new Pair(i, line.charAt(i));
  }

  static int compute(Deque<Pair> list) {
    Pair head = list.poll();  // get and remove
    int depth = head.depth;
    int op    = head.op;
    if ('0' <= op && op <= '9') { return op - '0'; }
    int acc = (op == '+' ? 0 : 1);
    while (! list.isEmpty() && list.getFirst().depth > depth) {
      int sub = compute(list);
      acc = (op == '+' ? acc + sub : acc * sub);
    }
    return acc;
  }

  static class Pair {
    int depth;
    int op;
    Pair(int depth, int op) {
      this.depth = depth;
      this.op    = op;
    }
  }
}
