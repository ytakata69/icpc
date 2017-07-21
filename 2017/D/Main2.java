import java.util.*;

/**
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * Problem D: Making Lunch Boxes
 *
 * 常に動的計画法を使う版
 */
class Main2 {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int n = scanner.nextInt();
      int m = scanner.nextInt();
      if (n == 0 && m == 0) { break; }
      BitSet[] bitSet = new BitSet[n];
      for (int i = 0; i < n; i++) {
        String b = scanner.next();
        bitSet[i] = new BitSet(m);
        for (int j = 0; j < m; j++) {
          bitSet[i].set(j, b.charAt(j) == '1');
        }
      }
      System.out.println(new Solver(n, m, bitSet).solve());
    }
  }
}

class Solver {
  int n, m;
  BitSet[] bitSet;

  Solver(int n, int m, BitSet[] bitSet) {
    this.n = n;
    this.m = m;
    this.bitSet = bitSet;
  }

  /** 全ビットが0 */
  final static BitSet zero = new BitSet();

  /**
   * 動的計画法で解く
   */
  int solve() {
    // table ∋ (k, v) such that
    //   k: 可能なxor和
    //   v: xor和が k になる最大部分集合の要素数
    Map<BitSet, Integer> table = new HashMap<>();
    table.put(zero, 0);  // 空集合の和はzero (全ビット0)

    // 母集合にbitSet[b]を加えていく
    for (int b = 0; b < n; b++) {
      Map<BitSet, Integer> subTable = new HashMap<>(table); // 一時的コピー

      // bitSet[0..b-1]で和iが作れる ⇒ bitSet[0..b]で和i+bitSet[b]が作れる
      for (BitSet i: table.keySet()) {
        Integer n = table.get(i);  // 和iが得られる最大部分集合の要素数
        BitSet ii = (BitSet) i.clone();
        ii.xor(bitSet[b]);         // ii = i + bitSet[b]
        Integer old = subTable.get(ii);
        if (old == null || old < n + 1) {
          subTable.put(ii, n + 1);
        }
      }
      table = subTable;
    }

    // 和が zero である最大部分集合の要素数
    return table.get(zero);
  }
}
