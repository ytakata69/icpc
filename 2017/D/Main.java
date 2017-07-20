import java.util.*;

/**
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * Problem D: Making Lunch Boxes
 */
class Main {
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

  int solve() {
    // n が小さければ総当り, m が小さければ動的計画法
    return n <= 23 ? bruteForce() : dynamicProgramming();
  }

  /**
   * 総当りで解く (n が小さい場合)
   */
  int bruteForce() {
    return bruteForce(0, new BitSet(), 0);
  }

  /**
   * 再帰的に全部分集合を試す
   * @param pos  処理中の位置 (0..n-1)
   * @param sum  bitSet[0..pos-1]のある部分集合 (の総xor)
   * @param size bitSet[0..pos-1]のある部分集合 (の要素数)
   */
  int bruteForce(int pos, BitSet sum, int size) {
    if (pos >= n) { // 再帰の終端
      // 和が0なら要素数を返す
      return sum.isEmpty() ? size : 0;
    }
    // bitSet[pos]を加えない場合
    int a1 = bruteForce(pos + 1, sum, size);

    // bitSet[pos]を加えた場合
    sum.xor(bitSet[pos]);
    int a2 = bruteForce(pos + 1, sum, size + 1);
    sum.xor(bitSet[pos]); // 元に戻す

    return Math.max(a1, a2);
  }

  /**
   * 動的計画法で解く (m が比較的小さい場合)
   */
  int dynamicProgramming() {
    // table[i]: 和が i である最大部分集合の要素数
    //           和が i である部分集合が存在しなければ -1
    int[] table = new int[1 << m];

    for (int i = 1; i < 1 << m; i++) { table[i] = -1; }
    table[0] = 0;  // 空集合の和は0

    // 母集合にbitSet[b]を加えていく
    for (int b = 0; b < n; b++) {
      int[] subTable = new int[table.length];  // tableの一時的コピー
      for (int i = 0; i < 1 << m; i++) { subTable[i] = table[i]; }

      int item = (int) bitSet[b].toLongArray()[0]; // ビット列をintに変換

      for (int i = 0; i < 1 << m; i++) {
        if (table[i] != -1) {
          // bitSet[0..b-1]で和iが作れる ⇒ bitSet[0..b]で和iiが作れる
          int ii = i ^ item;
          subTable[ii] = Math.max(subTable[ii], table[i] + 1);
        }
      }
      table = subTable;
    }

    // 和が 0 である最大部分集合の要素数
    return table[0];
  }
}
