import java.util.*;

/**
 * ACM-ICPC domestic 2016
 * Problem E: 3D printing
 */
class E {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      int n = scanner.nextInt(); // 場所数
      int k = scanner.nextInt(); // 立方体数
      int s = scanner.nextInt(); // 1辺
      if (n == 0 && k == 0 && s == 0) break;
      Space space = new Space(s);
      for (int i = 0; i < n; i++) {
        int x = scanner.nextInt();
        int y = scanner.nextInt();
        int z = scanner.nextInt();
        space.addPosition(x, y, z);
      }
      System.out.println(space.solve(k));
    }
  }
}

/**
 * Class of the 3D space the cubes will be placed in.
 *
 * この3D空間は, 場所を節点とする無向グラフと見なせる.
 * 2つの場所に置いた立方体が重なるとき, その2節点は隣接している.
 * 問題設定より, 各節点の次数は高々2である.
 * 従って, 3D空間の連結成分は, 孤立節点, 単純パス, 閉路のいずれか.
 * 多面体は, 連結成分中の連続するk節点に立方体を置くことで得られる.
 */
class Space {
  /**
   * @param s 立方体の1辺
   */
  Space(int s) {
    this.s = s;
  }

  int s; // 立方体の1辺
  List<Position> pos = new ArrayList<>(); // 場所のリスト

  /**
   * 表面積最小の多面体の表面積を返す.
   * @param k 立方体数
   */
  int solve(int k) {
    // 場所を連結成分に分ける
    List<Component> component = findComponents();

    int min = -1;
    for (Component c : component) {
      // 可能な多面体の個数
      int n = (c.isCycle() && c.size() > k)
             ? c.size()          // 閉路中の連続するk節点の選び方
             : c.size() - k + 1; // 単純パス中の連続するk節点の選び方
      for (int h = 0; h < n; h++) {
        // 連結成分cのh番要素からk節点を選んで作った多面体の表面積
        int area = c.area(h, k);
        if (min < 0 || min > area) {
          min = area;
        }
      }
    }
    return min;
  }

  /**
   * 連結成分に分ける.
   * @return 連結成分のリスト
   */
  List<Component> findComponents() {
    List<Component> component = new ArrayList<>();

    // 次数1以下の場所を始点とする成分を列挙
    for (Position p : pos) {
      if (p.joined || p.degree() > 1) continue;
      Component c = new Component();
      component.add(c);
      traverse(c, p);
    }
    // 次数2の場所だけからなる成分(=閉路)を列挙
    for (Position p : pos) {
      if (p.joined) continue;
      Component c = new Component();
      component.add(c);
      traverse(c, p);
    }
    return component;
  }

  /** 連結成分の要素を訪問 */
  void traverse(Component c, Position p) {
    if (p == null || p.joined) return;
    c.add(p);
    p.joined = true;
    if (p.degree() == 0) return;
    traverse(c, !p.n1.joined ? p.n1 : p.n2);
  }

  /**
   * 連結成分を表すクラス.
   * 連結成分は, 孤立節点, 単純パス, 閉路のいずれか.
   */
  class Component extends ArrayList<Position> {
    /** この連結成分が閉路か? */
    boolean isCycle() {
      return size() != 0 && get(0).degree() == 2; // 先頭要素の次数が2
    }

    /** head番以降のk節点からなる多面体の表面積 */
    int area(int head, int k) {
      int area = 6 * s * s * k; // k個の立方体の表面積

      // 端 (head番) の一つ前
      Position prev = null;
      if (isCycle() && size() == k) { // 閉路状の多面体
        prev = get((head - 1 + size()) % size());
      }
      // 隠されている表面積を引く
      for (int i = 0; i < k; i++) {
        Position p = get((head + i) % size());
        if (prev != null) {
          area -= p.hiddenBy(prev) + prev.hiddenBy(p);
        }
        prev = p;
      }
      return area;
    }

    @Override
    public String toString() { // デバッグ用
      return "<" + (isCycle() ? "cycle" : "path")
           + "(" + size() + "):"
           + super.toString() + ">";
    }
  }

  /** 場所を追加する */
  void addPosition(int x, int y, int z) {
    Position p = new Position(x, y, z);
    // 既知の場所と隣接しているか
    for (Position q : pos) {
      if (p.adjacent(q)) {
        p.addAdj(q);
        q.addAdj(p);
      }
    }
    pos.add(p);
  }

  /**
   * 場所を表すクラス
   */
  class Position {
    Position(int x, int y, int z) {
      this.x = x;
      this.y = y;
      this.z = z;
    }
    int x, y, z;
    Position n1, n2; // 隣接する場所
    boolean joined;  // いずれかの連結成分に所属済み

    /** 次数 */
    int degree() {
      return (n1 == null && n2 == null) ? 0 :
             (n1 == null || n2 == null) ? 1 : 2;
    }

    /** 場所thatと隣接しているか */
    boolean adjacent(Position that) {
      return Math.abs(this.x - that.x) < s
          && Math.abs(this.y - that.y) < s
          && Math.abs(this.z - that.z) < s;
    }

    /** 隣接場所を登録 */
    void addAdj(Position p) {
      if (n1 == null) n1 = p;
      else            n2 = p;
    }

    /**
     * 場所pの立方体に隠される, 場所thisの立方体の表面積
     */
    int hiddenBy(Position p) {
      if (p == null) return 0;
      int dx = Math.abs(this.x - p.x); // dx, dy, dzはs未満
      int dy = Math.abs(this.y - p.y);
      int dz = Math.abs(this.z - p.z);
      // 平行な2面のうちちょうど1面が隠される
      return (s - dx) * (s - dy)
           + (s - dx) * (s - dz)
           + (s - dy) * (s - dz);
    }

    @Override
    public String toString() { // デバッグ用
      return "(" + x + "," + y + "," + z + ")";
    }
  }

}
