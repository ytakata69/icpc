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
      int n = (c.isCycle() && c.size() > k) ? c.size() // 閉路状の多面体
            : c.size() - k + 1; // 両端のある多面体
      for (int h = 0; h < n; h++) {
        // 連結成分cのh番要素からk個の立方体を選んで作った多面体の表面積
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
    if (! p.prev.joined) { // next→pの順に訪問した
      Position tmp = p.prev;
      p.prev = p.next;
      p.next = tmp;
    }
    traverse(c, p.next);
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

    /** head番以降のk個の立方体からなる多面体の表面積 */
    int area(int head, int k) {
      boolean isCycle = (isCycle() && size() == k); // 閉路状の多面体
      int a = 0; // 表面積
      for (int i = 0; i < k; i++) {
        Position p = get((head + i) % size());
        // pの表面積からp.prev, p.nextに隠される表面積を引く
        a += 6 * s * s
           - (isCycle || i > 0     ? p.hiddenBy(p.prev) : 0)
           - (isCycle || i < k - 1 ? p.hiddenBy(p.next) : 0);
      }
      return a;
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
    Position prev, next;
    boolean joined; // いずれかの連結成分に所属済み

    /** 次数 */
    int degree() {
      return (prev == null && next == null) ? 0 :
             (prev == null || next == null) ? 1 : 2;
    }

    /** 場所thatと隣接しているか */
    boolean adjacent(Position that) {
      return Math.abs(this.x - that.x) < s
          && Math.abs(this.y - that.y) < s
          && Math.abs(this.z - that.z) < s;
    }

    /**
     * 隣接場所を登録.
     * とりあえずprevから先に登録.
     * あとで連結成分に分ける際, 連結成分の代表に近い側をprev,
     * 他方をnextに変える.
     */
    void addAdj(Position p) {
      if (prev == null) prev = p;
      else              next = p;
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
