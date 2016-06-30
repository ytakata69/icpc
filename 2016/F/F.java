import java.util.*;

class F {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      Image g1 = getImage(scanner);
      if (g1 == null) break;
      Image g2 = getImage(scanner);
      System.out.println(g1.equals(g2) ? "yes" : "no");
    }
  }

  /**
   * 画像データの読み出し
   */
  static Image getImage(Scanner scanner) {
    int h = scanner.nextInt();
    int w = scanner.nextInt();
    if (h == 0 && w == 0) return null;
    Image g = new Image(h, w);
    for (int i = 0; i < h; i++) {
      String p = scanner.next();
      for (int j = 0; j < w; j++) {
        g.setPixel(i, j, p.charAt(j) == '#');
      }
    }
    return g;
  }
}


/**
 * 画像
 */
class Image {
  Image(int h, int w) {
    this.h = h;
    this.w = w;
    this.pixel = new Pixel[h][w];
  }
  int h, w;
  Pixel[][] pixel;
  int nComponent; // 連結成分数

  /** 画素をセット */
  void setPixel(int y, int x, boolean isBlack) {
    pixel[y][x] = new Pixel(isBlack);
  }

  /**
   * ピクセル
   */
  class Pixel {
    Pixel(boolean isBlack) {
      this.isBlack = isBlack;
    }
    boolean isBlack;
    int component;
  }

  /**
   * 2画像が同じ文字を表しているか
   */
  @Override
  public boolean equals(Object obj) {
    if (obj == null) return false;
    if (! (obj instanceof Image)) return false;
    Image that = (Image)obj;

    // 連結成分に分解
    if (this.nComponent == 0) this.findComponents();
    if (that.nComponent == 0) that.findComponents();

    // 連結成分数が異なる
    if (this.nComponent != that.nComponent) return false;

    // 包含関係を表す木を構築
    Tree t1 = this.surroundingRelation();
    Tree t2 = that.surroundingRelation();

    // 木が同型か
    return t1.equals(t2);
  }

  /**
   * 各画素が属する連結成分を決める
   */
  void findComponents() {
    // 背景連結成分を抽出. 連結成分番号は1
    for (int y = 0; y < h; y++) {
      if (! pixel[y][0]  .isBlack) findComponent(y, 0,   1);
      if (! pixel[y][w-1].isBlack) findComponent(y, w-1, 1);
    }
    for (int x = 0; x < w; x++) {
      if (! pixel[0]  [x].isBlack) findComponent(0,   x, 1);
      if (! pixel[h-1][x].isBlack) findComponent(h-1, x, 1);
    }

    // 背景連結成分以外の連結成分を抽出
    nComponent = 1; // 連結成分数 (背景連結成分が1)
    for (int y = 0; y < h; y++) {
      for (int x = 0; x < w; x++) {
        if (pixel[y][x].component == 0) { // まだ連結成分に所属していない
          findComponent(y, x, ++nComponent);
        }
      }
    }
  }

  final static int[] dx = { 1, 0, -1,  0,  1, 1, -1, -1 };
  final static int[] dy = { 0, 1,  0, -1, -1, 1,  1, -1 };

  /**
   * 同じ連結成分の画素を訪問し, 連結成分に加える.
   * @param c 連結成分番号
   */
  private void findComponent(int y, int x, int c) {
    Pixel p = pixel[y][x];
    if (p.component != 0) return; // いずれかの連結成分に所属済み
    p.component = c;

    // 各方向
    for (int i = 0; i < (p.isBlack ? 8 : 4); i++) {
      int x2 = x + dx[i];
      int y2 = y + dy[i];
      if (x2 < 0 || x2 >= w || y2 < 0 || y2 >= h) continue;
      if (p.isBlack == pixel[y2][x2].isBlack) { // 同じ色
        findComponent(y2, x2, c);
      }
    }
  }

  /**
   * 連結成分間の包含関係を表す木を返す
   */
  @SuppressWarnings("unchecked")
  Tree surroundingRelation() {
    // 各連結成分の, それを包含する連結成分の番号の集合.
    SortedSet<Integer>[] surr = new SortedSet[nComponent + 1];

    for (int y = 0; y < h; y++) {
      for (int x = 0; x < w; x++) {
        Pixel p = pixel[y][x];

        // c1がcを包含 <=> cの任意の画素と任意の端との間にc1の画素が存在
        // pから端までの途中に存在する連結成分を求め, 共通集合を取る
        for (int dir = 0; dir < 4; dir++) {
          SortedSet<Integer> set = new TreeSet<>();
          int x2 = x + dx[dir];
          int y2 = y + dy[dir];
          while (0 <= x2 && x2 < w && 0 <= y2 && y2 < h) {
            Pixel p2 = pixel[y2][x2];
            if (p.isBlack != p2.isBlack) { // 異なる色
              set.add(p2.component); // 端までの途中に通る連結成分
            }
            x2 += dx[dir];
            y2 += dy[dir];
          }
          if (surr[p.component] == null) {
            surr[p.component] = set;
          } else {
            surr[p.component].retainAll(set); // 共通集合
          }
        }
        // 黒連結成分は背景連結成分に包含される
        if (p.isBlack) {
          surr[p.component].add(1);
        }
      }
    }

    // 包含関係を表す木を構築
    Tree tree = new Tree();
    tree.addRoot(1);
    for (int c = 2; c <= nComponent; c++) {
      if (surr[c] != null && ! surr[c].isEmpty()) {
        int c1 = surr[c].last(); // cを包含する直近の連結成分
        tree.addEdge(c1, c);
      }
    }
    return tree;
  }
}

/**
 * 包含関係を表す木.
 */
class Tree {
  /** 2つの木が同型か */
  @Override
  public boolean equals(Object obj) {
    if (obj == null) return false;
    if (! (obj instanceof Tree)) return false;
    Tree that = (Tree)obj;

    // Node#equalsに帰着
    return this.root.equals(that.root);
  }

  Node root;
  Map<Integer, Node> nodeSet = new HashMap<>();

  void addRoot(int root) {
    this.root = addNode(root);
  }

  void addEdge(int parent, int child) {
    Node node = addNode(child);
    nodeSet.get(parent).addChild(node);
  }

  Node addNode(int id) {
    Node node = new Node(id);
    nodeSet.put(id, node);
    return node;
  }

  /** 節点を表すクラス. idはデバッグ出力のために保持している. */
  class Node {
    int id;
    List<Node> children = new LinkedList<>();

    Node(int id) {
      this.id = id;
    }
    void addChild(Node n) {
      children.add(n);
    }

    @Override
    public String toString() {
      return "" + id + ":" + children.toString();
    }

    /** 部分木が同型か */
    @Override
    public boolean equals(Object obj) {
      if (obj == null) return false;
      if (! (obj instanceof Node)) return false;
      Node that = (Node)obj;

      // 子を並び替えると等価 (第i子同士が等価) になるか
      return permutationEq(this.children, that.children);
    }
  }

  /**
   * 同じ型のリストmutatedとpeerに対し,
   * mutatedを並び替えるとpeerと等価 (第i要素同士が等価) になるとき,
   * かつそのときに限り, <code>true</code>を返す
   */
  static <E> boolean permutationEq(List<E> mutated, List<E> peer) {
    if (mutated.size() != peer.size()) return false;
    if (mutated.isEmpty()) return true;

    for (int i = 0; i < mutated.size(); i++) {
      E m = mutated.get(i);
      E p = peer   .get(0);
      if (m.equals(p)) {
        mutated.remove(i);
        peer   .remove(0);
        boolean success = permutationEq(mutated, peer);

        // 元に戻す
        peer   .add(0, p);
        mutated.add(i, m);
        if (success) return true;
      }
    }
    return false;
  }
}
