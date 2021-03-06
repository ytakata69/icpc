import java.util.*;

/**
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * Problem E: Boolean Expression Compressor
 */
class Main {
  public static void main(String[] arg) {
    Scanner scanner = new Scanner(System.in);
    while (true) {
      String e = scanner.next();
      if (e.equals(".")) { break; }
      Expression exp = new Parser(e).parse();
      Expression can = Expression.canonicalForm(exp);
      System.out.println(can.length());
    }
  }
}

/**
 * 構文解析器
 */
class Parser {
  Parser(String string) {
    this.string = string;
  }
  String string; // 解析対象
  int pos = 0;   // 解析位置

  /**
   * stringのpos番目の文字から始まる部分文字列を解析し，構文木を返す。
   * posを解析済み文字列の次の位置まで進める。
   */
  Expression parse() {
    char c = string.charAt(pos);
    switch (c) {
    case '0': case '1':
      pos++;
      return new ConstantExp(c - '0');  // 0または1
    case 'a': case 'b':
    case 'c': case 'd':
      pos++;
      return new VariableExp(c);
    case '-':
      pos++;
      return new NegativeExp(parse());
    case '(':
      pos++;
      Expression left  = parse();
      char op = string.charAt(pos++);
      Expression right = parse();
      pos++; // skip a ')'
      return new BinaryExp(op, left, right);
    default:
      throw new RuntimeException("" + c);
    }
  }
}

/**
 * 式の抽象構文木を表すクラス。
 * 式の真偽値表を表す整数を code として持つ
 * (式が等価 ⇔ 真偽値表が同じ)。
 */
abstract class Expression {
  /**
   * この式の真偽値表を表す16ビットの数。
   * 第iビットが入力i (4ビット, 最下位がa, 最上位がd) に対する
   * この式の値を表す。
   */
  protected int code;
  public int code() { return code; }

  /** 文字列長を返す */
  abstract public int length();

  /**
   * 式eと等価な最簡の式を返す
   */
  public static Expression canonicalForm(Expression e) {
    return allSet.canonicalForm(e);
  }

  /**
   * 考慮対象であるすべての式 (の最簡形) からなる集合
   */
  static ExpSet allSet;
  static {
    allSet = new ExpSet();  // 等価な式は文字列長の短いもののみ格納する集合

    // 正負リテラル
    final char[] vars = { 'a', 'b', 'c', 'd' };
    for (char v : vars) {
      Expression e = new VariableExp(v);
      allSet.add(e);
      allSet.add(new NegativeExp(e)); // -a, -b, -c, -d
    }

    // 二項演算子の入れ子が3段以下の式 (それより長い式は考えなくてよい)
    for (int i = 0; i < 3; i++) {
      Set<Expression> set = new ExpSet();  // 一時バッファ
      for (Expression e1 : allSet) {
        for (Expression e2 : allSet) {
          // * も ^ も可換なので e1 < e2 の場合のみ考える
          // ((e1,e2) と (e2,e1) のちょうど一方のみ選ばれるようにする)。
          // x * x == x, x ^ x == 0 なので e1 == e2 の場合は不要。
          if (e1.code() >= e2.code()) { continue; }
          set.add(new BinaryExp('*', e1, e2));
          set.add(new BinaryExp('^', e1, e2));
        }
      }
      allSet.addAll(set);
      for (Expression e : set) { allSet.add(new NegativeExp(e)); } // -e
    }

    // 定数 (途中に定数を含む式は考えなくてよい)
    allSet.add(new ConstantExp(0));
    allSet.add(new ConstantExp(1));
  }
}

/**
 * 最簡の式を保持する集合
 */
class ExpSet extends AbstractSet<Expression> {
  private Map<Integer, Expression> map = new HashMap<>();

  /**
   * 式eと等価な最簡の式を返す
   */
  public Expression canonicalForm(Expression e) {
    return map.get(e.code());
  }

  /**
   * 式eをこの集合に追加する。
   * すでに等価でより短い式を保持している場合は追加しない。
   * @return 追加したかどうか
   */
  @Override
  public boolean add(Expression e) {
    Expression e0 = map.get(e.code());
    if (e0 == null || e0.length() > e.length()) {
      map.put(e.code(), e);
      return true;
    }
    return false;
  }

  @Override
  public int size() { return map.size(); }
  @Override
  public Iterator<Expression> iterator() { return map.values().iterator(); }
}

/**
 * 定数 0, 1
 */
class ConstantExp extends Expression {
  int val;
  ConstantExp(int val) {
    this.val  = val;
    this.code = val * 0xffff;  // 0 or 0xffff
  }
  @Override
  public int length() { return 1; }
}

/**
 * 変数 a, b, c, d
 */
class VariableExp extends Expression {
  char var;
  VariableExp(char var) {
    this.var  = var;
    this.code = 0;
    for (int i = 0; i < 16; i++) {          // すべての入力
      if ((i & (1 << (var - 'a'))) != 0) {  // 当該変数が1
        this.code |= (1 << i);
      }
    }
  }
  @Override
  public int length() { return 1; }
}

/**
 * 否定演算子 -
 */
class NegativeExp extends Expression {
  Expression exp;
  NegativeExp(Expression exp) {
    this.exp  = exp;
    this.code = exp.code ^ 0xffff;  // 真偽反転
  }
  @Override
  public int length() { return 1 + exp.length(); }
}

/**
 * 二項演算子 *, ^
 */
class BinaryExp extends Expression {
  char op;
  Expression left, right;
  BinaryExp(char op, Expression left, Expression right) {
    this.op    = op;
    this.left  = left;
    this.right = right;
    if (op == '*') {
      this.code = left.code & right.code;
    } else {
      this.code = left.code ^ right.code;
    }
  }
  @Override
  public int length() { return 3 + left.length() + right.length(); }
}
