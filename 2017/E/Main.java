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
      Expression sim = Expression.allSet.floor(exp); // 最も近い要素
      if (!sim.equals(exp)) { throw new RuntimeException("" + sim); }
      System.out.println(sim.toString().length());
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
      return new NegationExp(parse());
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
abstract class Expression implements Comparable<Expression> {
  /**
   * この式の真偽値表を表す16ビットの数。
   * 第iビットが入力i (4ビット, 最下位がa, 最上位がd) に対する
   * この式の値を表す。
   */
  int code;

  @Override
  public int hashCode() { return code; }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof Expression)) { return false; }
    return compareTo((Expression)o) == 0;
  }

  @Override
  public int compareTo(Expression e) {
    return this.hashCode() - e.hashCode();
  }

  /** 文字列長を返す */
  public int length() { return toString().length(); }

  /**
   * 考慮対象であるすべての式からなる集合
   * (「指定の要素以下で最大の要素」を返す関数 floor() を持つ)
   */
  static NavigableSet<Expression> allSet;
  static {
    allSet = new ExpSet();

    // 定数
    allSet.add(new ConstantExp(0));
    allSet.add(new ConstantExp(1));

    // リテラル
    final char[] vars = { 'a', 'b', 'c', 'd' };
    for (char v : vars) {
      Expression e = new VariableExp(v);
      allSet.add(e);
      allSet.add(new NegationExp(e));
    }

    // 二項演算子の入れ子が3段以下の式 (それより長い式は考えなくてよい)
    for (int i = 0; i < 3; i++) {
      Set<Expression> set = new ExpSet();  // 一時バッファ
      for (Expression e1 : allSet) {
        for (Expression e2 : allSet) {
          // * も ^ も可換なので e1 > e2 の場合は不要。
          // * はべき等。x ^ x == 0 なので e1 == e2 の場合も不要。
          if (e1.compareTo(e2) >= 0) { continue; }
          set.add(new BinaryExp('*', e1, e2));
          set.add(new BinaryExp('^', e1, e2));
        }
      }
      allSet.addAll(set);
      for (Expression e : set) { allSet.add(new NegationExp(e)); }
    }
  }

  /**
   * TreeSet<Expression> とほとんど同じだが，等価な式を add
   * しようとしたとき，文字列長が短ければ更新する。
   */
  static class ExpSet extends TreeSet<Expression> {
    @Override
    public boolean add(Expression e) {
      if (this.contains(e)) {
        Expression f = this.floor(e);
        if (f.length() > e.length()) {
          this.remove(f);
        }
      }
      return super.add(e);
    }
  }
}

/** 定数 0 または 1 */
class ConstantExp extends Expression {
  int val;
  ConstantExp(int val) {
    this.val  = val;
    this.code = val * 65535;  // 全部0 or 全部1
  }
  @Override
  public int length() { return 1; }
  @Override
  public String toString() { return Integer.toString(val); }
}

/** 変数 a, b, c, d */
class VariableExp extends Expression {
  char var;
  VariableExp(char var) {
    this.var  = var;
    this.code = 0;
    for (int i = 0; i < 16; i++) {
      if ((i & (1 << (var - 'a'))) != 0) {
        this.code |= (1 << i);
      }
    }
  }
  @Override
  public int length() { return 1; }
  @Override
  public String toString() { return Character.toString(var); }
}

/** 否定演算子 */
class NegationExp extends Expression {
  Expression exp;
  NegationExp(Expression exp) {
    this.exp  = exp;
    this.code = exp.code ^ 65535;  // 真偽反転
  }
  @Override
  public int length() { return 1 + exp.length(); }
  @Override
  public String toString() { return "-" + exp; }
}

/** 二項演算子 *, ^ */
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
  @Override
  public String toString() {
    return "(" + left.toString() + op + right.toString() + ")";
  }
}
