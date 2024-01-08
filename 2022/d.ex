#!/usr/bin/env elixir

# ACM-ICPC 2022 Domestic Qualifier
# Problem D - Audience Queue

defmodule Main do
  def split_into_list(str) do
    str |> String.split(~r{\s}, trim: true) |> Enum.map(&String.to_integer/1)
  end

  def main() do
    [n, k] = IO.gets("") |> split_into_list
    if n != 0 do
      [ss, ts] = for _ <- 1..2, do: IO.gets("") |> split_into_list
      solve(n, k, ss, ts) |> IO.inspect()
      main()
    end
  end

  # k個のゲートを使って入場待ち列ssを入場列tsに並べ替える方法の個数を計算.
  # ss: 入場待ち順の座席番号のリスト, ts: 入場順の座席番号のリスト
  def solve(n, k, ss, ts) do
    # 座席番号 → 入場順 であるMap
    entord = Enum.zip(ts, 0..n-1) |> Map.new

    # 入場待ち列上 必ず切断すべき位置 (前の人より先に入場) を表すboolean列
    es = Enum.map(ss, &(Map.get(entord, &1)))  # 各人の入場順
    mustcut = Enum.zip_with(es, tl(es), &(&1 > &2))

    # 入場列上 切断できない位置 (大きい座席番号より後に入場) を表すboolean列
    premax = Enum.scan(ts, &max/2)  # その位置までの最大座席番号
    notcut = Enum.zip_with(premax, ts, &(&1 > &2))

    # 切断可能位置のうち残りゲート数個を選ぶ選び方が答
    nmustcut = mustcut |> Enum.count(&(&1))  # trueの個数
    nnotcut  = notcut  |> Enum.count(&(&1))
    m = n - 1 - nmustcut - nnotcut  # 切断可能位置の数
    l = k - 1 - nmustcut            # 残りゲート数

    # mustcutで分割したとき大きい座席番号が別のゲートにいるなら答は0
    gs = [0] ++ Enum.scan(mustcut, 0, &(if &1, do: &2 + 1, else: &2))
    gate = Enum.zip(ss, gs) |> Map.new  # 座席番号 → ゲート番号
    conflict = Enum.zip_with(premax, ts, &(&1 > &2 && gate[&1] != gate[&2]))
               |> Enum.any?

    if l < 0 || conflict, do: 0, else: Modulo.sum_comb(m, l)
  end
end

defmodule Modulo do
  @modulus 998244353

  # sum_comb(m, l) = sum_{i=0}^{l} comb(m, i)
  def sum_comb(_, 0), do: 1
  def sum_comb(m, l) do
    Enum.scan(1..min(m, l), 1, &(mdiv(&2 * (m - &1 + 1), &1)))
      |> Enum.reduce(&madd/2)
      |> madd(1)  # comb(m, 0) (= 1) を加算
  end

  def mpow(_, 0), do: 1
  def mpow(x, p) do
    k = if rem(p, 2) == 0, do: 1, else: x
    rem(k * mpow(rem(x * x, @modulus), div(p, 2)), @modulus)
  end

  def madd(a, b), do: rem(a + b, @modulus)
  def mdiv(n, d), do: rem(n * mpow(d, @modulus - 2), @modulus)
end

Main.main
