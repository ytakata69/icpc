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

  # k個のゲートを使ってssを入場順tsに並べ替える方法の個数
  def solve(n, k, ss, ts) do
    # 座席番号の列ssを入場順の列に変換
    ent = Enum.zip(ts, 0..n-1) |> Map.new
    es = ss |> Enum.map(&(Map.get(ent, &1)))

    # 必ず切断すべき位置 (前の人より先に入場) を表すbooleanリスト
    mustcut = Enum.zip_with([hd(es) | es], es, &(&1 > &2))

    # mustcutで分割したときの各座席番号のグループ番号
    gs = Enum.scan(mustcut, 0, &(if &1, do: &2 + 1, else: &2))
    group = Enum.zip(ss, gs) |> Map.new

    # 切断できない位置 (大きい座席番号より後に入場) を表すbooleanリスト
    premax = Enum.scan(ts, &max/2)  # その位置までの最大座席番号
    notcut = premax |> Enum.zip_with(ts, &(&1 > &2))

    nmustcut = mustcut |> Enum.count(&(&1))  # trueの個数
    nnotcut  = notcut  |> Enum.count(&(&1))
    m = n - 1 - nmustcut - nnotcut  # 切断候補数
    l = k - 1 - nmustcut            # 残りゲート数

    # 大きい座席番号が別のグループにいる場合, 実現できない
    conflict = Enum.zip_with(premax, ts,
                             &(Map.get(group, &1) != Map.get(group, &2)))
               |> Enum.zip_with(notcut, &(&1 && &2))
               |> Enum.reduce(&(&1 || &2))

    if l < 0 || conflict, do: 0, else: Modulo.sum_comb(m, l)
  end
end

defmodule Modulo do
  @modulus 998244353

  # sum_comb(m, l) = sum_{i=0}^{l} comb(m, i)
  def sum_comb(_, 0), do: 1
  def sum_comb(m, l) do
    Enum.scan(1..min(m, l), 1, &(mdiv(&2 * (m - &1 + 1), &1)))
      |> Enum.reduce(&mplus/2)
      |> mplus(1)  # comb(m, 0)を加算
  end

  def mplus(a, b), do: rem(a + b, @modulus)

  def mpow(_, 0), do: 1
  def mpow(x, 1), do: rem(x, @modulus)
  def mpow(x, p) do
    k = if rem(p, 2) == 0, do: 1, else: x
    rem(k * mpow(rem(x * x, @modulus), div(p, 2)), @modulus)
  end

  def mdiv(n, d), do: rem(n * mpow(d, @modulus - 2), @modulus)
end

Main.main
