#!/usr/bin/env elixir

# ACM-ICPC 2021 Domestic Qualifier
# Problem C - Tree Transformation Puzzle

defmodule Main do
  def main do
    exp = IO.gets("") |> String.trim
    if exp != "-1" do
      solve(exp) |> IO.inspect
      main()
    end
  end

  def solve(exp) do
    ParseTree.parse(exp)    # 構文解析
      |> MinMaxTree.new     # 各部分木の最小値・最大値を計算
      |> MinMaxTree.chroot  # 各頂点を根にしたときの最大値の最大値
  end
end

defmodule Util do
  @doc """
  ```
  iex> rotations [1,2,3]
  [[1,2,3], [2,3,1], [3,1,2]]
  ```
  """
  def rotations(ls) do
    l = length(ls)
    ls |> Stream.cycle |> Stream.iterate(&(Stream.drop(&1, 1)))
       |> Stream.map(&(Enum.take(&1, l))) |> Enum.take(l)
  end
end

# ----------------------------------------
# 構文解析木
defmodule ParseTree do
  defstruct opr: nil, child: []

  # root式
  def parse(exp) do
    {first,  rest} = parseNonRoot(exp)
    {opr,    rest} = parseOpr(rest)
    {second, rest} = parseNonRoot(rest)
    {^opr,   rest} = parseOpr(rest)
    {third,  <<>>} = parseNonRoot(rest)
    %ParseTree{ opr: opr, child: [first, second, third] }
  end

  # non-root式 (先頭が開き括弧)
  defp parseNonRoot(<< ?( :: utf8, rest :: binary >>) do
    {first,  rest} = parseNonRoot(rest)
    {opr,    rest} = parseOpr(rest)
    {second, rest} = parseNonRoot(rest)
    << ?) :: utf8, rest :: binary >> = rest  # 閉じ括弧
    { %ParseTree{ opr: opr, child: [first, second] }, rest }
  end

  # 数字
  defp parseNonRoot(<< digit :: utf8, rest :: binary >>)
         when digit in ?0..?9 do
    { digit - ?0, rest }
  end

  # 演算子
  defp parseOpr(<< opr :: utf8, rest :: binary >>) when opr in [?+, ?-] do
    { (if opr == ?+, do: :plus, else: :minus), rest }
  end
end

# ----------------------------------------
# 最小値・最大値の付いた木
defmodule MinMaxTree do
  defstruct opr: nil, child: [], min: nil, max: nil

  @minBound (-(2 ** 31))  # -♾️

  # ParseTreeからMinMaxTreeに変換
  def new(tree = %ParseTree{}) do
    child = Enum.map(tree.child, &new/1)
    {mn, mx} = minMax(tree.opr, child)
    %MinMaxTree{ opr: tree.opr, child: child, min: mn, max: mx }
  end
  def new(digit), do: %MinMaxTree{ min: digit, max: digit }

  # 演算子opr, 子のリストchildからなる木の最小値・最大値
  defp minMax(opr, child) do
    mnsum = child |> Enum.map(&(&1.min)) |> Enum.sum  # 最小値の総和
    mxsum = child |> Enum.map(&(&1.max)) |> Enum.sum  # 最大値の総和
    mnmxs = child |> Enum.map(&({&1.min, &1.max}))
    if opr == :plus do
      { mnsum, mxsum }
    else  # :minus
      # mn = ((子の1個の最小値) - (それ以外の子の最大値の和)) の最小値
      # mx = ((子の1個の最大値) - (それ以外の子の最小値の和)) の最大値
      mn = mnmxs |> Enum.map(fn {mn, mx} -> mn - (mxsum - mx) end) |> Enum.min
      mx = mnmxs |> Enum.map(fn {mn, mx} -> mx - (mnsum - mn) end) |> Enum.max
      { mn, mx }
    end
  end

  # rootまたはその子孫を根にしたときの最大値.
  # {pmin, pmax}: rootの親を子にしたときの最小値・最大値
  defp chroot(_, %MinMaxTree{child: []}), do: @minBound
  defp chroot({pmin, pmax}, root) do
    parent = %MinMaxTree{min: pmin, max: pmax}  # 親を表す仮頂点
    # rootの子の1個を根にしたときの最大値
    Util.rotations(root.child)
      |> Enum.map(fn [c | cs] -> chroot(minMax(root.opr, [parent|cs]), c) end)
      |> Enum.max
         # rootを根に (rootの子にparentを追加) した場合と比較
      |> max(minMax(root.opr, [parent|root.child]) |> elem(1))
  end
  def chroot(root) do  # rootが元々の根 (子が3個)
    Util.rotations(root.child)
      |> Enum.map(fn [c | cs] -> chroot(minMax(root.opr, cs), c) end)
      |> Enum.max |> max(root.max)
  end
end

# ----------------------------------------
Main.main
