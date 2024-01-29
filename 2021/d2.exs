#!/usr/bin/env elixir

# ACM-ICPC 2021 Domestic Qualifier
# Problem D - Handing out Baloons

defmodule Main do
  def main do
    get_inputs() |> Parallel.pmap(&solve/1) |> Enum.map(&IO.inspect/1)
  end

  def get_inputs(acc \\ []) do
    n = IO.gets("") |> String.trim |> String.to_integer
    if n == 0 do
      Enum.reverse(acc)
    else
      bals = IO.gets("") |> String.split |> Enum.map(&String.to_integer/1)
      get_inputs([bals | acc])
    end
  end

  def solve(bals) do
    # 風船の総数とその3分の1
    total = Enum.sum(bals)
    onethird = div(total, 3)

    # dp = 作ることができる2個の部分和の集合
    dp = %{0 => BitSet.new([0])}

    # 各風船に渡って更新 (balsを整列すると少し速い)
    dp = Enum.reduce(Enum.sort(bals), dp, fn b, dp ->
           n1 = Map.new(for {b1, bs} <- dp, b1 < onethird, do: {b1 + b, bs})
           n2 = Map.new(for {b1, bs} <- dp, do:
                  {b1, BitSet.less_than(bs, min(onethird, b1 + 1))
                       |> BitSet.shift(b)  # map (+ b) to all elements
                       |> BitSet.union(bs)})
           Map.merge(n1, n2, fn _k, v1, v2 -> BitSet.union(v1, v2) end)
         end)

    # 3個の部分和の最小値 == 配れる人数
    Enum.max(for {b1, bs} <- dp, b2 <- bs,
               do: min(min(b1, b2), total - (b1 + b2)))
  end
end

defmodule BitSet do
  import Bitwise
  defstruct content: 0

  def new(collection) do
    %BitSet{content:
      Enum.reduce(collection, 0, fn v, acc -> acc ||| (1 <<< v) end)}
  end

  def union(%BitSet{content: set1}, %BitSet{content: set2}),
    do: %BitSet{content: set1 ||| set2}

  # { v | v in set, v < limit }
  def less_than(%BitSet{content: set}, limit),
    do: %BitSet{content: set &&& ((1 <<< limit) - 1)}

  # { v + delta | v in set }
  def shift(%BitSet{content: set}, delta),
    do: %BitSet{content: set <<< delta}

  def to_list(%BitSet{content: set}), do: _to_list(set, 0, [])

  defp _to_list(0, _, acc), do: acc
  defp _to_list(s, i, acc) do
    _to_list(s >>> 1, i + 1, (if (s &&& 1) != 0, do: [i | acc], else: acc))
  end
end

defimpl Enumerable, for: BitSet do
  import Bitwise

  def reduce(%BitSet{content: set}, state, fun) do
    _reduce({set, 0, 1}, state, fun)
  end

  def _reduce(_content, {:halt, acc}, _fun), do: {:halted, acc}
  def _reduce(content, {:suspend, acc}, fun),
    do: {:suspended, acc, &_reduce(content, &1, fun)}
  def _reduce({set, _i, mask}, {:cont, acc}, _fun) when set < mask,
    do: {:done, acc}
  def _reduce({set, i, mask}, state={:cont, acc}, fun) do
    nstate = if (set &&& mask) != 0, do: fun.(i, acc), else: state
    _reduce({set, i + 1, mask <<< 1}, nstate, fun)
  end

  def member?(%BitSet{content: set}, value) do
    (set &&& (1 <<< value)) != 0
  end
  def count(%BitSet{}), do: {:error, __MODULE__}
  def slice(%BitSet{}), do: {:error, __MODULE__}
end

# http://elixir-recipes.github.io/concurrency/parallel-map/
defmodule Parallel do
  def pmap(collection, func) do
    collection
    |> Enum.map(&(Task.async(fn -> func.(&1) end)))
    |> Enum.map(&(Task.await(&1, :infinity)))
  end
end

Main.main
