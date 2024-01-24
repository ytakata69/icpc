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
    dp = Map.new([{0, MapSet.new([0])}])

    # 各風船に渡って更新 (balsを整列すると少し速い)
    dp = Enum.reduce(Enum.sort(bals), dp, fn b, dp ->
           n1 = Map.new(for {b1, bs} <- dp, b1 < onethird, do: {b1 + b, bs})
           n2 = Map.new(for {b1, bs} <- dp, do:
                  {b1, MapSet.new(for b2 <- bs, b2 < onethird, b2 <= b1,
                                  do: b2 + b)})
           Map.merge(n1, n2, fn _k, v1, v2 -> MapSet.union(v1, v2) end)
           |> Map.merge(dp,  fn _k, v1, v2 -> MapSet.union(v1, v2) end)
         end)

    # 3個の部分和の最小値 == 配れる人数
    Enum.max(for {b1, bs} <- dp, b2 <- bs,
               do: min(min(b1, b2), total - (b1 + b2)))
  end
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
