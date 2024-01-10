#!/usr/bin/env elixir

# ACM-ICPC 2021 Domestic Qualifier
# Problem D - Handing out Baloons

defmodule Main do
  def main do
    n = IO.gets("") |> String.trim |> String.to_integer
    if n != 0 do
      bals = IO.gets("") |> String.split |> Enum.map(&String.to_integer/1)
      solve(bals) |> IO.inspect
      main()
    end
  end

  def solve(bals) do
    # 風船の総数とその3分の1
    total = Enum.sum(bals)
    onethird = div(total, 3)

    # dp = 作ることができる2個の部分和の集合
    dp = MapSet.new([{0, 0}])

    # 各風船に渡って更新 (balsを整列すると少し速い)
    dp = Enum.reduce(Enum.sort(bals), dp, fn b, dp ->
           MapSet.union(dp, MapSet.new(
             (for {b1, b2} <- dp, b1 < onethird, do: norm({b1 + b, b2})) ++
             (for {b1, b2} <- dp, b2 < onethird, do: {b1, b2 + b})))
         end)

    # 3個の部分和の最小値 == 配れる人数
    Enum.max(for {b1, b2} <- dp, do: min(min(b1, b2), total - (b1 + b2)))
  end

  # 左 <= 右 である対
  def norm({a, b}) when a > b, do: {b, a}
  def norm({a, b}), do: {a, b}
end

Main.main
