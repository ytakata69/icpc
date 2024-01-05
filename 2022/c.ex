#!/usr/bin/env elixir

# ACM-ICPC 2022 Domestic Qualifier
# Problem C - Training Schedule for ICPC

defmodule Main do
  def main() do
    [n, m] = IO.gets("") |> String.split(~r{\s}, trim: true)
                         |> Enum.map(&String.to_integer/1)
    if n != 0 || m != 0 do
      solve(n, m) |> IO.inspect()
      main()
    end
  end

  def solve(n, 0), do: n * n
  def solve(n, m) do
    1..min(m, n + 1) |> Enum.map(&(splitRepose(&1, n, m))) |> Enum.max()
  end

  # 休息をs個のセグメントに分けたときの利得
  def splitRepose(s, n, m) do
    # 休息は均等に分けるのが得
    {m1, r} = {div(m, s), rem(m, s)}
    lose = (m1 ** 2) * (s - r) + ((m1 + 1) ** 2) * r
    # 訓練は不均等に分けるのが得
    st = max(s - 1, 1)  # トレーニングのセグメント数
    gain = (n - (st - 1)) ** 2 + (st - 1)
    gain - lose
  end
end

Main.main()
