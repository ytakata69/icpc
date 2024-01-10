#!/usr/bin/env elixir

# ACM-ICPC 2022 Domestic Qualifier
# Problem B - Leave No One Behind

defmodule Main do
  def main do
    n = IO.gets("") |> String.trim |> String.to_integer
    if n != 0 do
      hands = for _ <- 1..n do
                IO.gets("") |> String.split |> Enum.map(&String.to_integer/1)
              end
      solve(n, hands) |> IO.inspect
      main()
    end
  end

  def solve(_n, hands) do
    # 同じカードを2枚持つプレイヤーを削除
    hands = hands |> Enum.filter(fn [c1, c2] -> c1 != c2 end)
    # playを繰り返し, 全員の手札がなくなるまでの回数を返す
    hands |> Stream.iterate(&play/1) |> Enum.take_while(&(&1 != []))
          |> length
  end

  def play([]), do: []
  def play([h1, h2 | rest]) do
    # 先頭プレイヤーの手札を最小カードc1とそれ以外に分ける
    [c1 | h1] = Enum.sort(h1)
    # c1を引いた後の次プレイヤーの手札
    h2 = if c1 in h2, do: Enum.filter(h2, &(&1 != c1)), else: [c1 | h2]
    # 先頭プレイヤーを末尾に移動. 手札が空のプレイヤーを削除
    [h2 | rest] ++ [h1] |> Enum.filter(&(&1 != []))
  end
end

Main.main
