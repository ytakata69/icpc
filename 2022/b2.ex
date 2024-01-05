#!/usr/bin/env elixir

# ACM-ICPC 2022 Domestic Qualifier
# Problem B - Leave No One Behind

defmodule Main do
  def main() do
    n = IO.gets("") |> String.trim() |> String.to_integer()
    if n != 0 do
      hands = for _ <- 1..n do
                IO.gets("") |> String.split(~r{\s}, trim: true)
                  |> Enum.map(&String.to_integer/1)
              end
      solve(n, hands) |> IO.inspect()
      main()
    end
  end

  def solve(_n, hands) do
    # 同じカードを2枚持つプレイヤーを削除
    hands = hands |> Enum.filter(fn [c1, c2] -> c1 != c2 end)
    # playを繰り返し, 全員の手札がなくなるまでの回数を返す
    {hands, []} |> Stream.iterate(&play/1)
                |> Enum.take_while(fn {l1, l2} -> l1 != [] || l2 != [] end)
                |> length()
  end

  def play({[],   rest}), do: play({Enum.reverse(rest), []})
  def play({[h1], rest}), do: play({[h1 | Enum.reverse(rest)], []})
  def play({[h1, h2 | rest1], rest2}) do
    # 先頭プレイヤーの手札を最小カードc1とそれ以外に分ける
    [c1 | h1] = Enum.sort(h1)
    # c1を引いた後の次プレイヤーの手札
    h2 = if c1 in h2, do: Enum.filter(h2, &(&1 != c1)), else: [c1 | h2]
    # 先頭プレイヤーを末尾に移動. 手札が空のプレイヤーを削除
    { (if h2 == [], do: rest1, else: [h2 | rest1]),
      (if h1 == [], do: rest2, else: [h1 | rest2]) }
  end
end

Main.main()
