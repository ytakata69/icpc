#!/usr/bin/env elixir

# ACM ICPC 2023 domestic qualifier
# B - Amidakuji

defmodule Main do
  def main do
    [n, m, p, q] = IO.gets("") |> String.split(~r{\s}, trim: true)
                   |> Enum.map(&String.to_integer/1)
    if n != 0 do
      x = IO.gets("") |> String.split(~r{\s}, trim: true)
          |> Enum.map(&String.to_integer/1)
      case solve(n, m, p, q, x) do
        {x, y} -> IO.puts("#{x} #{y}")
        str    -> IO.puts(str)
      end
      main()
    end
  end

  def solve(_n, _m, p, q, x) do
    # Pから下に進んだときの位置の軌跡 (の逆順)
    tp = trace(p, x)

    if hd(tp) == q do
      "OK"  # Qに到達
    else
      # Qから上に進んだときの位置の軌跡
      tq = trace(q, Enum.reverse(x))

      # Pからの軌跡とQからの軌跡が距離1に初めてなる位置を探す
      case Enum.zip(Enum.reverse(tp), tq)
           |> Enum.split_while(fn {a, b} -> abs(a - b) > 1 end) do
        {_, []} -> "NG"
        {l, [{h1, h2}|_]} -> {min(h1, h2), length(l)}
      end
    end
  end

  # 横線のリストxによるpの軌跡 (の逆順)
  def trace(p, x) do
    Enum.reduce(x, [p], &([cross(hd(&2), &1) | &2]))
  end

  # 横線xによるpの移動
  def cross(p, x) when p == x,     do: p + 1
  def cross(p, x) when p == x + 1, do: p - 1
  def cross(p, _x), do: p
end

# Remove the following line when you run this code on AtCoder
Main.main
