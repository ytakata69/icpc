#!/usr/bin/env elixir

# ACM-ICPC 2022 Domestic Qualifier
# Problem A - Counting Peaks of Infection

defmodule Main do
  def main() do
    n = IO.gets("") |> String.trim() |> String.to_integer()
    if n != 0 do
      v = IO.gets("") |> String.split(~r{\s}, trim: true)
            |> Enum.map(&String.to_integer/1)
      solve(n, v) |> IO.inspect()
      main()
    end
  end

  def solve(_n, v) do
    Enum.zip([v, tl(v), tl(tl(v))])
      |> Enum.filter(fn {a, b, c} -> a < b && b > c end)
      |> length()
  end
end

Main.main()
