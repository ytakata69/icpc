#!/usr/bin/env elixir

# ACM-ICPC 2021 Domestic Qualifier
# Problem A - Marbles Tell Your Lucky Number

defmodule Main do
  def main do
    cups = IO.gets("") |> String.split |> Enum.map(&String.to_integer/1)
    if cups != [0, 0, 0, 0] do
        solve(cups) |> IO.inspect
        main()
    end
  end

  def solve(cups) do
     cups |> Enum.filter(&(&1 > 0)) |> lucky
  end

  def lucky([m]), do: m
  def lucky(cs) do
    [m | cs] = Enum.sort(cs)
    lucky([m | (for m2 <- cs, m2 > m, do: m2 - m)])
  end
end

Main.main
