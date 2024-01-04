#!/usr/bin/env elixir

# ACM ICPC 2023 domestic qualifier
# A - Which Team Should Receive the Sponsor Prize?

defmodule Main do
  def main do
    n = IO.gets("") |> String.trim() |> String.to_integer()
    if n != 0 do
      a = IO.gets("") |> String.split(~r{\s}, trim: true)
          |> Enum.map(&String.to_integer/1)
      solve(n, a) |> IO.inspect()
      main()
    end
  end
  
  def solve(n, a) do
    a |> Enum.map(&(abs(&1 - 2023))) |> Enum.zip(1..n)
      |> Enum.min() |> elem(1)
  end
end

# Remove the following line when you run this code on AtCoder
Main.main
