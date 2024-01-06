#!/usr/bin/env elixir

# ACM-ICPC 2021 Domestic Qualifier
# Problem B - Hundred-Cell Calculation Puzzles

defmodule Main do
  def split_into_list(str) do
    String.split(str, ~r{\s}, trim: true) |> Enum.map(&String.to_integer/1)
  end

  def main do
    [w, h] = IO.gets("") |> split_into_list
    if [w, h] != [0, 0] do
      k = w + h - 1
      xyn = for _ <- 1..k, do: IO.gets("") |> split_into_list
      (if solve(w, h, xyn), do: "YES", else: "NO") |> IO.puts
      main()
    end
  end

  def solve(w, h, xyn) do
    # 上端の数を頂点1, 2, ..., w, 左端の数を頂点-1, -2, ..., -hとし,
    # 解答マス(x, y)に数が入っているとき頂点x, -y間に辺があるグラフを
    # 考える. このグラフが連結であるとき答は真.
    xy  = for [x, y, _] <- xyn, do: {x, -y}  # edges between x and -y
    adj = xy ++ Enum.map(xy, fn {a, b} -> {b, a} end)  # bidirectional
    # 頂点1から全頂点に到達可能か?
    w + h == MapSet.size(dfs(MapSet.new(), adj, 1))
  end

  # startから深さ優先探索
  def dfs(visited, adj, start) do
    neighbors = for {a, b} <- adj, a == start, do: b
    neighbors |> Enum.filter(&(! MapSet.member?(visited, &1)))
      |> Enum.reduce(MapSet.put(visited, start), &(dfs(&2, adj, &1)))
  end
end

Main.main
