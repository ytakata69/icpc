#!/usr/bin/env elixir

# ACM ICPC 2023 domestic qualifier
# D - Efficient Problem Set

defmodule Main do
  def main do
    get_inputs()
    |> Parallel.pmap(fn {n, s} -> solve(n, s) end)
    |> Enum.map(&IO.inspect/1)
  end

  def get_inputs(acc \\ []) do
    n = IO.gets("") |> String.trim |> String.to_integer
    if n == 0 do
      Enum.reverse(acc)
    else
      s = IO.gets("") |> String.trim
      get_inputs([{n, s} | acc])
    end
  end


  def solve(n, s) do
    # setに変換
    s = MapSet.new(s |> String.to_charlist |> Enum.with_index
          |> Enum.flat_map(fn {c, i} -> if c == ?o, do: [i], else: [] end))
    s = Enum.into(s, s, &(n - &1))  # 対称化
    mn = Enum.sort(s) |> Enum.fetch!(1)  # 0の次に小さいsの要素

    max_n_elem = Util.ceil_log2(n + 1)  # 0..nの全部分和を構成可能
    min_n_elem = Util.ceil_log2(MapSet.size(s))  # sの要素を構成する最低限

    # 小さい要素数から順に試す (max_n_elem個あれば必ずカバー可能)
    min_n_elem..(max_n_elem - 1)//1
      |> Enum.drop_while(fn n_elem ->
           # 和がnであるn_elem個の要素の列がsをカバーしなければdrop
           not(Enum.any?(permut(n, n_elem, 1, [], mn),
                         &(MapSet.subset?(s, subset_sums(&1)))))
         end)
      |> Enum.take(1)
      |> (fn ans -> if ans == [], do: max_n_elem, else: hd(ans) end).()
  end

  # enumの部分和の集合
  def subset_sums(enum) do
    bag = MapSet.new([0])
    Enum.reduce(enum, bag, fn x, acc -> Enum.into(acc, acc, &(&1 + x)) end)
  end

  # 和がnであるn_elem個の要素の列 (最小要素はmin_elem以上max_min以下)
  def permut(n, n_elem, min_elem, acc, max_min \\ nil) do
    if n_elem <= 1 do
      [Enum.reverse([n | acc])]
    else
      # 最小要素の上限は div(n, n_elem)
      max_min = if max_min == nil, do: div(n, n_elem),
                    else: min(max_min, div(n, n_elem))
      for i <- max_min..min_elem//-1 do
          permut(n - i, n_elem - 1, i, [i | acc])
      end
        |> Enum.concat
    end
  end
end

defmodule Util do
  # 2 ** (i - 1) < n <= 2 ** i である i を返す.
  def ceil_log2(n), do: ceil_log2(n, false)

  def ceil_log2(n, false) when n <= 1, do: 0
  def ceil_log2(n, true)  when n <= 1, do: 1
  def ceil_log2(n, carry) do
    ceil_log2(div(n, 2), carry or rem(n, 2) > 0) + 1
  end
end

# Taken from: http://elixir-recipes.github.io/concurrency/parallel-map/
defmodule Parallel do
  def pmap(collection, func) do
    collection
    |> Enum.map(&(Task.async(fn -> func.(&1) end)))
    |> Enum.map(&(Task.await(&1, :infinity)))
  end
end

Main.main
