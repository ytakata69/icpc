# ACM ICPC 2023 domestic qualifier
# C - Changing the Sitting Arrangement

defmodule Main do
  def main do
    n = IO.gets("") |> String.trim() |> String.to_integer()
    if n != 0 do
      a = for _ <- 1..n do
            IO.gets("") |> String.split(~r{\s}, trim: true)
              |> Enum.map(&String.to_integer/1)
          end
      solve(n, a)
        |> Enum.map(&(Enum.join(&1, " ") |> IO.puts()))
      main()
    end
  end

  def solve(_n, a) do
    a |> Enum.map(&shuffle/1) |> shuffle()
  end

  # 隣の要素が距離n/2以上離れるよう並べ替える
  def shuffle(a) do
    a = List.to_tuple(a)
    n = tuple_size(a)
    Enum.concat(1..(n-1)//2, 0..(n-1)//2)
      |> Enum.map(&elem(a, &1))
  end
end

# Remove the following line when you run this code on AtCoder
Main.main
