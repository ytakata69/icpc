(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * A: Taro's shopping
 *)

(* a faster algorithm *)

open List

let solve n m prices =
  (* ソート & 配列に変換 *)
  let table = Array.of_list (sort compare prices) in
  (* 配列の両端から探索 *)
  let rec search i j acc =
    if i >= j then acc else
      let p = table.(i) + table.(j) in
      if p <= m then search (i + 1) j (max acc p)
                else search i (j - 1) acc
  in search 0 (n - 1) 0

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let nm = read_int_list () in
  let (n, m) = (nth nm 0, nth nm 1) in
  if not (n = 0 && m = 0) then
    let prices = read_int_list () in
    let max = solve n m prices in
    print_endline (if max > 0 then string_of_int max else "NONE");
    main ()

let () = main ()
