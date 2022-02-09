(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * A: Taro's shopping
 *)

open List

(* Lisp's maplist. maplist f [1;2;3] => [f [1;2;3]; f [2;3]; f [3]] *)
let rec maplist f = function
  [] -> [] | x :: rest -> f (x :: rest) :: maplist f rest

let solve m prices =
  (* 2つの商品の合計額のリスト (のリスト) *)
  let sums = maplist (fun ls -> map ((+) (hd ls)) (tl ls)) prices in
  (* m以下の要素の最大値 *)
  fold_left max 0 (filter ((>=) m) (flatten sums))

(* ----- 以下は入出力 ----- *)

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let nm = read_int_list () in
  let (n, m) = (nth nm 0, nth nm 1) in
  if not (n = 0 && m = 0) then
    let prices = read_int_list () in
    let max = solve m prices in
    print_endline (if max > 0 then string_of_int max else "NONE");
    main ()

let () = main ()
