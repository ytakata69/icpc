(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * B: Almost Identical Programs
 *)

open List

(* 奇数・偶数番目の2リストに分ける. splitter [3;1;5;4;2] => ([3;5;2],[1;4]) *)
let rec splitter = function
    [] -> ([], [])
  | x::rest -> let (left, right) = splitter rest in (x::right, left)

(* リスト中の値vの個数 *)
let count v ls = length (filter ((=) v) ls)

let solve s1 s2 =
  (* '"'で分割. Str.split_delim (Str.regexp "\"") でも良い. *)
  let t1 = String.split_on_char '"' s1
  and t2 = String.split_on_char '"' s2 in
  if length t1 <> length t2 then "DIFFERENT"
  else
    (* 等しいか否か (true/false) のリストを作り, 奇数番目と偶数番目に分け,
       falseの個数を数える *)
    let (outer, inner) = splitter (map2 (=) t1 t2) in
    let (outer, inner) = (count false outer, count false inner) in
    if outer > 0 || inner > 1 then "DIFFERENT"
    else if inner = 0 then "IDENTICAL"
    else "CLOSE"

(* ----- 以下は入出力 ----- *)

let rec main () =
  let s1 = read_line () in
  if s1 <> "." then
    let s2 = read_line () in
    print_endline (solve s1 s2);
    main ()

let () = main ()
