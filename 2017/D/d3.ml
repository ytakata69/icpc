(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* 動的計画法 (m < 23) *)
let solve_by_dp n m recipes =
  (* intに変換 *)
  let int_recipes = map (fun s -> int_of_string ("0b" ^ s)) recipes in
  let p2m = 1 lsl m in (* 2^m *)
  let tbl = Array.make p2m (-1) in  (* 全要素 -1 *)
  tbl.(0) <- 0;  (* 全材料の残り0, 試したレシピ数0 *)

  let final_tbl =
    fold_left  (* レシピごとにtblを更新 *)
      (fun tbl recipe ->
         let newtbl = Array.copy tbl in
         for i = 0 to p2m - 1 do
           if tbl.(i) >= 0 then
             let j = i lxor recipe  (* recipeを試したときの残り方 *)
             and v = tbl.(i) + 1 in (* 試し済みレシピ数 + 1 *)
             newtbl.(j) <- max newtbl.(j) v
         done;
         newtbl)
      tbl int_recipes
  in
  (* 全材料の残りが0である最大レシピ数 *)
  final_tbl.(0)

(* int配列でビット列を表すデータ構造 *)
module Bitv = struct
  type t = int array

  let block_size = 30
  let _size_for_m m = 
    let m' = if m mod block_size = 0 then m
             else m + (block_size - (m mod block_size)) in
    m' / block_size

  let create m b =
    let init = if b = true then (1 lsl block_size) - 1 else 0 in
    Array.make (_size_for_m m) init

  let of_string str =
    let m = String.length str in
    let ary = Array.make (_size_for_m m) 0 in
    for i = 0 to (_size_for_m m) - 1 do
      let slen = min (m - i * block_size) block_size in
      let s = String.sub str (i * block_size) slen in
      ary.(i) <- int_of_string ("0b" ^ s)
    done;
    ary

  let bw_xor x y =
    let len = Array.length x in
    let ary = Array.make len 0 in
    for i = 0 to len - 1 do
      ary.(i) <- x.(i) lxor y.(i)
    done;
    ary
end;;

(* based on: https://stackoverflow.com/a/14741059 *)
(*
module Bitv = struct
  type t = { len : int; buf : bytes }

  let create len x =
    let init = (if x = true then '\255' else '\000') in
    let buf = Bytes.make (len / 8 + 1) init in
    { len = len; buf = buf }

  let of_string str =
    let len = String.length str in
    let result = create len false in
    for i = 0 to Bytes.length result.buf - 1 do
      let slen = min (len - i * 8) 8 in
      if slen > 0 then
        let s = String.sub str (i * 8) slen in
        Bytes.set result.buf i (char_of_int (int_of_string ("0b" ^ s)))
    done;
    result

  let bw_xor x y =
    let result = create x.len false in
    for i = 0 to Bytes.length x.buf - 1 do
      Bytes.set result.buf i
      (char_of_int ((int_of_char (Bytes.get x.buf i)) lxor
                    (int_of_char (Bytes.get y.buf i))))
    done;
    result
end;;
*)

(* 全探索 (n <= 23) *)
let solve_by_bruteforce n m recipes =
  (* bitsetに変換 *)
  let bit_recipes = map (fun s -> Bitv.of_string s) recipes in
  let zero = Bitv.create m false in

  (* 各レシピについて試す・試さないをすべて調べる.
     used=試し済みレシピ数, remain=材料の残り方 *)
  let rec bruteforce used remain = function
      [] -> if remain = zero then used else 0
    | recipe :: rest ->
        max (bruteforce used remain rest)  (* recipeを試さない *)
            (bruteforce (used + 1) (Bitv.bw_xor remain recipe) rest)(* 試す *)
  in
  bruteforce 0 zero bit_recipes

let solve n m recipes =
  if n > m then solve_by_dp n m recipes
           else solve_by_bruteforce n m recipes

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let nm = read_int_list () in
  let (n, m) = (nth nm 0, nth nm 1) in
  if not (n = 0 && m = 0) then
    let recipes = init n (fun _ -> read_line ()) in
    Printf.printf "%d\n" (solve n m recipes);
    main ()

let () = main ()
