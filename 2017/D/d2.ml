(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* 動的計画法 (m < 23) *)
let solve_by_dp n m recipes =
  (* intに変換 *)
  let int_recipes = map (fun s -> int_of_string ("0b" ^ s)) recipes in
  (* 2^m通りの各残り方に対し, それが得られるレシピ数 *)
  let tbl = Array.make (1 lsl m) (-1) in
  tbl.(0) <- 0;  (* 残り方0 -> 試したレシピ数0 *)

  let final_tbl =
    fold_left  (* レシピごとにtblを更新 *)
      (fun tbl recipe ->
         let newtbl = Array.copy tbl in
         Array.iteri (fun i v ->
           if v >= 0 then
             let j = i lxor recipe in (* recipeを試したときの残り方 *)
             newtbl.(j) <- max newtbl.(j) (v + 1))
           tbl;
         newtbl)
      tbl int_recipes
  in
  (* 全材料の残りが0である最大レシピ数 *)
  final_tbl.(0)

let xor s1 s2 =
  let m = String.length s1 in
  String.init m (fun i -> if s1.[i] <> s2.[i] then '1' else '0')

(* 全探索 (n <= 23) *)
let solve_by_bruteforce n m recipes =
  let zero = String.make m '0' in

  (* 各レシピについて試す・試さないをすべて調べる.
     used=試し済みレシピ数, remain=材料の残り方 *)
  let rec bruteforce used remain = function
      [] -> if remain = zero then used else 0
    | recipe :: rest ->
        max (bruteforce used remain rest)  (* recipeを試さない *)
            (bruteforce (used + 1) (xor remain recipe) rest)  (* 試す *)
  in
  bruteforce 0 zero recipes

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
