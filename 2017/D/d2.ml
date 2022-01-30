(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* 配列を使った動的計画法 (m < 23) *)
let solve_with_array n m recipes =
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

(* stringをキーとするMap *)
module StrMap = Map.Make(String)

(* Mapを使った動的計画法 (n <= 23) *)
let solve_with_map n m recipes =
  let zero = String.make m '0' in (* 全材料の残り0 *)

  (* 各残り方に対してそれが得られる最大レシピ数を返すMap *)
  let init_tbl = StrMap.singleton zero 0 in

  let final_tbl =
    fold_left  (* レシピごとにtblを更新 *)
      (fun cur_tbl recipe ->
         StrMap.fold (fun k v tbl -> (* cur_tbl中の各残り方について *)
             (* recipeを試したときの残り方と試したレシピ数 *)
             let new_k = xor k recipe in
             let new_v = if StrMap.mem new_k tbl
                         then max (StrMap.find new_k tbl) (v + 1)
                         else v + 1 in
             StrMap.add new_k new_v tbl) (* 新しいtbl *)
           cur_tbl cur_tbl)
      init_tbl recipes
  in
  (* 全材料の残りが0となる最大レシピ数 *)
  StrMap.find zero final_tbl

let solve n m recipes =
  if n > m then solve_with_array n m recipes
           else solve_with_map   n m recipes

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
