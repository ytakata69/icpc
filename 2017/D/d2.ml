(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* stringをキーとするMap *)
module StrMap = Map.Make(String)

(* 文字列のXOR *)
let xor s1 s2 =
  String.init (String.length s1)
    (fun i -> char_of_int ((int_of_char s1.[i]) lxor (int_of_char s2.[i])))

(* '0'と'1'の文字列を8ビット1文字の文字列に圧縮 *)
let packed_string str =
  let len = String.length str in
  let size = len / 8 + if len mod 8 > 0 then 1 else 0 in
  String.init size (fun i ->
    let sublen = min 8 (len - i * 8) in
    char_of_int (int_of_string ("0b" ^ String.sub str (i * 8) sublen)))

(* Mapを使った動的計画法 *)
let solve n m recipes =
  let zero = String.make m '0' in (* 全材料の残り0 *)
  let zero = packed_string zero in

  (* 各残り方に対してそれが得られる最大レシピ数を返すMap *)
  let init_tbl = StrMap.singleton zero 0 in

  let final_tbl =
    fold_left  (* レシピごとにtblを更新 *)
      (fun cur_tbl recipe ->
         let recipe = packed_string recipe in

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
