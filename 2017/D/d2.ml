(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* stringをキーとするMap *)
module StrMap = Map.Make(String)

(* 0-1文字列のXOR *)
let xor s1 s2 =
  let m = String.length s1 in
  String.init m (fun i -> if s1.[i] = s2.[i] then '0' else '1')

(* Mapを使った動的計画法 *)
let solve n m recipes =
  let zero = String.make m '0' in (* 全材料の残り0 *)

  (* 各残り方に対してそれが得られる最大レシピ数を返すMap *)
  let init_tbl = StrMap.singleton zero 0 in

  (* 1レシピに関してcur_tblを更新する (新しいMapを返す) 関数 *)
  let update_table cur_tbl recipe =
    (* cur_tbl中の各残り方kに対しrecipeを試したときの残り方を追加 *)
    StrMap.fold
      (fun k v tbl ->
        let k' = xor k recipe in (* recipeを試した後の残り方 *)
        if StrMap.mem k' tbl && StrMap.find k' tbl > v
        then tbl (* 更新しない *)
        else StrMap.add k' (v + 1) tbl) (* 更新したMapを返す *)
      cur_tbl cur_tbl
  in

  (* update_tableを全レシピに適用 *)
  let final_tbl = fold_left update_table init_tbl recipes in

  (* 全材料の残りが0となる最大レシピ数 *)
  StrMap.find zero final_tbl

(* ----- 以下は入出力 ----- *)

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
