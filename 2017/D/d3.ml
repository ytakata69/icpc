(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* 0-1文字列のXOR *)
let xor s1 s2 =
  let m = String.length s1 in
  String.init m (fun i -> if s1.[i] = s2.[i] then '0' else '1')

(* 昇順に整列された連想リストに対し, 同一キーのエントリを最大値以外削除 *)
let compress =
  let compress' acc (k, v) =
    let (k', v') = hd acc in
      (k, v) :: if k = k' then tl acc else acc (* k = k'なら(k', v')を削除 *)
  in function [] -> []
     | x :: rest -> fold_left compress' [x] rest

(* 連想リストを使った動的計画法 *)
let solve n m recipes =
  let zero = String.make m '0' in (* 全材料の残り0 *)

  (* 各残り方に対してそれが得られる最大レシピ数を返す連想リスト *)
  let init_tbl = [(zero, 0)] in

  (* 1レシピに関してcur_tblを更新する (新しい連想リストを返す) 関数 *)
  let update_table cur_tbl recipe =
    (* cur_tbl中の各残り方kに対しrecipeを試したときの残り方を追加 *)
    let updated = fold_left
      (fun tbl (k, v) -> (xor k recipe, v + 1) :: tbl)
      cur_tbl cur_tbl
    in
    compress (sort compare updated) (* 同一の残り方を一つにまとめる *)
  in

  (* update_tableを全レシピに適用 *)
  let final_tbl = fold_left update_table init_tbl recipes in

  (* 全材料の残りが0となる最大レシピ数 *)
  assoc zero final_tbl

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
