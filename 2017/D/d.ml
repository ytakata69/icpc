(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * D: Making Lunch Boxes
 *)

open List

(* 2つのbool listに対するXOR *)
let xor = map2 (<>)

let solve n m recipes =
  let tbl = ref (Hashtbl.create 1234567) in
  let zero = init m (fun _ -> false) in (* 全材料の残り0 *)
  Hashtbl.add !tbl zero 0; (* 全材料の残り0, 試したレシピ数0 *)

  iter (* 各レシピについて *)
    (fun recipe ->
       let newtbl = Hashtbl.copy !tbl in
       Hashtbl.iter (* 各材料の残り方について *)
         (fun key value ->
            let newkey = xor recipe key in (* recipeを試したときの残り方 *)
            match Hashtbl.find_opt newtbl newkey with
                None   -> Hashtbl.add newtbl newkey (value + 1)
              | Some x -> Hashtbl.replace newtbl newkey (max x (value + 1)))
         !tbl;
       tbl := newtbl)
    recipes;
  (* 全材料の残りが0である最大レシピ数 *)
  Hashtbl.find !tbl zero

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

(* 0-1文字列をbool listに変換 *)
let bools_of_string str =
  init (String.length str) (fun i -> str.[i] = '1')

let rec main () =
  let nm = read_int_list () in
  let (n, m) = (nth nm 0, nth nm 1) in
  if not (n = 0 && m = 0) then
    let recipes = init n (fun _ -> bools_of_string (read_line ())) in
    Printf.printf "%d\n" (solve n m recipes);
    main ()

let () = main ()
