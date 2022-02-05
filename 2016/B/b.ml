(*
 * ACM-ICPC domestic qualifier 2016
 * Problem B: Look for the Winner!
 *)

open List

(* Python's enumerate: enumerate [a;b;c] => [(0,a);(1,b);(2,c)] *)
let enumerate ls =
  let rec enum' i = function
    [] -> [] | x::rest -> (i, x) :: enum' (i+1) rest
  in enum' 0 ls

(* 配列中の最大値を取る添字とその最大値. ただし添字excludeを除く *)
let array_max_index ?(exclude=(-1)) ary =
  snd (Array.fold_left
         (fun (i, (mi, m)) x ->
            (i + 1, if i <> exclude && x > m then (i, x) else (mi, m)))
         (0, (-1, min_int)) ary)

(* 結果を表すヴァリアント *)
type result_t = Tie | Winner of char * int

let solve n votes =
  (* 'A'--'Z'と0--25の相互変換 *)
  let int_of_alph c = int_of_char c - int_of_char 'A'
  and alph_of_int i = char_of_int (i + int_of_char 'A') in

  (* 得票数 *)
  let gained = Array.make (int_of_alph 'Z' + 1) 0 in

  fold_left            (* 各票について *)
    (fun res (i, c) -> (* res=暫定結果, i=何票目か, c=投票先 *)
       (* cの得票数を更新 *)
       gained.(int_of_alph c) <- gained.(int_of_alph c) + 1;
       (* 得票数1位と2位 *)
       let (top_i, top_g) = array_max_index gained in
       let (snd_i, snd_g) = array_max_index ~exclude:top_i gained in
       let remain = n - (i + 1) in (* 残り票数 *)
       (* 初めて勝利が決定したら結果を更新 *)
       if res = Tie && top_g > snd_g + remain
         then Winner (alph_of_int top_i, i + 1) else res)
    Tie (enumerate votes)

(* 標準入力から空白区切りの文字の列を読み出す *)
let read_char_list () =
  map (fun s -> s.[0]) (String.split_on_char ' ' (read_line ()))

let rec main () =
  let n = read_int () in
  if n <> 0 then
    let votes = read_char_list () in
    begin
      match solve n votes with
          Tie -> Printf.printf "TIE\n"
        | Winner (c, i) -> Printf.printf "%c %d\n" c i
    end;
    main ()

let () = main ()
