(*
 * ACM-ICPC domestic qualifier 2016
 * Problem B: Look for the Winner!
 *)

open List

(* 結果を表すヴァリアント *)
type result_t = Tie | Winner of char * int

let solve n votes =
  (* 'A'--'Z'と0--25の相互変換 *)
  let int_of_alph c = int_of_char c - int_of_char 'A'
  and alph_of_int i = char_of_int (i + int_of_char 'A') in

  (* 得票数 *)
  let gained = Array.make (int_of_alph 'Z' + 1) 0 in

  snd (fold_left       (* 各票について *)
    (fun ((i, top, sec), res) c ->
       (* i=何票目か, top=1位, sec=2位, res=暫定結果, c=投票先 *)
       (* cの得票数を更新 *)
       let ci = int_of_alph c in
       gained.(ci) <- gained.(ci) + 1;
       (* 新しい1位と2位 *)
       let top' = if gained.(ci) > gained.(top) then ci else top in
       let sec' = if gained.(ci) > gained.(top) then top else
                  if gained.(ci) > gained.(sec) && ci <> top then ci else sec in
       ((i + 1, top', sec'),
         (* 初めて勝利が決定したら結果を更新 *)
         if res = Tie && gained.(top') > gained.(sec') + (n - i)
           then Winner (alph_of_int top', i) else res))
    ((1, int_of_alph 'A', int_of_alph 'B'), Tie) votes)

(* ----- 以下は入出力 ----- *)

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
