(*
 * ACM-ICPC domestic qualifier 2016
 * Problem B: Look for the Winner!
 *)

open List

type result_t = Tie | Winner of (char * int)

let solve n votes =
  (* 得票数 *)
  let gained = ref
    (init 26 (fun i -> (char_of_int (i + int_of_char 'A'), 0))) in

  snd
    (fold_left (* 各票について *)
       (fun (i, res) c -> (* i=何票目か, res=暫定結果, c=投票先 *)
          (* 候補者cの得票数を更新 *)
          let g = assoc c !gained in
          gained := remove_assoc c !gained;
          gained := (c, g + 1) :: !gained;
          (* 得票数の降順に整列 *)
          gained := sort (fun (_, g1) (_, g2) -> g2 - g1) !gained;
          let (topc, topg) = nth !gained 0    (* 1位 *)
          and (sndc, sndg) = nth !gained 1 in (* 2位 *)
          let remain = n - (i + 1) in (* 残り投票数 *)
          (i + 1, if topg > sndg + remain && res = Tie
                  then Winner (topc, i + 1) else res))
       (0, Tie)
       votes)

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
