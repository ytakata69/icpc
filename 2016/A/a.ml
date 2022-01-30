(*
 * ACM-ICPC domestic qualifier 2016
 * Problem A: Selection of Participants of an Experiment
 *)

open List

(* gap_list [a1; a2; a3; ...] => [a2-a1; a3-a2; ...] *)
let rec gap_list = function
    ([] | [_]) -> []
  | x :: y :: rest -> (y - x) :: gap_list (y :: rest)

let solve scores =
  let gaps = gap_list (sort compare scores) in
  fold_left min max_int gaps

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let n = read_int () in
  if n <> 0 then
    let scores = read_int_list () in
    Printf.printf "%d\n" (solve scores);
    main ()

let () = main ()
