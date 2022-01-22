(*
 * ACM-ICPC domestic qualifier 2015
 * Problem A: Entrance Examination
 *)

open List;;

(* gap_list [a1; a2; a3; ...] => [a1-a2; a2-a3; ...] *)
let rec gap_list = function
    ([] | [_]) -> []
  | x :: y :: rest -> (x - y) :: gap_list (y :: rest);;

let max_pair (a, b) (x, y) =
  if a > x || (a = x && b > y) then (a, b) else (x, y);;

let solve m nmin nmax ps =
  let gaps = gap_list ps in
  let index = tl (init m (fun i -> i)) in
  let indexed = combine gaps index in
  let filtered = filter (fun (d, i) -> nmin <= i && i <= nmax) indexed in
  let (_, n) = fold_left max_pair (0, 0) filtered in
  n;;

let rec main () =
  let (m, nmin, nmax) =
    Scanf.scanf "%d %d %d\n" (fun a b c -> (a, b, c)) in
  if m <> 0 then
    let ps = init m (fun _ -> Scanf.scanf "%d\n" (fun a -> a)) in
    Printf.printf "%d\n" (solve m nmin nmax ps);
    main ();;

let () = main ();;
