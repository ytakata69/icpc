(*
 * ACM-ICPC domestic qualifier 2015
 * Problem B: Short Phrase
 *)

open List;;

let is_tanku (ls : int list) : bool =
  let rec is_tanku' pattern ls =
    match (pattern, ls) with
      ([], _) -> true
    | (_, []) -> false
    | (p::ps, x::xs) ->
        (p > x && is_tanku' ((p-x)::ps) xs) || (p = x && is_tanku' ps xs)
  in is_tanku' [5; 7; 5; 7; 7] ls;;

(* the list of all suffixes *)
let rec suffix_list = function
  [] -> [[]] | x::xs -> (x::xs) :: suffix_list xs;;

(* Python's enumerate() *)
let enumerate ls =
  let rec enum' i = function
    [] -> [] | x::rest -> (i, x) :: enum' (i+1) rest
  in enum' 0 ls;;

let solve (ws : string list) : int =
  let lens = map String.length ws in
  let suffixes = suffix_list lens in
  1 + fst (find (fun (_, ls) -> is_tanku ls) (enumerate suffixes));;

let rec main () =
  let n = read_int () in
  if n <> 0 then
    let ws = init n (fun _ -> read_line ()) in
    Printf.printf "%d\n" (solve ws);
    main ();;

let () = main ();;
