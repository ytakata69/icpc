(*
 * ACM-ICPC domestic qualifier 2015
 * Problem D: 500-yen Saving
 *)

open List;;

let max_price = 5000;;

let max_pair (c1, s1) (c2, s2) =
  if c1 > c2 || (c1 = c2 && s1 <= s2) then (c1, s1) else (c2, s2);;

let buy_or_not_buy dp price =
  let dp' = Array.copy dp in
  let change = (max_price - price) mod 1000 in
  Array.iteri
    (fun remain (coins, spent) ->
      if coins >= 0 then
        let (remain', coins') = if remain + change < 500
          then (remain + change, coins)
          else (remain + change - 500, coins + 1) in
        let spent' = spent + price in
        dp'.(remain') <- max_pair dp'.(remain') (coins', spent'))
    dp;
  dp';;

let solve n ps =
  let dp = Array.make (1000 * n) (-1, -1) in
  dp.(0) <- (0, 0);  (* dp.(remain) = (coins, spent) *)
  let dp' = fold_left buy_or_not_buy dp ps in
  Array.fold_left max_pair (0, 0) dp';;

let rec main () =
  let n = read_int () in
  if n <> 0 then
    let ps = init n (fun _ -> read_int ()) in
    let (c, s) = solve n ps in
    Printf.printf "%d %d\n" c s;
    main ();;

let () = main ();;
