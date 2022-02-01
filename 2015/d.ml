(*
 * ACM-ICPC domestic qualifier 2015
 * Problem D: 500-yen Saving
 *)

open List;;

let max_price = 5000;;

(* 1つの店での買い物の結果 *)
let buy_or_not_buy dp price =
  let dp' = Array.copy dp in
  (* 得られる小銭 *)
  let change = (max_price - price) mod 1000 in
  Array.iteri
    (fun remain (coins, spent) ->
      if coins >= 0 then
        let (remain, coins) = if remain + change < 500
          then (remain + change, coins)
          else (remain + change - 500, coins + 1) in
        let spent = spent - price in (* 消費額は負数で考える *)
        dp'.(remain) <- max dp'.(remain) (coins, spent))
    dp;
  dp';;

let solve n ps =
  (* 残っている小銭は最大 999 * 店数 *)
  let dp = Array.make (999 * n + 1) (-1, 0) in
  dp.(0) <- (0, 0);  (* dp.(remain) = (coins, spent) *)
  let dp = fold_left buy_or_not_buy dp ps in
  Array.fold_left max (0, 0) dp;;

let rec main () =
  let n = read_int () in
  if n <> 0 then
    let ps = init n (fun _ -> read_int ()) in
    let (c, s) = solve n ps in
    Printf.printf "%d %d\n" c (-s);
    main ();;

let () = main ();;
