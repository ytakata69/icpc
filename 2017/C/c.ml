(*
 * ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
 * C: A Garden with Ponds
 *)

open List

(* Python's range: range 1 4 => [1; 2; 3] *)
let range n m = if m > n then init (m - n) ((+) n) else []

(* 直積: cartesian [1;2;3] [4;5] => [(1,4);(1,5);(2,4);(2,5);(3,4);(3,5)] *)
let cartesian l1 l2 =
  flatten (map (fun x -> map (fun y -> (x, y)) l2) l1)

(* 名前通りの便利関数 *)
let list_max = fold_left max min_int
let list_min = fold_left min max_int
let list_sum = fold_left (+) 0

let solve d w garden =
  (* 2次元配列に変換 *)
  let garray = Array.of_list (map Array.of_list garden) in
  let height_at (x, y) = garray.(y).(x) in

  (* すべてのXY座標のリスト *)
  let xys = cartesian (range 0 w) (range 0 d) in
  (* (x, y)を左上隅とする長方形のリスト *)
  let every_fence (x, y) =
    map (fun (fw, fd) -> (x, y, fw, fd))
      (cartesian (range 3 (w - x + 1)) (range 3 (d - y + 1))) in
  (* すべての長方形のリスト *)
  let fences = flatten (map every_fence xys) in

  (* 外周(x, y, fw, fd)の高さ *)
  let fence_height (x, y, fw, fd) =
    (* 外周上の全座標 *)
    let fence_xys =
      cartesian [x] (range y (y + fd)) @
      cartesian [x + fw - 1] (range y (y + fd)) @
      cartesian (range x (x + fw)) [y] @
      cartesian (range x (x + fw)) [y + fd - 1] in
    (* 最小の高さ *)
    list_min (map height_at fence_xys)
  in

  (* 長方形(x, y, fw, fd)を外周とする池の容量 *)
  let pond_capacity (x, y, fw, fd) =
    (* 外周の高さ *)
    let fh = fence_height (x, y, fw, fd) in
    (* 外周の内側の全座標 *)
    let inner_xys =
      cartesian (range (x + 1) (x + fw - 1)) (range (y + 1) (y + fd - 1)) in
    (* 各座標の外周からの深さ *)
    let dps = map (fun (x, y) -> fh - height_at (x, y)) inner_xys in
    (* すべて外周より低ければ深さの和を返す *)
    if list_min dps > 0 then list_sum dps else 0
  in
  (* 最大の容量 *)
  list_max (map pond_capacity fences)

(* ----- 以下は入出力 ----- *)

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let dw = read_int_list () in
  let (d, w) = (nth dw 0, nth dw 1) in
  if not (d = 0 && w = 0) then
    let garden = init d (fun _ -> read_int_list ()) in
    Printf.printf "%d\n" (solve d w garden);
    main ()

let () = main ()
