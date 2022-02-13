(*
 * ACM-ICPC domestic qualifier 2016
 * Problem D: Daruma Otoshi
 *)

open List

let pairable n m = abs(n - m) <= 1

let solve n weights =
  (* 配列に変換 *)
  let ws = Array.of_list weights in

  (* removable.(l).(i) = 区間[i, i + l]がすべて消せるか *)
  let removable = Array.init n (fun _ -> Array.make n false) in

  (* removable.(l).(i) を小さい l から順に埋めていく *)
  for l = 1 to n - 1 do
    for i = 0 to n - l - 1 do
      (* 区間[i + 1, i + l - 1]がすべて消せて両端も消せるなら *)
      if pairable ws.(i) ws.(i + l) &&
         (l < 2 || removable.(l - 2).(i + 1)) then
        removable.(l).(i) <- true
      else
        (* [i, i + m] と [i + m + 1, i + l] に分ける各場合 *)
        for m = 0 to l - 1 do
          removable.(l).(i) <- removable.(l).(i) ||
            (removable.(m).(i) && removable.(l - m - 1).(i + m + 1))
        done
    done
  done;

  (* max_removable.(l) = 区間[0, l - 1]の中で消せる最大ブロック数 *)
  let max_removable = Array.make (n + 1) 0 in

  (* max_removable.(l) を小さい l から順に埋めていく *)
  for l = 2 to n do
    max_removable.(l) <- max_removable.(l - 1);
    for i = 0 to l - 1 do
      if removable.(l - i - 1).(i) then (* [i, l - 1]が消せる *)
        max_removable.(l) <- max max_removable.(l) (max_removable.(i) + (l-i))
    done
  done;

  max_removable.(n)

(* ----- 以下は入出力 ----- *)

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let n = read_int () in
  if n <> 0 then begin
    let weights = read_int_list () in
    print_int (solve n weights); print_newline ();
    main ()
  end

let () = main ()
