(*
 * ACM-ICPC domestic qualifier 2016
 * Problem C: Bamboo Blossoms
 *)

open List

(* 初めて花が咲かない年の最大値 (Output for the Sample Inputより) *)
let max_no_bloom = 7368791

(* m年以降初めて花が咲かない年を返す. n=区画数 *)
let solve m n =
  (* bloom.(i): i年後に花を咲かせられる *)
  let bloom = Array.make (max_no_bloom + 1) false in

  (* i年竹の種をまく *)
  let sow_seeds i =
    let rec make_bloom j = (* j年に花が咲く *)
      if j <= max_no_bloom then begin
        bloom.(j) <- true; make_bloom (j + i) end
    in make_bloom i
  in

  (* i年後以降初めて花が咲かない年. n=空き区画数 *)
  let rec first_no_bloom i n =
    if not bloom.(i) then
      if n <= 0 then i (* 残り区画がなければi年後花が咲かない *)
      else begin       (* 残り区画の1つにi年竹の種をまいて次のiへ *)
        sow_seeds i; first_no_bloom (i+1) (n-1) end
    else first_no_bloom (i+1) n  (* i年後は花が咲くので次のiへ *)
  in
  first_no_bloom m n

(* 標準入力から空白区切りの整数の列を読み出す *)
let read_int_list () =
  map int_of_string (String.split_on_char ' ' (read_line ()))

let rec main () =
  let mn = read_int_list () in
  let (m, n) = (nth mn 0, nth mn 1) in
  if not (m = 0 && n = 0) then begin
    print_int (solve m n); print_newline ();
    main ()
  end

let () = main ()
