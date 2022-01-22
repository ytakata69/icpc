(*
 * ACM-ICPC domestic qualifier 2015
 * Problem C: ICPC Calculator
 *)

open List;;

type token = Plus | Mult | Num of int;;
exception ParseError;;

let parse_line line = (* convert one line to (level, token) *)
  let n = String.length line in
  let c = line.[n - 1] in
  let t = if c = '+' then Plus else
          if c = '*' then Mult else Num (int_of_char c - int_of_char '0')
  in (n - 1, t);;  (* (level, token) *)

let neutral = function Mult -> 1 | _ -> 0;; (* the neutral element *)
let apply op x y = match op with
  Plus -> x + y | Mult -> x * y | _ -> raise ParseError;;

let rec calculate level op acc lines = match lines with
    [] -> (acc, [])
  | (lv, token)::rest ->
      if lv = level then
        let (v, rest') = (* (the value of the subexp, remaining lines) *)
          match token with
            Num n -> (n, rest)
          | _ -> calculate (lv + 1) token (neutral token) rest
        in calculate lv op (apply op acc v) rest'
      else if lv < level then (acc, lines)  (* return to the caller *)
      else raise ParseError;;

let solve lines =
  let prepro = map parse_line lines in
  fst (calculate 0 Plus 0 prepro);;

let rec main () =
  let n = read_int () in
  if n <> 0 then
    let lines = init n (fun _ -> read_line ()) in
    Printf.printf "%d\n" (solve lines);
    main ();;

let () = main ();;
