#!/usr/bin/env ocaml

let string_repeat s n =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len;
  done;
  (res)
;;

let rec body n max =
  let padding = min (n mod 8) (8 - n mod 8) in
  Printf.printf
    "%s╚═(███)═╝\n" (string_repeat " " padding) ;
  if n < max - 1 then begin
    body (n + 1) max ;
  end
;;

let millipede n =
  Printf.printf
    "  ╚⊙ ⊙╝\n" ;
  body 0 n
;;

let run = millipede 20
