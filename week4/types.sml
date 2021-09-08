(* type grouping is left associative *)

(* int option *)
SOME 8;

(* a' option *)
NONE;

(* string list *)
["IDRIS", "RAJA"];

(* (string list) option *)

SOME ["IDRIS", "RAJA"];
(* (((string option) list) option) *)
SOME [SOME "IDRIS", NONE];

(* a' list *)
[];

(* int *)
1;

(* int list *)
[1];

(* int list option *)
SOME [1];

(* int list option list *)
[SOME [1], NONE, SOME [1, 2, 3, 4]]
