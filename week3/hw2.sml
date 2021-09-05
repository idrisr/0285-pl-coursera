(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append(xs',ys)

(* put your solutions for problem 2 here *)

fun filter(s, xs) = 
  case xs of
       [] => []
     | x::xs' => if same_string(x, s)
                 then filter(s, xs')
                 else x::filter(s, xs')

fun is_in(s, xs) =
  case xs of
       [] => false
     | x::xs' => same_string(s, x) orelse is_in(s, xs')

fun all_except_option(s, xs) = 
  if is_in(s, xs)
  then SOME (filter(s, xs))
  else NONE

fun is_sub_list(xs, ys) =
  (* xs: sub list *)
  (* ys: big list *)
  case (xs, ys) of
       (_, []) => false
     | ([], _) => true
     | (x::xs',_) => if is_in(x, ys)
  then is_sub_list(xs', ys)
  else false

fun get_substitutions1(xs, s) =
  case xs of 
       [] => []
     | x::xs' => case all_except_option(s, x) of
                      NONE => get_substitutions1(xs', s)
                    | SOME x => x@get_substitutions1(xs', s)
