(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

fun g f1 f2 p =
    let val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** you can put all your code here ****)
val count_wildcards = fn p => g (fn _ => 1) (fn _ => 0) p

(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


fun only_capitals xs =
  let val first = Char.isUpper o (fn x=>String.sub(x, 0)) in
  List.filter first xs end

fun longest_string f xs = 
  List.foldl(fn (a,b) => if f(a, b) then a else b) "" xs

fun longest_string1 xs = 
  longest_string (fn (a,b) => String.size(a) >  String.size(b)) xs

fun longest_string2 xs = 
  longest_string (fn (a,b) => String.size(a) >= String.size(b)) xs

fun longest_string_helper f xs = 
  List.foldl(fn (a,b) => if f(String.size(a), String.size(b)) 
                         then a 
                         else b) "" xs 

val longest_string3 = fn xs => longest_string_helper (fn (a,b) => a >  b) xs
val longest_string4 = fn xs => longest_string_helper (fn (a,b) => a >= b) xs
val longest_capitalized = fn xs => (longest_string1 o only_capitals) xs
val rev_string = fn x => (String.implode o List.rev o String.explode) x

fun first_answer f xs =
  case (List.find(fn x=>
    case f(x) of
      SOME x => true
    | NONE => false) xs)
    of SOME x => x
    | NONE => raise NoAnswer


fun all_answers f xs = 
  let fun aux (xs, acc) =
  case (acc, xs) of
    (SOME acc,[])             => SOME acc
  | (SOME acc, (SOME x)::xs') => aux(xs', SOME(acc@x))
  | (_, (NONE)::xs')          => NONE
  in
    aux((List.map f xs), (SOME []))
  end
