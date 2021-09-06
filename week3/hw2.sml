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



fun is_in(s, xs, f) =
  case xs of
       [] => false
     | x::xs' => f(s, x) orelse is_in(s, xs', f)

fun card_color c =
  case c of
       (Clubs,_) => Black
     | (Spades,_) => Black
     | (Hearts,_) => Red
     | (Diamonds,_) => Red

fun all_same_color cs =
  case cs of
       [] => true
     | x::[] => true
     | head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

fun same_card(c1, c2) =
  c1=c2

    
fun card_value c =
  case c of
       (_, Ace) => 11
     | (_, King) => 10
     | (_, Queen) => 10
     | (_, Jack) => 10
     | (_, Num i) => i

fun sum_cards cs =
  let fun aux(cs, acc) =
  case cs of
       [] => acc
     | c::cs' => aux(cs', acc+card_value c)
  in
    aux(cs, 0)
  end

fun score(cs, goal) =
  let val total = sum_cards cs
    val prelim = if total > goal
                 then 3 * (total-goal)
                 else goal-total
  in
    if all_same_color(cs)
    then prelim div 2
    else prelim
  end


(* put your solutions for problem 2 here *)
fun filter_first(cs, c) =
  case cs of
       [] => []
     | d::cs' => if d=c
                 then cs'
                 else d::filter_first(cs', c)

fun remove_card(cs, c, e) =
  if not(is_in(c, cs, same_card))
  then raise e
  else filter_first(cs, c)

fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append(xs',ys)


fun filter(s, xs) = 
  case xs of
       [] => []
     | x::xs' => if same_string(x, s)
                 then filter(s, xs')
                 else x::filter(s, xs')



fun all_except_option(s, xs) = 
  if is_in(s, xs, same_string)
  then SOME (filter(s, xs))
  else NONE


fun get_substitutions1(xs, s) =
  case xs of 
       [] => []
     | x::xs' => case all_except_option(s, x) of
                      NONE   => get_substitutions1(xs', s)
                    | SOME x => x@get_substitutions1(xs', s)


fun get_substitutions2(xs, s) =
  let fun aux(acc, xs) =
  case xs of 
       [] => acc
     | x::xs' => case all_except_option(s, x) of
                      NONE   => aux(acc,  xs')
                    | SOME x => aux(acc@x,xs')
  in
    aux([], xs)
  end


fun similar_names(xs, name: {first:string,middle:string,last:string}) =
  let 
    val {first=x,middle=y,last=z} = name
    fun make_name(x) = {first=x, middle=y, last=z}
    val ys = get_substitutions2(xs, x) 
    fun map (xs,f) =
          case xs of
              [] => []
            | x::xs' => f(x) :: map(xs',f)
  in
    name::map(ys, make_name)
  end

fun officiate(cs, ms, goal) = 
  let 
    fun turn (cs, ms, hs) = 
      (* no more moves game over *)
      case (cs, ms, hs) of
           (_, [], _) => score(hs, goal)

           (* no more cards *)
         | ([], m::ms', _) => 
             (case m of 
                   Discard c => turn(cs, ms, remove_card(hs, c, IllegalMove))
                 | Draw => score(hs, goal))
           (* more cards *)
         | (c::cs', m::ms', _) => 
             (case m of 
                   Discard c => turn(cs, ms', remove_card(hs, c, IllegalMove))
                 | Draw => if score(c::hs, goal) > goal
                            then score(c::hs, goal)
                            else turn(cs', ms', c::hs))
  in
    turn(cs, ms, [])
  end
