fun a(a, b) = 
  a+b

fun b a b =
  a+b


val c = ListPair.zip( [1, 2, 3], [4, 5, 6]);

val d = List.map a c
