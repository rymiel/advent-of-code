type coord = int * int
type t = coord

val compare : 'a -> 'a -> int
val manhattan : coord -> coord -> int
val chebyshev : coord -> coord -> int
val ( + ) : coord -> coord -> coord
val ( - ) : coord -> coord -> coord
val ( * ) : coord -> int -> coord
val ( % ) : coord -> coord -> coord
val print : out_channel -> coord -> unit
val to_string : coord -> string
val ( >> ) : coord -> Dir.t -> coord
val left : coord
val right : coord
val up : coord
val down : coord
val west : coord
val east : coord
val north : coord
val south : coord
