type top;;

type _jni_jPoint;;
type _jni_jColored;;

class type jPoint =
object
  method _get_jni_jPoint : _jni_jPoint
  method set_x : int -> unit
  method get_x : unit -> int
  method set_y : int -> unit
  method get_y : unit -> int
  method moveto : int -> int -> unit
  method rmoveto : int -> int -> unit
  method toString : unit -> string
  method display : unit -> unit
  method distance : unit -> float
  method eq : jPoint -> bool
end;;

class type jColored =
object
  method _get_jni_jColored : _jni_jColored
  method getColor : unit -> string
  method setColor : string -> unit
end;;

val jPoint_of_top : top -> jPoint;; 
val jColored_of_top : top -> jColored;;

val _instance_of_jPoint : top -> bool;;
val _instance_of_jColored : top -> bool;;

class point : int -> int -> jPoint;;

class default_point : unit -> jPoint;;
