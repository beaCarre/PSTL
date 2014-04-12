type _jni_jPoint;;
type _jni_jColored;;
type _jni_jColoredPoint;;
class type jPoint =
  object
    inherit JniHierarchy.top
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
  end
and jColored =
  object
    inherit JniHierarchy.top
    method _get_jni_jColored : _jni_jColored
    method getColor : unit -> string
    method setColor : string -> unit
  end
and jColoredPoint =
  object
    inherit jPoint
    inherit jColored
    method _get_jni_jColoredPoint : _jni_jColoredPoint
    method eq_colored_point : jColoredPoint -> bool
  end;;
val jPoint_of_top : JniHierarchy.top -> jPoint;;
val jColored_of_top : JniHierarchy.top -> jColored;;
val jColoredPoint_of_top : JniHierarchy.top -> jColoredPoint;;
val _instance_of_jPoint : JniHierarchy.top -> bool;;
val _instance_of_jColored : JniHierarchy.top -> bool;;
val _instance_of_jColoredPoint : JniHierarchy.top -> bool;;
val jArray_init_jPoint : int -> (int -> jPoint) -> jPoint JniArray.jArray;;
val jArray_init_jColored :
  int -> (int -> jColored) -> jColored JniArray.jArray;;
val jArray_init_jColoredPoint :
  int -> (int -> jColoredPoint) -> jColoredPoint JniArray.jArray;;
class point : int -> int -> jPoint;;
class default_point : unit -> jPoint;;
class colored_point : int -> int -> string -> jColoredPoint;;
class default_colored_point : unit -> jColoredPoint;;

val mypack_jPoint__main : string JniArray.jArray -> unit;;
val mypack_jColoredPoint__main : string JniArray.jArray -> unit;;
