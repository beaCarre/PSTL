type _jni_jPoint;;
class type jPoint =
  object
    inherit JniHierarchy.top
    method _get_jni_jPoint : _jni_jPoint
    method mETHStrVoid : unit -> string
    method mETHVoidIntInt : int -> int -> unit
    method mETHPointPoint : jPoint -> jPoint
    method mETHIntBoolStringPoint : bool -> string -> jPoint -> int
  end;;
val jPoint_of_top : JniHierarchy.top -> jPoint;;
val _instance_of_jPoint : JniHierarchy.top -> bool;;
val jArray_init_jPoint : int -> (int -> jPoint) -> jPoint JniArray.jArray;;



