type _jni_jP;;
type _jni_jPoint;;
class type jP =
  object inherit JniHierarchy.top method _get_jni_jP : _jni_jP end
and jPoint = object inherit jP method _get_jni_jPoint : _jni_jPoint end;;
val jP_of_top : JniHierarchy.top -> jP;;
val jPoint_of_top : JniHierarchy.top -> jPoint;;
val _instance_of_jP : JniHierarchy.top -> bool;;
val _instance_of_jPoint : JniHierarchy.top -> bool;;
val jArray_init_jP : int -> (int -> jP) -> jP JniArray.jArray;;
val jArray_init_jPoint : int -> (int -> jPoint) -> jPoint JniArray.jArray;;



