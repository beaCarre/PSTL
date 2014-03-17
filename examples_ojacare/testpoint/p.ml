type _jni_jP = Jni.obj;;
type _jni_jPoint = Jni.obj;;
class type jP =
  object inherit JniHierarchy.top method _get_jni_jP : _jni_jP end
and jPoint = object inherit jP method _get_jni_jPoint : _jni_jPoint end;;
let __jni_obj_of_jni_jP (java_obj : _jni_jP) =
  (Obj.magic : _jni_jP -> Jni.obj) java_obj;;
let __jni_obj_of_jni_jPoint (java_obj : _jni_jPoint) =
  (Obj.magic : _jni_jPoint -> Jni.obj) java_obj;;
let __jni_jP_of_jni_obj =
  let clazz =
    try Jni.find_class "mypack/P"
    with | _ -> failwith "Class not found : mypack.P."
  in
    fun (java_obj : Jni.obj) ->
      if not (Jni.is_instance_of java_obj clazz)
      then failwith "``cast error'' : jP (mypack/P)"
      else (Obj.magic java_obj : _jni_jP);;
let __jni_jPoint_of_jni_obj =
  let clazz =
    try Jni.find_class "mypack/Point"
    with | _ -> failwith "Class not found : mypack.Point."
  in
    fun (java_obj : Jni.obj) ->
      if not (Jni.is_instance_of java_obj clazz)
      then failwith "``cast error'' : jPoint (mypack/Point)"
      else (Obj.magic java_obj : _jni_jPoint);;
let _alloc_jP =
  let clazz = Jni.find_class "mypack/P"
  in fun () -> (Jni.alloc_object clazz : _jni_jP);;
let _alloc_jPoint =
  let clazz = Jni.find_class "mypack/Point"
  in fun () -> (Jni.alloc_object clazz : _jni_jPoint);;

class _capsule_jP =
  let clazz = Jni.find_class "mypack/P"
  in
    fun (jni_ref : _jni_jP) ->
      let _ =
        if Jni.is_null jni_ref
        then raise (JniHierarchy.Null_object "mypack/P")
        else ()
      in
        object (self)
          method _get_jni_jP = jni_ref
          inherit JniHierarchy.top jni_ref
        end
and _capsule_jPoint = let clazz = Jni.find_class "mypack/Point"
  in
    let _ =
      if not (Jni.is_assignable_from clazz (Jni.find_class "mypack/P"))
      then
        failwith
          "Wrong super class in IDL : mypack.Point not extends mypack.P."
      else ()
    in
      fun (jni_ref : _jni_jPoint) ->
        let _ =
          if Jni.is_null jni_ref
          then raise (JniHierarchy.Null_object "mypack/Point")
          else ()
        in
          object (self)
            method _get_jni_jPoint = jni_ref
            method _get_jni_jP = jni_ref
            inherit JniHierarchy.top jni_ref
          end;;
let jP_of_top (o : JniHierarchy.top) : jP =
  new _capsule_jP (__jni_jP_of_jni_obj o#_get_jniobj);;
let jPoint_of_top (o : JniHierarchy.top) : jPoint =
  new _capsule_jPoint (__jni_jPoint_of_jni_obj o#_get_jniobj);;
let _instance_of_jP =
  let clazz = Jni.find_class "mypack/P"
  in fun (o : JniHierarchy.top) -> Jni.is_instance_of o#_get_jniobj clazz;;
let _instance_of_jPoint =
  let clazz = Jni.find_class "mypack/Point"
  in fun (o : JniHierarchy.top) -> Jni.is_instance_of o#_get_jniobj clazz;;
let _new_jArray_jP size =
  let java_obj = Jni.new_object_array size (Jni.find_class "mypack/P")
  in
    new JniArray._Array Jni.get_object_array_element Jni.
      set_object_array_element (fun jniobj -> new _capsule_jP jniobj)
      (fun obj -> obj#_get_jni_jP) java_obj;;
let jArray_init_jP size f =
  let a = _new_jArray_jP size
  in (for i = 0 to pred size do a#set i (f i) done; a);;
let _new_jArray_jPoint size =
  let java_obj = Jni.new_object_array size (Jni.find_class "mypack/Point")
  in
    new JniArray._Array Jni.get_object_array_element Jni.
      set_object_array_element (fun jniobj -> new _capsule_jPoint jniobj)
      (fun obj -> obj#_get_jni_jPoint) java_obj;;
let jArray_init_jPoint size f =
  let a = _new_jArray_jPoint size
  in (for i = 0 to pred size do a#set i (f i) done; a);;





