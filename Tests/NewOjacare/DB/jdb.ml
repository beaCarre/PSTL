type top = java'lang'Object java_instance;;
exception Null_object of string;;
type _jni_jGraphicDB = mypack'GraphicDB java_instance;;
class type jGraphicDB =
  object method _get_jni_jGraphicDB : _jni_jGraphicDB end;;
class _capsule_jGraphicDB (jni_ref : _jni_jGraphicDB) =
  let _ =
    if Java.is_null jni_ref
    then raise (Null_object "mypack/GraphicDB")
    else ()
  in object (self) method _get_jni_jGraphicDB = jni_ref end;;
let jGraphicDB_of_top (o : top) : jGraphicDB =
  new _capsule_jGraphicDB (Java.cast "mypack.GraphicDB" o);;
let _instance_of_jGraphicDB (o : top) =
  Java.instanceof "mypack.GraphicDB" o;;

let getJarray _p1  =
  let _p1a = Java.make_array "java.lang.String[]" (Int32.of_int (Array.length _p1)) in
  for i=0 to ((Array.length _p1)-1) do
    JavaReferenceArray.set _p1a (Int32.of_int i) (JavaString.of_string _p1.(i))
  done;
  _p1a
let get_array_array _p2 =
  let _p2a =
    Java.make_array "java.lang.String[][]" (Int32.of_int( Array.length _p2))  (Int32.of_int( Array.length _p2.(0))) in
  for i=0 to ((Array.length _p2)-1) do
    for j=0 to (Array.length _p2.(0))-1 do
      JavaReferenceArray.set (JavaReferenceArray.get _p2a (Int32.of_int i)) (Int32.of_int j) (JavaString.of_string _p2.(i).(j))
    done
  done;
  _p2a

class graphicDB _p0 _p1 _p2 =
  let _p2a =  get_array_array _p2 in
  let _p1a = getJarray _p1 in
  
  let _p0 = JavaString.of_string _p0
      in
        let java_obj =
          Java.make
            "mypack.GraphicDB(java.lang.String,java.lang.String[],java.lang.String[][])"
            _p0 _p1a _p2a
        in object (self) inherit _capsule_jGraphicDB java_obj end;;


