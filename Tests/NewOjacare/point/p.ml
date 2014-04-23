type top = java'lang'Object java_instance;;
exception Null_object of string;;
type _jni_jPoint = mypack'Point java_instance;;
type _jni_jColored = mypack'Colored java_instance;;
type _jni_jColoredPoint = mypack'ColoredPoint java_instance;;
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
    method display : int64 -> int
    method distance : unit -> int64
    method eq : jPoint -> bool
  end
and jColored =
  object
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
class _capsule_jPoint (jni_ref : _jni_jPoint) =
  let _ =
    if Java.is_null jni_ref then raise (Null_object "mypack/Point") else ()
  in
    object (self)
      method eq =
        fun (_p0 : jPoint) ->
          let _p0 = _p0#_get_jni_jPoint
          in Java.call "mypack.Point.eq(mypack.Point):boolean" jni_ref _p0
      method distance =
        fun () ->
          Int64.to_int (Java.call "mypack.Point.distance():long" jni_ref ())
      method display =
        fun _p0 ->
          let _p0 = Int64.of_int _p0
          in Java.call "mypack.Point.display(long):int" jni_ref _p0
      method toString =
        fun () ->
          JavaString.to_string
            (Java.call "mypack.Point.toString():java.lang.String" jni_ref ())
      method rmoveto =
        fun _p0 _p1 ->
          let _p1 = Int32.of_int _p1 in
          let _p0 = Int32.of_int _p0
          in Java.call "mypack.Point.rmoveto(int,int):void" jni_ref _p0 _p1
      method moveto =
        fun _p0 _p1 ->
          let _p1 = Int32.of_int _p1 in
          let _p0 = Int32.of_int _p0
          in Java.call "mypack.Point.moveto(int,int):void" jni_ref _p0 _p1
      method set_y =
        fun _p ->
          let _p = Int32.of_int _p
          in Java.set "mypack.Point.y:int" jni_ref _p
      method get_y = fun () -> Java.get "mypack.Point.y:int" jni_ref
      method set_x =
        fun _p ->
          let _p = Int32.of_int _p
          in Java.set "mypack.Point.x:int" jni_ref _p
      method get_x = fun () -> Java.get "mypack.Point.x:int" jni_ref
      method _get_jni_jPoint = jni_ref
    end
and _capsule_jColored (jni_ref : _jni_jColored) =
  let _ =
    if Java.is_null jni_ref then raise (Null_object "mypack/Colored") else ()
  in
    object (self)
      method setColor =
        fun _p0 ->
          let _p0 = JavaString.of_string _p0
          in
            Java.call "mypack.Colored.setColor(java.lang.String):void"
              jni_ref _p0
      method getColor =
        fun () ->
          JavaString.to_string
            (Java.call "mypack.Colored.getColor():java.lang.String" jni_ref
               ())
      method _get_jni_jColored = jni_ref
    end
and _capsule_jColoredPoint (jni_ref : _jni_jColoredPoint) =
  let _ =
    if Java.is_null jni_ref
    then raise (Null_object "mypack/ColoredPoint")
    else ()
  in
    object (self)
      method eq_colored_point =
        fun (_p0 : jColoredPoint) ->
          let _p0 = _p0#_get_jni_jColoredPoint
          in
            Java.call "mypack.ColoredPoint.eq(mypack.ColoredPoint):boolean"
              jni_ref _p0
      method setColor =
        fun _p0 ->
          let _p0 = JavaString.of_string _p0
          in
            Java.call "mypack.Colored.setColor(java.lang.String):void"
              jni_ref _p0
      method getColor =
        fun () ->
          JavaString.to_string
            (Java.call "mypack.Colored.getColor():java.lang.String" jni_ref
               ())
      method eq =
        fun (_p0 : jPoint) ->
          let _p0 = _p0#_get_jni_jPoint
          in Java.call "mypack.Point.eq(mypack.Point):boolean" jni_ref _p0
      method distance =
        fun () ->
          Int64.to_int (Java.call "mypack.Point.distance():long" jni_ref ())
      method display =
        fun _p0 ->
          let _p0 = Int64.of_int _p0
          in Java.call "mypack.Point.display(long):int" jni_ref _p0
      method toString =
        fun () ->
          JavaString.to_string
            (Java.call "mypack.Point.toString():java.lang.String" jni_ref ())
      method rmoveto =
        fun _p0 _p1 ->
          let _p1 = Int32.of_int _p1 in
          let _p0 = Int32.of_int _p0
          in Java.call "mypack.Point.rmoveto(int,int):void" jni_ref _p0 _p1
      method moveto =
        fun _p0 _p1 ->
          let _p1 = Int32.of_int _p1 in
          let _p0 = Int32.of_int _p0
          in Java.call "mypack.Point.moveto(int,int):void" jni_ref _p0 _p1
      method set_y =
        fun _p ->
          let _p = Int32.of_int _p
          in Java.set "mypack.Point.y:int" jni_ref _p
      method get_y = fun () -> Java.get "mypack.Point.y:int" jni_ref
      method set_x =
        fun _p ->
          let _p = Int32.of_int _p
          in Java.set "mypack.Point.x:int" jni_ref _p
      method get_x = fun () -> Java.get "mypack.Point.x:int" jni_ref
      method _get_jni_jColoredPoint = jni_ref
      method _get_jni_jPoint = (jni_ref :> _jni_jPoint)
      method _get_jni_jColored = (jni_ref :> _jni_jColored)
    end;;
let jPoint_of_top (o : JniHierarchy.top) : jPoint =
  new _capsule_jPoint (__jni_jPoint_of_jni_obj o#_get_jniobj);;
let jColored_of_top (o : JniHierarchy.top) : jColored =
  new _capsule_jColored (__jni_jColored_of_jni_obj o#_get_jniobj);;
let jColoredPoint_of_top (o : JniHierarchy.top) : jColoredPoint =
  new _capsule_jColoredPoint (__jni_jColoredPoint_of_jni_obj o#_get_jniobj);;
let _instance_of_jPoint =
  let clazz = Jni.find_class "mypack/Point"
  in fun (o : JniHierarchy.top) -> Jni.is_instance_of o#_get_jniobj clazz;;
let _instance_of_jColored =
  let clazz = Jni.find_class "mypack/Colored"
  in fun (o : JniHierarchy.top) -> Jni.is_instance_of o#_get_jniobj clazz;;
let _instance_of_jColoredPoint =
  let clazz = Jni.find_class "mypack/ColoredPoint"
  in fun (o : JniHierarchy.top) -> Jni.is_instance_of o#_get_jniobj clazz;;
let _new_jArray_jPoint size =
  let java_obj = Jni.new_object_array size (Jni.find_class "mypack/Point")
  in
    new JniArray._Array Jni.get_object_array_element Jni.
      set_object_array_element (fun jniobj -> new _capsule_jPoint jniobj)
      (fun obj -> obj#_get_jni_jPoint) java_obj;;
let jArray_init_jPoint size f =
  let a = _new_jArray_jPoint size
  in (for i = 0 to pred size do a#set i (f i) done; a);;
let _new_jArray_jColored size =
  let java_obj = Jni.new_object_array size (Jni.find_class "mypack/Colored")
  in
    new JniArray._Array Jni.get_object_array_element Jni.
      set_object_array_element (fun jniobj -> new _capsule_jColored jniobj)
      (fun obj -> obj#_get_jni_jColored) java_obj;;
let jArray_init_jColored size f =
  let a = _new_jArray_jColored size
  in (for i = 0 to pred size do a#set i (f i) done; a);;
let _new_jArray_jColoredPoint size =
  let java_obj =
    Jni.new_object_array size (Jni.find_class "mypack/ColoredPoint")
  in
    new JniArray._Array Jni.get_object_array_element Jni.
      set_object_array_element
      (fun jniobj -> new _capsule_jColoredPoint jniobj)
      (fun obj -> obj#_get_jni_jColoredPoint) java_obj;;
let jArray_init_jColoredPoint size f =
  let a = _new_jArray_jColoredPoint size
  in (for i = 0 to pred size do a#set i (f i) done; a);;
let _init_point =
  let clazz = Jni.find_class "mypack/Point" in
  let id =
    try Jni.get_methodID clazz "<init>" "(int,int):void"
    with
    | _ ->
        failwith
          "Unknown constructor from IDL in class \"mypack.Point\" : \"Point(int,int)\"."
  in
    fun (java_obj : _jni_jPoint) _p0 _p1 ->
      let _p1 = Int32.of_int _p1 in
      let _p0 = Int32.of_int _p0
      in
        Jni.call_nonvirtual_void_method java_obj clazz id
          [| Camlint _p0; Camlint _p1 |];;
let _init_default_point =
  let clazz = Jni.find_class "mypack/Point" in
  let id =
    try Jni.get_methodID clazz "<init>" "():void"
    with
    | _ ->
        failwith
          "Unknown constructor from IDL in class \"mypack.Point\" : \"Point()\"."
  in
    fun (java_obj : _jni_jPoint) ->
      Jni.call_nonvirtual_void_method java_obj clazz id [|  |];;
let _init_colored_point =
  let clazz = Jni.find_class "mypack/ColoredPoint" in
  let id =
    try Jni.get_methodID clazz "<init>" "(int,int,java.lang.String):void"
    with
    | _ ->
        failwith
          "Unknown constructor from IDL in class \"mypack.ColoredPoint\" : \"ColoredPoint(int,int,string)\"."
  in
    fun (java_obj : _jni_jColoredPoint) _p0 _p1 _p2 ->
      let _p2 = JavaString.of_string _p2 in
      let _p1 = Int32.of_int _p1 in
      let _p0 = Int32.of_int _p0
      in
        Jni.call_nonvirtual_void_method java_obj clazz id
          [| Camlint _p0; Camlint _p1; Obj _p2 |];;
let _init_default_colored_point =
  let clazz = Jni.find_class "mypack/ColoredPoint" in
  let id =
    try Jni.get_methodID clazz "<init>" "():void"
    with
    | _ ->
        failwith
          "Unknown constructor from IDL in class \"mypack.ColoredPoint\" : \"ColoredPoint()\"."
  in
    fun (java_obj : _jni_jColoredPoint) ->
      Jni.call_nonvirtual_void_method java_obj clazz id [|  |];;

class point _p0 _p1 =
  let java_obj = _alloc_jPoint ()
  in let _ = _init_point java_obj _p0 _p1
    in object (self) inherit _capsule_jPoint java_obj end;;
class default_point () =
  let java_obj = _alloc_jPoint ()
  in let _ = _init_default_point java_obj
    in object (self) inherit _capsule_jPoint java_obj end;;
class colored_point _p0 _p1 _p2 =
  let java_obj = _alloc_jColoredPoint ()
  in let _ = _init_colored_point java_obj _p0 _p1 _p2
    in object (self) inherit _capsule_jColoredPoint java_obj end;;
class default_colored_point () =
  let java_obj = _alloc_jColoredPoint ()
  in let _ = _init_default_colored_point java_obj
    in object (self) inherit _capsule_jColoredPoint java_obj end;;


