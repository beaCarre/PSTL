type _jni_jPoint = Jni.obj;;
class type jPoint =
  object
    inherit JniHierarchy.top
    method _get_jni_jPoint : _jni_jPoint
    method mVOID : unit -> unit
    method mBOOLEAN : bool -> bool
    method mBYTE : int -> int
    method mCHAR : int -> int
    method mSHORT : int -> int
    method mINT : int -> int
    method mLONG : int64 -> int64
    method mFLOAT : float -> float
    method mDOUBLE : float -> float
    method mSTRING : string -> string
    method mPOINT : jPoint -> jPoint
  end;;
let __jni_obj_of_jni_jPoint (java_obj : _jni_jPoint) =
  (Obj.magic : _jni_jPoint -> Jni.obj) java_obj;;
let __jni_jPoint_of_jni_obj =
  let clazz =
    try Jni.find_class "mypack/Point"
    with | _ -> failwith "Class not found : mypack.Point."
  in
    fun (java_obj : Jni.obj) ->
      if not (Jni.is_instance_of java_obj clazz)
      then failwith "``cast error'' : jPoint (mypack/Point)"
      else (Obj.magic java_obj : _jni_jPoint);;
let _alloc_jPoint =
  let clazz = Jni.find_class "mypack/Point"
  in fun () -> (Jni.alloc_object clazz : _jni_jPoint);;

class _capsule_jPoint =
  let clazz = Jni.find_class "mypack/Point"
  in
    let __mid_mPOINT =
      try Jni.get_methodID clazz "mPOINT" "(Lmypack/Point;)Lmypack/Point;"
      with
      | _ ->
          failwith
            "Unknown method from IDL in class \"mypack.Point\" : \"mypack.Point mPOINT(mypack.Point)\"."
    in
      let __mid_mSTRING =
        try
          Jni.get_methodID clazz "mSTRING"
            "(Ljava/lang/String;)Ljava/lang/String;"
        with
        | _ ->
            failwith
              "Unknown method from IDL in class \"mypack.Point\" : \"string mSTRING(string)\"."
      in
        let __mid_mDOUBLE =
          try Jni.get_methodID clazz "mDOUBLE" "(D)D"
          with
          | _ ->
              failwith
                "Unknown method from IDL in class \"mypack.Point\" : \"double mDOUBLE(double)\"."
        in
          let __mid_mFLOAT =
            try Jni.get_methodID clazz "mFLOAT" "(F)F"
            with
            | _ ->
                failwith
                  "Unknown method from IDL in class \"mypack.Point\" : \"float mFLOAT(float)\"."
          in
            let __mid_mLONG =
              try Jni.get_methodID clazz "mLONG" "(J)J"
              with
              | _ ->
                  failwith
                    "Unknown method from IDL in class \"mypack.Point\" : \"long mLONG(long)\"."
            in
              let __mid_mINT =
                try Jni.get_methodID clazz "mINT" "(I)I"
                with
                | _ ->
                    failwith
                      "Unknown method from IDL in class \"mypack.Point\" : \"int mINT(int)\"."
              in
                let __mid_mSHORT =
                  try Jni.get_methodID clazz "mSHORT" "(S)S"
                  with
                  | _ ->
                      failwith
                        "Unknown method from IDL in class \"mypack.Point\" : \"short mSHORT(short)\"."
                in
                  let __mid_mCHAR =
                    try Jni.get_methodID clazz "mCHAR" "(C)C"
                    with
                    | _ ->
                        failwith
                          "Unknown method from IDL in class \"mypack.Point\" : \"char mCHAR(char)\"."
                  in
                    let __mid_mBYTE =
                      try Jni.get_methodID clazz "mBYTE" "(B)B"
                      with
                      | _ ->
                          failwith
                            "Unknown method from IDL in class \"mypack.Point\" : \"byte mBYTE(byte)\"."
                    in
                      let __mid_mBOOLEAN =
                        try Jni.get_methodID clazz "mBOOLEAN" "(Z)Z"
                        with
                        | _ ->
                            failwith
                              "Unknown method from IDL in class \"mypack.Point\" : \"boolean mBOOLEAN(boolean)\"."
                      in
                        let __mid_mVOID =
                          try Jni.get_methodID clazz "mVOID" "()V"
                          with
                          | _ ->
                              failwith
                                "Unknown method from IDL in class \"mypack.Point\" : \"void mVOID()\"."
                        in
                          fun (jni_ref : _jni_jPoint) ->
                            let _ =
                              if Jni.is_null jni_ref
                              then
                                raise
                                  (JniHierarchy.Null_object "mypack/Point")
                              else ()
                            in
                              object (self)
                                method mPOINT =
                                  fun (_p0 : jPoint) ->
                                    let _p0 = _p0#_get_jni_jPoint
                                    in
                                      (new _capsule_jPoint
                                         (Jni.call_object_method jni_ref
                                            __mid_mPOINT [| Jni.Obj _p0 |]) :
                                        jPoint)
                                method mSTRING =
                                  fun _p0 ->
                                    let _p0 = Jni.string_to_java _p0
                                    in
                                      Jni.string_from_java
                                        (Jni.call_object_method jni_ref
                                           __mid_mSTRING [| Jni.Obj _p0 |])
                                method mDOUBLE =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_double_method jni_ref
                                        __mid_mDOUBLE [| Jni.Double _p0 |]
                                method mFLOAT =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_float_method jni_ref
                                        __mid_mFLOAT [| Jni.Float _p0 |]
                                method mLONG =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_long_method jni_ref
                                        __mid_mLONG [| Jni.Long _p0 |]
                                method mINT =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_camlint_method jni_ref
                                        __mid_mINT [| Jni.Camlint _p0 |]
                                method mSHORT =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_short_method jni_ref
                                        __mid_mSHORT [| Jni.Short _p0 |]
                                method mCHAR =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_char_method jni_ref
                                        __mid_mCHAR [| Jni.Char _p0 |]
                                method mBYTE =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_byte_method jni_ref
                                        __mid_mBYTE [| Jni.Byte _p0 |]
                                method mBOOLEAN =
                                  fun _p0 ->
                                    let _p0 = _p0
                                    in
                                      Jni.call_boolean_method jni_ref
                                        __mid_mBOOLEAN [| Jni.Boolean _p0 |]
                                method mVOID =
                                  fun () ->
                                    Jni.call_void_method jni_ref __mid_mVOID
                                      [|  |]
                                method _get_jni_jPoint = jni_ref
                                inherit JniHierarchy.top jni_ref
                              end;;
let jPoint_of_top (o : JniHierarchy.top) : jPoint =
  new _capsule_jPoint (__jni_jPoint_of_jni_obj o#_get_jniobj);;
let _instance_of_jPoint =
  let clazz = Jni.find_class "mypack/Point"
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





