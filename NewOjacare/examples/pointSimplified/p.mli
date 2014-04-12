type _jni_jPoint;;
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
val jPoint_of_top : JniHierarchy.top -> jPoint;;
val _instance_of_jPoint : JniHierarchy.top -> bool;;
val jArray_init_jPoint : int -> (int -> jPoint) -> jPoint JniArray.jArray;;



