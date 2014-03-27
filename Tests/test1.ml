open Package'java'lang

class point (this : java'lang'Object java_instance) = 
object (self)
  method this_object () = this;
  method displayP () = print_endline "point"
end

let p = new point (Java.make "java'lang'Object()" ())

class pc (this : java'lang'String java_instance) =
object (self)
  inherit point (this:>java'lang'Object java_instance)
  (*method  this_object () = this;*)
  method display () = print_endline "pc"
end

let _ =
  let p_c = new pc (Java.make "java'lang'String(java.lang.String)" (JavaString.of_string "blop")) in
  p_c#display() ;
  p_c#displayP()
