open Package'java'lang

class point (this : (*Class encapsulée*) ) = 
object (self)
  method this () = this;
end

let p = new point (Java.make "java'lang'Object()" ())
