open Package'java'lang

class point (this : (*Class encapsul�e*) ) = 
object (self)
  method this () = this;
end

let p = new point (Java.make "java'lang'Object()" ())
