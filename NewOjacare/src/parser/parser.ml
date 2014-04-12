type token =
  | PACKAGE
  | CLASS
  | INTERFACE
  | EXTENDS
  | IMPLEMENTS
  | STATIC
  | ABSTRACT
  | FINAL
  | NAME
  | CALLBACK
  | VOID
  | BOOLEAN
  | BYTE
  | SHORT
  | CAMLINT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | CHAR
  | STRING
  | TOP
  | ARRAY
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | COMMA
  | SEMI
  | COLON
  | DOT
  | INIT
  | IDENT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(*	$Id: parser.mly,v 1.3 2004/03/11 19:51:45 henry Exp $	*)

open Idl
open Syntax_error

(* fonctions de constructions : permets d'associer la position dans le fichier source *)

let make_ident s = {
  id_location = Loc.get ();
  id_desc = s
}

let make_qident p n = {
  qid_location = Loc.get ();
  qid_package = p;
  qid_name = n;
}

let make_type  t = {
  t_location = Loc.get ();
  t_desc = t
}

let make_modifiers desc = {
  mo_location = Loc.get ();
  mo_desc = desc 
}

let make_annotation desc = {
  an_location = Loc.get ();
  an_desc = desc 
}

let make_arg ?(annotations = []) ?ident t = {
  arg_location = Loc.get ();
  arg_type = t ;
  arg_annot = annotations
}

type def_sort =
    Content of content | Init of init

let make_init annotation args = 
  Init 
    {
     i_location = Loc.get ();
     i_annot = annotation; 
     i_args = args 
   }

let make_field ?(modifiers = []) ?(annotations = []) t name =
  List.iter (fun m -> match m.mo_desc with 
    Istatic | Ifinal -> () 
  | d -> raise (Syntax_error (Efield_modifiers m))) modifiers;
  Content (Field 
    {
     f_location = Loc.get ();
     f_modifiers = modifiers;
     f_annot = annotations; 
     f_name = name; 
     f_type = t 
   })

let make_method ?(modifiers = []) ?(annotations = []) rtype name args = 
  List.iter (fun m -> match m.mo_desc with 
    Istatic | Iabstract -> () 
  | d -> raise (Syntax_error (Emethod_modifiers m))) modifiers;
  Content (Method 
    {
     m_location = Loc.get ();
     m_annot = annotations;
     m_modifiers = modifiers;
     m_name = name;
     m_return_type = rtype;
     m_args = args
   })
    
let filter_inits l = List.fold_left (fun l -> (function (Init d) -> d::l | _ -> l)) [] l
let filter_contents l = List.fold_left (fun l -> (function (Content d) -> d::l | _ -> l)) [] l

let make_def ?(modifiers = []) ?(annotations = []) ?(interface = false) name  ?super ?(implements = []) decls = 
  let contents = filter_contents decls
  and inits = filter_inits decls in

(*  let inits = (* ajout init par défaut ... non car conflit de nom *)
   if inits = [] && not interface then
   [{ i_location = Loc.get ();
   i_annot = []; 
   i_args = [] }]
   else inits in *)
  
  if interface then begin
    List.iter (fun m -> match m.mo_desc with 
    | d -> raise (Syntax_error (Einterface_modifiers m))) modifiers;
    if inits != [] then  raise (Syntax_error (Enoinit (List.hd inits)));
    List.iter (fun c -> 
      match c with 
      | Method m -> 
	  if m.m_modifiers != [] then 
	    let m = List.hd m.m_modifiers in
	    raise (Syntax_error (Einterfacemethod_modifiers m))
      | Field f -> 
	  if f.f_modifiers != [] then 
	    let f = List.hd f.f_modifiers in
	    raise (Syntax_error (Einterfacefield_modifiers f))
      ) contents;
  end
  else begin
    List.iter (fun m -> match m.mo_desc with 
      Iabstract -> () 
    | d -> raise (Syntax_error (Eclass_modifiers m))) modifiers
  end;
  {
   d_location = Loc.get ();
   d_super = super;
   d_implements = implements;
   d_interface = interface;
   d_modifiers = modifiers;
   d_annot = annotations;
   d_name = name;
   d_inits = inits;
   d_contents = contents;
 }

let make_package name defs = {
   p_name = name;
   p_defs = defs;
 }

# 172 "parser.ml"
let yytransl_const = [|
  257 (* PACKAGE *);
  258 (* CLASS *);
  259 (* INTERFACE *);
  260 (* EXTENDS *);
  261 (* IMPLEMENTS *);
  262 (* STATIC *);
  263 (* ABSTRACT *);
  264 (* FINAL *);
  265 (* NAME *);
  266 (* CALLBACK *);
  267 (* VOID *);
  268 (* BOOLEAN *);
  269 (* BYTE *);
  270 (* SHORT *);
  271 (* CAMLINT *);
  272 (* INT *);
  273 (* LONG *);
  274 (* FLOAT *);
  275 (* DOUBLE *);
  276 (* CHAR *);
  277 (* STRING *);
  278 (* TOP *);
  279 (* ARRAY *);
  280 (* LBRACKET *);
  281 (* RBRACKET *);
  282 (* LBRACE *);
  283 (* RBRACE *);
  284 (* LPAREN *);
  285 (* RPAREN *);
  286 (* COMMA *);
  287 (* SEMI *);
  288 (* COLON *);
  289 (* DOT *);
  290 (* INIT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\010\000\007\000\007\000\013\000\013\000\014\000\
\014\000\014\000\015\000\015\000\015\000\015\000\016\000\016\000\
\016\000\016\000\017\000\017\000\019\000\019\000\020\000\020\000\
\021\000\021\000\021\000\021\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\009\000\009\000\022\000\022\000\022\000\008\000\023\000\
\023\000\024\000\024\000\024\000\006\000\004\000\004\000\012\000\
\012\000\011\000\011\000\000\000"

let yylen = "\002\000\
\002\000\002\000\004\000\005\000\001\000\002\000\003\000\004\000\
\004\000\005\000\004\000\005\000\005\000\006\000\005\000\006\000\
\006\000\007\000\006\000\007\000\007\000\008\000\003\000\004\000\
\005\000\006\000\002\000\003\000\002\000\002\000\003\000\001\000\
\001\000\001\000\002\000\003\000\003\000\004\000\003\000\004\000\
\004\000\005\000\003\000\002\000\002\000\003\000\001\000\003\000\
\001\000\002\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\001\000\001\000\002\000\001\000\001\000\001\000\003\000\001\000\
\003\000\002\000\001\000\001\000\001\000\001\000\003\000\001\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\068\000\069\000\070\000\
\000\000\084\000\000\000\000\000\000\000\000\000\000\000\000\000\
\078\000\000\000\077\000\000\000\000\000\000\000\075\000\076\000\
\000\000\000\000\001\000\002\000\006\000\000\000\000\000\000\000\
\000\000\067\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\000\000\023\000\074\000\071\000\000\000\000\000\000\000\
\000\000\000\000\000\000\079\000\000\000\000\000\080\000\027\000\
\000\000\000\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\029\000\000\000\000\000\
\000\000\065\000\000\000\000\000\032\000\033\000\034\000\000\000\
\000\000\011\000\000\000\073\000\000\000\008\000\000\000\000\000\
\024\000\000\000\000\000\009\000\000\000\004\000\000\000\015\000\
\000\000\000\000\044\000\000\000\000\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\012\000\
\000\000\000\000\010\000\000\000\000\000\000\000\013\000\000\000\
\081\000\083\000\045\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\031\000\064\000\039\000\019\000\016\000\
\000\000\026\000\000\000\000\000\014\000\017\000\000\000\000\000\
\051\000\046\000\000\000\000\000\040\000\041\000\020\000\018\000\
\000\000\021\000\052\000\048\000\042\000\022\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\054\000\013\000\055\000\040\000\014\000\
\015\000\041\000\057\000\058\000\075\000\076\000\077\000\078\000\
\079\000\080\000\099\000\126\000\127\000\016\000\025\000\026\000"

let yysindex = "\018\000\
\064\000\000\000\232\254\020\255\020\255\000\000\000\000\000\000\
\000\255\000\000\045\000\080\000\030\255\010\255\101\255\118\255\
\000\000\097\255\000\000\017\255\062\255\020\255\000\000\000\000\
\081\255\079\255\000\000\000\000\000\000\020\255\020\255\114\255\
\020\255\000\000\030\255\078\255\086\255\086\255\181\255\000\000\
\064\255\086\255\000\000\000\000\000\000\000\255\059\255\091\255\
\020\255\066\255\121\255\000\000\000\000\061\255\000\000\000\000\
\103\255\102\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\130\255\231\255\
\042\000\000\000\135\255\128\255\000\000\000\000\000\000\246\254\
\086\255\000\000\103\255\000\000\086\255\000\000\092\255\086\255\
\000\000\069\255\086\255\000\000\093\255\000\000\133\255\000\000\
\086\255\002\000\000\000\130\255\042\000\246\254\246\254\000\000\
\206\255\138\255\130\255\103\255\000\000\103\255\086\255\000\000\
\103\255\086\255\000\000\094\255\103\255\086\255\000\000\000\000\
\000\000\000\000\000\000\042\000\246\254\136\255\139\255\000\000\
\246\254\130\255\130\255\000\000\000\000\000\000\000\000\000\000\
\103\255\000\000\103\255\086\255\000\000\000\000\103\255\246\254\
\000\000\000\000\022\000\130\255\000\000\000\000\000\000\000\000\
\103\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\132\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\145\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\171\000\000\000\131\255\000\000\000\000\000\000\
\000\000\146\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\147\255\000\000\142\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\151\255\
\000\000\000\000\000\000\000\000\109\255\000\000\149\255\000\000\
\000\000\148\255\152\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\111\255\
\000\000\000\000\000\000\154\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\125\000\017\000\179\000\000\000\254\255\250\255\217\255\
\010\000\214\255\221\255\223\255\085\000\000\000\000\000\000\000\
\000\000\244\255\183\255\044\000\000\000\000\000\150\000\000\000"

let yytablesize = 344
let yytable = "\072\000\
\005\000\020\000\021\000\056\000\087\000\074\000\083\000\093\000\
\022\000\023\000\017\000\030\000\031\000\106\000\043\000\006\000\
\007\000\008\000\001\000\044\000\037\000\038\000\024\000\032\000\
\019\000\034\000\128\000\047\000\048\000\029\000\050\000\004\000\
\005\000\134\000\082\000\006\000\007\000\008\000\074\000\074\000\
\086\000\089\000\039\000\092\000\027\000\108\000\090\000\116\000\
\073\000\110\000\096\000\051\000\113\000\009\000\019\000\117\000\
\149\000\150\000\124\000\102\000\103\000\122\000\037\000\085\000\
\074\000\072\000\042\000\074\000\081\000\037\000\091\000\074\000\
\037\000\114\000\157\000\137\000\109\000\107\000\139\000\028\000\
\112\000\101\000\143\000\115\000\039\000\125\000\119\000\039\000\
\129\000\039\000\074\000\039\000\121\000\095\000\039\000\088\000\
\111\000\118\000\140\000\130\000\131\000\135\000\033\000\136\000\
\153\000\045\000\138\000\124\000\046\000\141\000\142\000\144\000\
\052\000\074\000\073\000\049\000\039\000\039\000\039\000\039\000\
\053\000\003\000\145\000\006\000\007\000\008\000\148\000\035\000\
\039\000\036\000\151\000\097\000\152\000\066\000\125\000\077\000\
\154\000\049\000\049\000\050\000\050\000\155\000\066\000\066\000\
\066\000\066\000\158\000\066\000\066\000\066\000\066\000\066\000\
\066\000\066\000\077\000\077\000\077\000\098\000\105\000\077\000\
\077\000\104\000\133\000\078\000\146\000\077\000\066\000\120\000\
\147\000\072\000\003\000\082\000\035\000\030\000\077\000\094\000\
\077\000\047\000\036\000\077\000\077\000\018\000\037\000\079\000\
\038\000\077\000\006\000\007\000\008\000\132\000\156\000\059\000\
\060\000\061\000\062\000\084\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\000\000\009\000\000\000\000\000\070\000\
\000\000\000\000\000\000\006\000\007\000\008\000\071\000\053\000\
\059\000\060\000\061\000\062\000\000\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\007\000\008\000\071\000\
\053\000\059\000\060\000\061\000\062\000\000\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\100\000\053\000\000\000\000\000\059\000\060\000\061\000\062\000\
\000\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\000\000\009\000\000\000\000\000\000\000\000\000\123\000\000\000\
\059\000\060\000\061\000\062\000\053\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\053\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\003\000\004\000\005\000\000\000\000\000\006\000\007\000\008\000\
\000\000\000\000\000\000\000\000\053\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000"

let yycheck = "\039\000\
\000\000\004\000\005\000\037\000\047\000\039\000\042\000\050\000\
\009\001\010\001\035\001\002\001\003\001\024\001\021\000\006\001\
\007\001\008\001\001\000\022\000\004\001\005\001\023\001\014\000\
\035\001\016\000\100\000\030\000\031\000\013\000\033\000\002\001\
\003\001\107\000\041\000\006\001\007\001\008\001\072\000\073\000\
\047\000\048\000\026\001\050\000\000\000\081\000\049\000\090\000\
\039\000\085\000\057\000\035\000\088\000\024\001\035\001\091\000\
\130\000\131\000\098\000\072\000\073\000\097\000\004\001\005\001\
\098\000\105\000\005\001\101\000\005\001\004\001\005\001\105\000\
\004\001\005\001\148\000\111\000\083\000\080\000\114\000\000\000\
\087\000\072\000\118\000\090\000\026\001\098\000\093\000\026\001\
\101\000\026\001\124\000\026\001\095\000\033\001\026\001\005\001\
\005\001\005\001\005\001\102\000\103\000\108\000\002\001\110\000\
\140\000\025\001\113\000\147\000\030\001\116\000\117\000\124\000\
\035\001\147\000\105\000\002\001\026\001\026\001\026\001\026\001\
\035\001\001\001\125\000\006\001\007\001\008\001\129\000\031\001\
\026\001\033\001\137\000\030\001\139\000\002\001\147\000\005\001\
\143\000\029\001\030\001\029\001\030\001\144\000\011\001\012\001\
\013\001\014\001\153\000\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\024\001\005\001\026\001\028\001\031\001\029\001\
\030\001\027\001\025\001\033\001\029\001\035\001\035\001\035\001\
\030\001\025\001\000\000\026\001\031\001\027\001\024\001\051\000\
\026\001\029\001\031\001\029\001\030\001\003\000\031\001\033\001\
\031\001\035\001\006\001\007\001\008\001\105\000\147\000\011\001\
\012\001\013\001\014\001\046\000\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\255\255\255\255\027\001\
\255\255\255\255\255\255\006\001\007\001\008\001\034\001\035\001\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\255\255\255\255\
\255\255\255\255\255\255\255\255\006\001\007\001\008\001\034\001\
\035\001\011\001\012\001\013\001\014\001\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\255\255\255\255\255\255\
\034\001\035\001\255\255\255\255\011\001\012\001\013\001\014\001\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\255\255\255\255\255\255\255\255\029\001\255\255\
\011\001\012\001\013\001\014\001\035\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\255\255\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\035\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\001\001\002\001\003\001\255\255\255\255\006\001\007\001\008\001\
\255\255\255\255\255\255\255\255\035\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\024\001"

let yynames_const = "\
  PACKAGE\000\
  CLASS\000\
  INTERFACE\000\
  EXTENDS\000\
  IMPLEMENTS\000\
  STATIC\000\
  ABSTRACT\000\
  FINAL\000\
  NAME\000\
  CALLBACK\000\
  VOID\000\
  BOOLEAN\000\
  BYTE\000\
  SHORT\000\
  CAMLINT\000\
  INT\000\
  LONG\000\
  FLOAT\000\
  DOUBLE\000\
  CHAR\000\
  STRING\000\
  TOP\000\
  ARRAY\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  SEMI\000\
  COLON\000\
  DOT\000\
  INIT\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'idl_file_list) in
    Obj.repr(
# 164 "parser.mly"
                    ( _1 )
# 457 "parser.ml"
               : Idl.file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'defs) in
    Obj.repr(
# 165 "parser.mly"
           ( [make_package [] _1] )
# 464 "parser.ml"
               : Idl.file))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 169 "parser.mly"
                               ( [make_package _2 _4] )
# 472 "parser.ml"
               : 'idl_file_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'defs) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'idl_file_list) in
    Obj.repr(
# 170 "parser.mly"
                                             ( (make_package _2 _4) :: _5 )
# 481 "parser.ml"
               : 'idl_file_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'def) in
    Obj.repr(
# 173 "parser.mly"
      ( [_1] )
# 488 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 174 "parser.mly"
           ( _1 :: _2 )
# 496 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 178 "parser.mly"
                       ( make_def _2 _3 )
# 504 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 179 "parser.mly"
                                   ( make_def ~annotations:_1 _3 _4 )
# 513 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'modifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 180 "parser.mly"
                                 ( make_def  ~modifiers:_1 _3 _4 )
# 522 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'modifiers) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 181 "parser.mly"
                                             ( make_def ~modifiers:_2 ~annotations:_1 _4 _5 )
# 532 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'super) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 182 "parser.mly"
                             ( make_def _2 ~super:_3 _4 )
# 541 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'super) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 183 "parser.mly"
                                         ( make_def ~annotations:_1 _3 ~super:_4 _5 )
# 551 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'modifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'super) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 184 "parser.mly"
                                       ( make_def  ~modifiers:_1 _3 ~super:_4 _5 )
# 561 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'modifiers) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'super) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 185 "parser.mly"
                                                   ( make_def ~modifiers:_2 ~annotations:_1 _4 ~super:_5 _6 )
# 572 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 187 "parser.mly"
                                                 ( make_def _2 ~implements:_4 _5 )
# 581 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 188 "parser.mly"
                                                             ( make_def ~annotations:_1 _3 ~implements:_5 _6 )
# 591 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'modifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 189 "parser.mly"
                                                           ( make_def  ~modifiers:_1 _3 ~implements:_5 _6 )
# 601 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'modifiers) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 190 "parser.mly"
                                                                       ( make_def ~modifiers:_2 ~annotations:_1 _4 ~implements:_6 _7 )
# 612 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'super) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 191 "parser.mly"
                                                       ( make_def _2 ~super:_3 ~implements:_5 _6 )
# 622 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'super) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 192 "parser.mly"
                                                                   ( make_def ~annotations:_1 _3 ~super:_4 ~implements:_6 _7 )
# 633 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'modifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'super) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 193 "parser.mly"
                                                                 ( make_def  ~modifiers:_1 _3 ~super:_4 ~implements:_6 _7 )
# 644 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'modifiers) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'super) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 194 "parser.mly"
                                                                             ( make_def ~modifiers:_2 ~annotations:_1 _4 ~super:_5 ~implements:_7 _8 )
# 656 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 196 "parser.mly"
                           ( make_def ~interface:true _2 _3 )
# 664 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 197 "parser.mly"
                                       ( make_def ~annotations:_1 ~interface:true _3 _4 )
# 673 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 199 "parser.mly"
                                                     ( make_def ~interface:true _2 ~implements:_4 _5 )
# 682 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'interface_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 200 "parser.mly"
                                                                 ( make_def ~annotations:_1 ~interface:true _3 ~implements:_5 _6 )
# 692 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'qident) in
    Obj.repr(
# 204 "parser.mly"
                 ( _2 )
# 699 "parser.ml"
               : 'super))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 207 "parser.mly"
                      ( _2 )
# 706 "parser.ml"
               : 'def_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 208 "parser.mly"
                ( [] )
# 712 "parser.ml"
               : 'def_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 212 "parser.mly"
            ( [_1] )
# 719 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 213 "parser.mly"
                  ( _1 :: _3 )
# 727 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 217 "parser.mly"
         ( _1 )
# 734 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'methods) in
    Obj.repr(
# 218 "parser.mly"
          ( _1 )
# 741 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'inits) in
    Obj.repr(
# 219 "parser.mly"
        ( _1 )
# 748 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 223 "parser.mly"
            ( make_field _1 _2 )
# 756 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 224 "parser.mly"
                        ( make_field ~annotations:_1 _2 _3 )
# 765 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modifiers) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 225 "parser.mly"
                      ( make_field ~modifiers:_1 _2 _3 )
# 774 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'modifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 226 "parser.mly"
                                  ( make_field ~modifiers:_2 ~annotations:_1 _3 _4 )
# 784 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 230 "parser.mly"
      ( make_method _1 _2 _3 )
# 793 "parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 232 "parser.mly"
      ( make_method ~annotations:_1 _2 _3 _4 )
# 803 "parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'modifiers) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 234 "parser.mly"
      ( make_method ~modifiers:_1 _2 _3 _4 )
# 813 "parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'modifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 236 "parser.mly"
      ( make_method ~modifiers:_2 ~annotations:_1 _3 _4 _5 )
# 824 "parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'annotations) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 239 "parser.mly"
                        ( make_init _1 _3 )
# 832 "parser.ml"
               : 'inits))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 240 "parser.mly"
            ( raise (Syntax_error (Einit_no_alias (Loc.get ()))) )
# 839 "parser.ml"
               : 'inits))
; (fun __caml_parser_env ->
    Obj.repr(
# 244 "parser.mly"
                ( [] )
# 845 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arg_list) in
    Obj.repr(
# 245 "parser.mly"
                         ( _2 )
# 852 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 249 "parser.mly"
      ( [_1] )
# 859 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 250 "parser.mly"
                     ( _1 :: _3 )
# 867 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 254 "parser.mly"
      ( make_arg _1 )
# 874 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 255 "parser.mly"
                  ( make_arg ~annotations:_1 _2)
# 882 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 256 "parser.mly"
            ( make_arg ~ident:_2 _1 )
# 890 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'annotations) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 257 "parser.mly"
                        ( make_arg ~annotations:_1 ~ident:_3 _2 )
# 899 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 261 "parser.mly"
       ( make_type Ivoid )
# 905 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 262 "parser.mly"
           ( make_type Iboolean )
# 911 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 263 "parser.mly"
        ( make_type Ibyte )
# 917 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 264 "parser.mly"
         ( make_type Ishort )
# 923 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 265 "parser.mly"
       ( make_type Icamlint )
# 929 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 266 "parser.mly"
        ( make_type Ilong )
# 935 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 267 "parser.mly"
         ( make_type Ifloat )
# 941 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 268 "parser.mly"
          ( make_type Idouble )
# 947 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 269 "parser.mly"
       ( make_type Ichar )
# 953 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 270 "parser.mly"
         ( make_type Istring )
# 959 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 271 "parser.mly"
      ( make_type Itop )
# 965 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    Obj.repr(
# 272 "parser.mly"
                        ( make_type (Iarray _1) )
# 972 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qident) in
    Obj.repr(
# 273 "parser.mly"
         ( make_type (Iobject _1) )
# 979 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modifier) in
    Obj.repr(
# 277 "parser.mly"
           ( [_1] )
# 986 "parser.ml"
               : 'modifiers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modifiers) in
    Obj.repr(
# 278 "parser.mly"
                     ( _1::_2 )
# 994 "parser.ml"
               : 'modifiers))
; (fun __caml_parser_env ->
    Obj.repr(
# 281 "parser.mly"
         ( make_modifiers Istatic )
# 1000 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 282 "parser.mly"
           ( make_modifiers Iabstract )
# 1006 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 283 "parser.mly"
         ( make_modifiers Ifinal )
# 1012 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ann_list) in
    Obj.repr(
# 287 "parser.mly"
                             ( _2 )
# 1019 "parser.ml"
               : 'annotations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ann) in
    Obj.repr(
# 291 "parser.mly"
      ( [_1] )
# 1026 "parser.ml"
               : 'ann_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ann) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ann_list) in
    Obj.repr(
# 292 "parser.mly"
                     ( _1 :: _3 )
# 1034 "parser.ml"
               : 'ann_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 296 "parser.mly"
             ( make_annotation (Iname _2) )
# 1041 "parser.ml"
               : 'ann))
; (fun __caml_parser_env ->
    Obj.repr(
# 297 "parser.mly"
           ( make_annotation Icallback )
# 1047 "parser.ml"
               : 'ann))
; (fun __caml_parser_env ->
    Obj.repr(
# 298 "parser.mly"
        ( make_annotation (Icamlarray) )
# 1053 "parser.ml"
               : 'ann))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 302 "parser.mly"
        ( make_ident _1 )
# 1060 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 306 "parser.mly"
        ( [_1] )
# 1067 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 307 "parser.mly"
                       ( _3 :: _1 )
# 1075 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 311 "parser.mly"
        ( make_qident [] _1 )
# 1082 "parser.ml"
               : 'qident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 312 "parser.mly"
                       ( make_qident _1 _3 )
# 1090 "parser.ml"
               : 'qident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qident) in
    Obj.repr(
# 316 "parser.mly"
         ( [_1] )
# 1097 "parser.ml"
               : 'interface_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'interface_list) in
    Obj.repr(
# 317 "parser.mly"
                              ( _1 :: _3 )
# 1105 "parser.ml"
               : 'interface_list))
(* Entry idl_file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let idl_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Idl.file)
