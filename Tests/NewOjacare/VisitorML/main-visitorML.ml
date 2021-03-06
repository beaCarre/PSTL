open Formule
open Format
let jni : fr'upmc'infop6'mlo'VisiteurML java_instance ref = ref Java.null

let _ =
jni :=
  Java.proxy "fr.upmc.infop6.mlo.VisiteurML" (
    object (self)
      val buf = Buffer.create 80

      method _get_jni_jVisiteur = Obj.magic !jni
      method _get_jni_jVisiteurML = Obj.magic !jni

      method visite_cst (cst = 
	Buffer.add_string buf (if cst#valeur () then "true" else "false")

      method visite_var var = 
	Buffer.add_string buf  (var#ident ())

      method visite_non non = 
	let sf = non#sous_formule () in
	  Buffer.add_string buf "!(";
	  sf#accepte (self :> jVisiteur);
	  Buffer.add_string buf ")"
	    
      method visite_et et = 
	let fg = et#sous_formule_g ()
	and fd = et#sous_formule_d () in
	  Buffer.add_string buf "(";
	  fg#accepte (self :> jVisiteur);
	  Buffer.add_string buf " ^ "; 
	  fd#accepte (self :> jVisiteur);
	  Buffer.add_string buf ")";
	  
      method visite_ou ou = 
	let fg = ou#sous_formule_g ()
	and fd = ou#sous_formule_d () in
	  Buffer.add_string buf "(";
	  fg#accepte (self :> jVisiteur);
	  Buffer.add_string buf " v "; 
	  fd#accepte (self :> jVisiteur);
	  Buffer.add_string buf ")";

      method get_res () =
	let s = Buffer.contents buf in
	  Buffer.reset buf;
	  s

    end)

(*
let visiteur_ml = 

let mainML =
  Java.proxy "fr.upmc.infop6.mlo.MainML" (
    object
      method cree_visiteur () =
	(new visiteur_ml :> jVisiteurML)
    end)

let _ = fr_upmc_infop6_mlo_jMainJava__main ( mainML :> jMainML)
*)
