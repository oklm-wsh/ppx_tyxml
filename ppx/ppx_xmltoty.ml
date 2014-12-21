open Parsetree
open Ast_helper
open Ast_mapper

let xmltoty_mapper _ = 
  { default_mapper with
    expr =
      fun mapper expr ->
      (match expr with
       | { pexp_desc =
	     Pexp_extension ({txt = "xmltoty"; loc}, pstr)} ->
	  (match pstr with
	  | PStr [{ pstr_desc =
		      Pstr_eval 
			({pexp_loc = loc;
			  pexp_desc = Pexp_constant (Asttypes.Const_string (xml, None))}, _)
		 }] ->
	     Xmltoty.xmltoty (`String (0, xml))
	  | _ -> failwith "Usage : [%xmltoty string]")
       | _ -> default_mapper.expr mapper expr)
	
  }

let () = register "xmltoty" xmltoty_mapper
