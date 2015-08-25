open Parsetree
open Ast_helper
open Ast_mapper

let tyxml_mapper _ = 
  { default_mapper with
    expr =
      fun mapper expr ->
      (match expr with
       | { pexp_desc =
	     Pexp_extension ({Asttypes.txt = "tyxml"; _}, pstr)} ->
	  (match pstr with
	  | PStr [{ pstr_desc =
		      Pstr_eval 
			({pexp_loc = {Location.loc_start; _};
			  pexp_desc = Pexp_constant (Asttypes.Const_string (xml, _))}, _)
		 }] ->
	     Xmltoty.xmltoty loc_start (`String (0, xml))
	  | _ -> failwith "Usage : [%tyxml string]")
       | _ -> default_mapper.expr mapper expr)	
  }

let () = register "tyxml" tyxml_mapper
