open Ast_helper
open Pprintast

let string s = Asttypes.Const_string (s, None)

let float s = Asttypes.Const_float s

let int v = Asttypes.Const_int v

let mkloc (l, c) =
 Location.{
    loc_start = Lexing.{
		pos_fname = "";
		pos_lnum = l;
		pos_bol = l;
		pos_cnum = c;
	      };
    loc_end = Lexing.{
	      pos_fname = "";
	      pos_lnum = l;
	      pos_bol = l;
	      pos_cnum = c (* + String.length id *);
	    };
    loc_ghost = false
  }

let mklid (l, c) id =
  let lid = Longident.Lident id in
  Location.mkloc lid (mkloc (l, c))

let mkident pos ident = Exp.ident ~loc:(mkloc pos) (mklid pos ident)

let mkconstant pos constant = Exp.constant ~loc:(mkloc pos) constant

let mkstring s = mkconstant (0, 0) (string s)

let mkfloat s = mkconstant (0, 0) (float s)

let mkint v = mkconstant (0, 0) (int v)

let mkconstruct pos construct param = Exp.construct ~loc:(mkloc pos) (mklid pos construct) param

let mkvariant pos label param = Exp.variant ~loc:(mkloc pos) label param

let mktuple pos tuple = Exp.tuple ~loc:(mkloc pos) tuple

let mkapply pos name attrs params =
  Exp.apply ~loc:(mkloc pos) (mkident pos name) (attrs @ (List.map (fun a -> ("", a)) params))

let unit pos = mkconstruct pos "()" None
let true_ pos = mkconstruct pos "true" None
let false_ pos = mkconstruct pos "false" None
let cons pos hd tail = mkconstruct pos "::" (Some (mktuple pos [hd; tail]))
let nil pos = mkconstruct pos "[]" None

let rec mklist pos = function
  | [] -> nil pos
  | x :: xs ->
     cons pos x (mklist pos xs)
