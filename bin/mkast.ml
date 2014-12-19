open Ast_helper
open Pprintast

let string s = Asttypes.Const_string (s, None)

let mkloc id =
  let lid = Longident.Lident id in
  Location.mkloc lid !default_loc

let mkident ident = Exp.ident (mkloc ident)

let mkconstant constant = Exp.constant constant

let mkstring s = mkconstant (string s)

let mkconstruct construct = Exp.construct (mkloc construct) None

let mktuple tuple = Exp.tuple tuple

let mkapply name attrs params =
  Exp.apply (mkident name) (attrs @ (List.map (fun a -> ("", a)) params))

let unit = mkconstruct "()"
let true_ = mkconstruct "true"
let false_ = mkconstruct "false"
let cons = mkconstruct "::"
let nil = mkconstruct "[]"

let rec mklist = function
  | [] -> nil
  | x :: xs ->
     mktuple [x; mktuple [cons; mklist xs]]
