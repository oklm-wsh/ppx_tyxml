open Ast_helper
open Pprintast

let string s = Asttypes.Const_string (s, None)

let mkloc id =
  let lid = Longident.Lident id in
  Location.mkloc lid !default_loc

let mkident ident = Exp.ident (mkloc ident)

let mkconstant constant = Exp.constant constant

let mkstring s = mkconstant (string s)

let mkconstruct construct param = Exp.construct (mkloc construct) param

let mkvariant label = Exp.variant label None

let mktuple tuple = Exp.tuple tuple

let mkapply name attrs params =
  Exp.apply (mkident name) (attrs @ (List.map (fun a -> ("", a)) params))

let unit = mkconstruct "()" None
let true_ = mkconstruct "true" None
let false_ = mkconstruct "false" None
let cons hd tail = mkconstruct "::" (Some (mktuple [hd; tail]))
let nil = mkconstruct "[]" None

let rec mklist = function
  | [] -> nil
  | x :: xs ->
     cons x (mklist xs)
