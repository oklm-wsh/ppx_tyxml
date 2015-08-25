open Ast_helper
open Pprintast

let string s = Asttypes.Const_string (s, None)

let float s = Asttypes.Const_float s

let int v = Asttypes.Const_int (int_of_string v)

let mkloc pos endpos =
 Location.{
    loc_start = pos;
    loc_end = Lexing.{pos with pos_cnum = pos.pos_cnum + endpos};
    loc_ghost = false
  }

let mklid pos id =
  let lid = Longident.Lident id in
  Location.mkloc lid (mkloc pos (String.length id))

let mkident pos ident =
  let loc = mkloc pos (String.length ident) in
  Exp.ident ~loc (mklid pos ident)

let mkconstant pos ~typ constant =
  let conv = match typ with
    | `String -> string
    | `Float -> float
    | `Int -> int
  in
  let const = conv constant in
  let endpos = String.length constant in
  Exp.constant ~loc:(mkloc pos endpos) const

let mkstring pos s = mkconstant pos ~typ:`String s

let mkfloat pos f = mkconstant pos ~typ:`Float f

let mkint pos n = mkconstant pos ~typ:`Int n

let mkconstruct pos construct param =
  let endpos = String.length construct in
  let loc = mkloc pos endpos in
  Exp.construct ~loc (mklid pos construct) param

let mkvariant pos label param =
  let loc = mkloc pos (String.length label) in
  Exp.variant ~loc label param

let mktuple pos tuple = Exp.tuple ~loc:(mkloc pos 0) tuple

let mkapply pos name labels params =
  let loc = mkloc pos (String.length name) in
  let params = List.map (fun a -> ("", a)) params in
  Exp.apply ~loc (mkident pos name) (labels @ params)

let unit pos = mkconstruct pos "()" None
let true_ pos = mkconstruct pos "true" None
let false_ pos = mkconstruct pos "false" None
let cons pos hd tail = mkconstruct pos "::" (Some (mktuple pos [hd; tail]))
let nil pos = mkconstruct pos "[]" None

let rec mklist pos = function
  | [] -> nil pos
  | x :: xs -> cons pos x (mklist pos xs)
