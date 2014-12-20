(*open PPrint*)
open Attr
open Mkast

type t = [
  | `El of Xmlm.tag * t list
  | `Data of string
]

let tag_name (((_, name), _) : Xmlm.tag) = name

let is_el = function
  | `El _ -> true
  | `Data _ -> false

let is_data d = not (is_el d)

let tag_name_is name = function
  | `El (tag, _) -> tag_name tag = name
  | _ -> false

let extract_els name =
  List.partition 
    (function `El (e, _) -> tag_name e = name
            |  _ -> false)

let extract_el name l =
  match extract_els name l with
  | [el], l -> el, l
  | _ -> failwith ("Must have only one " ^ name)

let rec xml_to_ml = function
  | `El (tag, childs) ->
    tag_to_ml tag childs
  | `Data s ->
    (*string ("pcdata \"" ^ s ^ "\"")*)
     mkapply "pcdata" [] [mkstring s]

and tag_to_ml ((_, name), attrs) childs =
  (*let _ = List.map (attr_to_ml name) attrs in*)
  let fun_to_ml =
    match name with
(*    | "html" -> html_to_ml
    | "head" -> head_to_ml
    | "link" -> link_to_ml
    | "img" -> img_to_ml
    | "svg" -> svg_to_ml
    | "bdo" -> bdo_to_ml
    | "figure" -> figure_to_ml
    | "object" -> object_to_ml
    | "audio" -> audio_to_ml
    | "video" -> video_to_ml
    | "area" -> area_to_ml
    | "table" -> table_to_ml
    | "fieldset" -> fieldset_to_ml
    | "datalist" -> datalist_to_ml
    | "optgroup" -> optgroup_to_ml
    | "command" -> command_to_ml
    | "menu" -> menu_to_ml*)
    | "base"
    | "hr"
    | "wbr"
    | "br"
    | "param"
    | "embed"
    | "source"
    | "col"
    | "input"
    | "keygen"
    | "meta" -> nullary_to_ml name
    | "title"
    | "option"
    | "textarea"
    | "script" -> unary_to_ml name
    | "body" 
    | "footer"
    | "header"
    | "section"
    | "nav"
    | "h1"
    | "h2"
    | "h3"
    | "h4"
    | "h5"
    | "h6"
    | "hgroup"
    | "address"
    | "article"
    | "aside"
    | "p"
    | "pre"
    | "blockquote"
    | "div"
    | "dl"
    | "ol"
    | "ul"
    | "dd"
    | "dt"
    | "li"
    | "figcaption"
    | "rt"
    | "rp"
    | "ruby"
    | "b"
    | "i"
    | "u"
    | "small"
    | "sub"
    | "sup"
    | "mark"
    | "abbr"
    | "cite"
    | "code"
    | "dfn"
    | "em"
    | "kbd"
    | "q"
    | "samp"
    | "span"
    | "strong"
    | "time"
    | "var"
    | "a"
    | "del"
    | "ins"
    | "iframe"
    | "canvas"
    | "map"
    | "caption"
    | "colgroup"
    | "thead"
    | "tbody"
    | "tfoot"
    | "td"
    | "th"
    | "tr"
    | "form"
    | "legend"
    | "label"
    | "button"
    | "select"
    | "progress"
    | "meter"
    | "summary"
    | "noscript"
    | "style" -> star_to_ml name
    | "output" -> star_to_ml "output_elt"
    | _ -> failwith ("Unknown tag " ^ name ^ ".")
  in fun_to_ml attrs childs

and childs_to_ml childs = (*function
  | [] -> string "[]"
  | childs ->  
     let ml_childs = List.map xml_to_ml childs in 
     OCaml.list (fun x -> x) ml_childs*)
  mklist (List.map xml_to_ml childs)

and nullary_to_ml name attrs = function
  | [] ->
     mkapply name [("a", attrs_to_ml name attrs)] [unit]
  | _ -> failwith "Must not have childs"

and unary_to_ml name attrs = function
  | [x] ->
     mkapply name [("a", attrs_to_ml name attrs)] [xml_to_ml x]
  | _ -> failwith "Must have only one childs"

and star_to_ml name attrs childs =
  mkapply name [("a", attrs_to_ml name attrs)] [childs_to_ml childs]
		      
(*and nullary_to_ml name attrs = function
  | [] ->
    string name ^^ space 
    ^^ attrs_to_ml name attrs ^^ space ^^ string "()"
  | _ -> failwith "Must not have childs."*)

(*and unary_to_ml name attrs = function
  | [x] ->
    string name ^^ space 
    ^^ attrs_to_ml name attrs ^^ space
    ^^ parens (xml_to_ml x)
  | _ -> failwith (name ^ " Must have only one child.")*)

(*and star_to_ml name attrs childs =
  string name ^^ space ^^ 
    attrs_to_ml name attrs ^^ space 
    ^^ childs_to_ml childs*)

(*		    
and html_to_ml attrs childs =
  let head, childs = extract_el "head" childs in
  let body, childs = extract_el "body" childs in
  if List.length childs = 0 then
    string "html" ^^ space ^^ attrs_to_ml "html" attrs 
    ^^ space ^^ parens (xml_to_ml head) ^^ space ^^ 
      space ^^ parens (xml_to_ml body)
  else
    failwith "<html> must only have <head> and <body> as childs"

and head_to_ml attrs childs = 
  let title, childs = extract_el "title" childs in
  string "head" ^^ space ^^ attrs_to_ml "head" attrs ^^ space 
  ^^ parens (xml_to_ml title) ^^ space
  ^^ childs_to_ml childs

and link_to_ml attrs = function
  | [] ->
    let rel, attrs = extract_attr "rel" attrs in
    let href, attrs = extract_attr "href" attrs in
    let rel = param_attr_to_ml (attr_to_ml "link" rel) in
    let href = param_attr_to_ml (attr_to_ml "link" href) in
    string "link" ^^ space ^^ rel ^^ space ^^ href ^^ space
    ^^ attrs_to_ml "link" attrs ^^ space ^^ string "()"
  | _ -> failwith "Must not have childs"

and img_to_ml attrs = function
  | [] ->
    let src, attrs = extract_attr "src" attrs in
    let alt, attrs = extract_attr "alt" attrs in
    let src = param_attr_to_ml (attr_to_ml "img" src) in
    let alt = param_attr_to_ml (attr_to_ml "img" alt) in
    string "img" ^^ space ^^ src ^^ space ^^ alt ^^ space
    ^^ attrs_to_ml "link" attrs ^^ space ^^ string "()"
  | _ -> failwith "Must not have childs"

and svg_to_ml attrs childs =
  let xmlns, attrs = extract_opt_attr "xmlns" attrs in
  let xmlns = opt_attr_to_ml "svg" xmlns
  in
  string "svg" ^^ space ^^ xmlns ^^ space ^^ attrs_to_ml "svg" attrs
  ^^ space ^^ childs_to_ml childs

and bdo_to_ml attrs childs =
  let dir, attrs = extract_attr "dir" attrs in
  let dir = param_attr_to_ml (attr_to_ml "bdo" dir) in
  string "bdo" ^^ dir ^^ space ^^ attrs_to_ml "bdo" attrs
  ^^ space ^^ childs_to_ml childs

and figure_to_ml attrs childs = assert false

and object_to_ml attrs childs = assert false

and multimedia_to_ml name attrs childs =
  let src, attrs = extract_opt_attr "src" attrs in
  let srcs, childs = extract_els "source" childs in
  let src = opt_attr_to_ml name src in
  string name ^^ space ^^ src ^^ space ^^ string "~srcs:" 
  ^^ childs_to_ml srcs ^^ space^^ attrs_to_ml name attrs ^^ space
  ^^ childs_to_ml childs

and audio_to_ml attrs childs =
  multimedia_to_ml "audio" attrs childs

and video_to_ml attrs childs =
  multimedia_to_ml "video" attrs childs

and area_to_ml attrs childs =
  let alt, attrs = extract_attr "alt" attrs in
  let alt = param_attr_to_ml (attr_to_ml "area" alt) in
  string "area" ^^ space ^^ alt ^^ space ^^ attrs_to_ml "area" attrs
  ^^ space ^^ childs_to_ml childs

and table_to_ml attrs childs = (* note : caption etc... should be opt *)
  let caption, childs = extract_el "caption" childs in
  let caption = string "~caption:" ^^ parens (xml_to_ml caption) in
  let columns, childs= extract_els "colgroup" childs in
  let columns = string "~columns:" ^^ childs_to_ml columns in
  let thead, childs = extract_el "thead" childs in
  let thead = string "~thead:" ^^ parens (xml_to_ml thead) in
  let tfoot, childs = extract_el "tfoot" childs in
  let tfoot = string "~tfoot:" ^^ parens (xml_to_ml tfoot) in
  let name = 
    if List.exists (tag_name_is "tr") childs then
      "table"
    else
      "tablex"
  in
  string name ^^ space ^^
  caption ^^ space ^^ columns ^^ space ^^
  thead ^^ space ^^ tfoot ^^ space ^^
  attrs_to_ml "table" attrs ^^ space ^^ childs_to_ml childs

and fieldset_to_ml attrs childs = assert false

and datalist_to_ml attrs childs = assert false

and optgroup_to_ml attrs childs = 
  let label, attrs = extract_attr "label" attrs in
  let label = param_attr_to_ml (attr_to_ml "optgroup" label) in
  string "optgroup" ^^ space ^^ label ^^ space ^^ 
  attrs_to_ml "optgroup" attrs ^^ space^^ 
  childs_to_ml childs

and command_to_ml attrs = function
  | [] ->
    let label, attrs = extract_attr "label" attrs in
    let label = param_attr_to_ml (attr_to_ml "command" label) in
    string "command" ^^ space ^^ label ^^ space ^^ 
    attrs_to_ml "command" attrs ^^ string "()"
  | _ -> failwith "Must not have childs"

and menu_to_ml attrs childs = assert false
 *)

(*

let nullary_to_ml name attrs = function
  | [] ->
     mkapply name [("a", attrs)] [unit]
  | _ -> failwith "Must not have childs"

let unary_to_ml name attrs = function
  | [x] ->
     mkapply name [("a", attrs)] [x]
  | _ -> failwith "Must have only one childs"

let star_to_ml name attrs childs =
  mkapply name [("a", attrs)] childs

 *)
