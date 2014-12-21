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

let extract_opt_el name l =
  match extract_els name l with
  | [el], l -> Some el, l
  | _ -> None, l

let rec xml_to_ml = function
  | `El (tag, childs) ->
    tag_to_ml tag childs
  | `Data s ->
     mkapply "pcdata" [] [mkstring s]

and tag_to_ml ((_, name), attrs) childs =
  let fun_to_ml =
    match name with
  | "html" -> html_to_ml
    | "head" -> head_to_ml
    | "link" -> link_to_ml
    | "img" -> img_to_ml
(*    | "svg" -> svg_to_ml*)
    | "bdo" -> bdo_to_ml
(*    | "figure" -> figure_to_ml
    | "object" -> object_to_ml*)
    | "audio" -> audio_to_ml
    | "video" -> video_to_ml
    | "area" -> area_to_ml
    | "table" -> table_to_ml
(*    | "fieldset" -> fieldset_to_ml
    | "datalist" -> datalist_to_ml*)
    | "optgroup" -> optgroup_to_ml
    | "command" -> command_to_ml
(*    | "menu" -> menu_to_ml*)
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

and childs_to_ml childs =
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
		    
and html_to_ml attrs childs =
  let head, childs = extract_el "head" childs in
  let body, childs = extract_el "body" childs in
  if List.length childs = 0 then
    mkapply "html" 
	    ["a", attrs_to_ml "html" attrs] 
	    [xml_to_ml head; xml_to_ml body]
  else
    failwith "<html> must only have <head> and <body> as childs"

and head_to_ml attrs childs = 
  let title, childs = extract_el "title" childs in
  mkapply "head" 
	  ["a", attrs_to_ml "head" attrs]
	  [xml_to_ml title; childs_to_ml childs]

and link_to_ml attrs = function
  | [] ->
    let rel, attrs = extract_attr "rel" attrs in
    let href, attrs = extract_attr "href" attrs in
    mkapply "link" 
	    ["rel", attrs_to_ml "link" [rel];
	     "href", attrs_to_ml "link" [href];
	     "a", attrs_to_ml "link" attrs]
	    [unit]
  | _ -> failwith "Must not have childs"

and img_to_ml attrs = function
  | [] ->
    let src, attrs = extract_attr "src" attrs in
    let alt, attrs = extract_attr "alt" attrs in
    mkapply "link" 
	    ["src", attrs_to_ml "img" [src];
	     "alt", attrs_to_ml "img" [alt];
	     "a", attrs_to_ml "img" attrs]
	    [unit]
  | _ -> failwith "Must not have childs"
(*
and svg_to_ml attrs childs =
  let xmlns, attrs = extract_opt_attr "xmlns" attrs in
  let xmlns = opt_attr_to_ml "svg" xmlns
  in
  string "svg" ^^ space ^^ xmlns ^^ space ^^ attrs_to_ml "svg" attrs
  ^^ space ^^ childs_to_ml childs
 *)
and bdo_to_ml attrs childs =
  let dir, attrs = extract_attr "dir" attrs in
  mkapply "bdo" ["dir", attrs_to_ml "bdo" [dir];
		 "a", attrs_to_ml "bdo" attrs]
	  [childs_to_ml childs]

and figure_to_ml attrs childs = assert false

and object_to_ml attrs childs = assert false
 
and multimedia_to_ml name attrs childs =
  let src, attrs = extract_opt_attr "src" attrs in
  let srcs, childs = extract_els "source" childs in
  let attrs =
    ["srcs", childs_to_ml srcs;
     "a", attrs_to_ml name attrs]
  in
  let attrs = 
    match src with
    | Some a -> ("src", attrs_to_ml name [a]) :: attrs
    | None -> attrs
  in
  mkapply name
	  attrs
	  [childs_to_ml childs]

and audio_to_ml attrs childs =
  multimedia_to_ml "audio" attrs childs

and video_to_ml attrs childs =
  multimedia_to_ml "video" attrs childs

and area_to_ml attrs childs =
  let alt, attrs = extract_attr "alt" attrs in
  mkapply "area" 
	  ["alt", attrs_to_ml "area" [alt];
	   "a", attrs_to_ml "area" attrs]
	  [childs_to_ml childs]

and table_to_ml attrs childs =
  let caption, childs = extract_opt_el "caption" childs in
  let columns, childs= extract_els "colgroup" childs in
  let thead, childs = extract_el "thead" childs in
  let tfoot, childs = extract_el "tfoot" childs in
  let name = 
    if List.exists (tag_name_is "tr") childs then
      "table"
    else
      "tablex"
  in
  let attrs =
    ["columns", childs_to_ml columns;
     "thead", xml_to_ml thead;
     "tfoot", xml_to_ml tfoot;
     "a", attrs_to_ml name attrs]
  in
  let attrs =
    match caption with
    | Some a -> ("caption", xml_to_ml a) :: attrs
    | None -> attrs
  in
  mkapply name
	  attrs
	  [childs_to_ml childs]

and fieldset_to_ml attrs childs = assert false

and datalist_to_ml attrs childs = assert false

and optgroup_to_ml attrs childs = 
  let label, attrs = extract_attr "label" attrs in
  mkapply "optgroup" 
	  ["label", attrs_to_ml "optgroup" [label];
	  "a", attrs_to_ml "optgroup" attrs]
	  [childs_to_ml childs]

and command_to_ml attrs = function
  | [] ->
    let label, attrs = extract_attr "label" attrs in
    mkapply "command"
	    ["label", attrs_to_ml "command" [label];
	     "a", attrs_to_ml "command" attrs]
	    [unit]
  | _ -> failwith "Must not have childs"

and menu_to_ml attrs childs = assert false

