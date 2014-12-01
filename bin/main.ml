open PPrint

(* Known issues : <tag> xmlm throws error *)
(*                formnovalidate output a_novalidate *)
(*                enctyp output a_enctyp (should be a_formenctyp with form tag,
                  remind to check issue with form *)

type t = [
  | `El of Xmlm.tag * t list
  | `Data of string
]

let tag_name (((_, name), _) : Xmlm.tag) = name

let is_el = function
  | `El _ -> true
  | `Data _ -> false

let is_data d = not (is_el d)

let extract_els name =
  List.partition 
    (function `El (e, _) -> tag_name e = name
            |  _ -> false)

let extract_el name l =
  match extract_els name l with
  | [el], l -> el, l
  | _ -> failwith ("Must have only one " ^ name)

let attr_to_ml tag_name ((_, name), value) =
  let autocomplete_attr_to_ml = function
    | "on" -> "`On"
    | "off" -> "`Off"
    | _ -> failwith "Unknown autocomplete attr value"
  in
  let ml_attr_value = 
    match name with
    | "autocomplete" -> autocomplete_attr_to_ml value
    | "async" -> "`Async"
    | "autofocus" -> "`Autofocus"
    | "autoplay" -> "`Autoplay"
    | "muted" -> "`Muted"
    | "controls" -> "`Muted"
    | "novalidate" when tag_name = "form" -> "`Formnovalidate" (* formnovalidate and novalidate, issue here *)
    | "novalidate" -> "`Novalidate"
    | "hidden" -> "`Hidden"
    | "ismap" -> "`Ismap"
    | "loop" -> "`Loop"
    | "open" -> "`Open"
    | "pubdate" -> "`Pubdate"
    | "required" -> "`Required"
    | "reversed" -> "`Reversed"
    | "scoped" -> "`Scoped"
    | "seamless" -> "`Seamless"
    | "checked" -> "`Checked"
    | "multiple" -> "`Multiple"
    | "selected" -> "`Selected"
    | "disabled" -> "`Disabled"
    | "readonly" -> "`ReadOnly"
    | "defer" -> "`Defer"
    | "mediagroup"
    | "challenge"
    | "contenteditable"
    | "form"
    | "enctyp"
    | "keytype"
    | "list"
    | "pattern"
    | "placeholder"
    | "radiogroup"
    | "srclang"
    | "class"
    | "id"
    | "title"
    (* | xml-lang *)
    | "onabort"
    | "onafterprint"
    | "onbeforeprint"
    | "onbeforeunload"
    | "onblur"
    | "oncanplay"
    | "oncanplaythrough"
    | "onchange"
    | "ondurationchange"
    | "onemptied"
    | "onended"
    | "onerror"
    | "onfocus"
    | "onformchange"
    | "onforminput"
    | "onhashchange"
    | "oninput"
    | "oninvalid"
    | "onmousewheel"
    | "onoffline"
    | "ononline"
    | "onpause"
    | "onplay"
    | "onplaying"
    | "onpagehide"
    | "onpageshow"
    | "onpopstate"
    | "onprogress"
    | "onratechange"
    | "onreadystatechange"
    | "onredo"
    | "onresize"
    | "onscroll"
    | "onseeked"
    | "onseeking"
    | "onselect"
    | "onshow"
    | "onstalled"
    | "onstorage"
    | "onsubmit"
    | "onsuspend"
    | "ontimeupdate"
    | "onundo"
    | "onunload"
    | "onvolumechange"
    | "onwaiting"
    | "onload"
    | "onloadeddata"
    | "onloadedmetadata"
    | "onloadstart"
    | "onmessage"
    | "onclick"
    | "oncontextmenu"
    | "ondblclick"
    | "ondrag"
    | "ondragend"
    | "ondragenter"
    | "ondragleave"
    | "ondragover"
    | "ondragstart"
    | "ondrop"
    | "onmousedown"
    | "onmouseup"
    | "onmouseover"
    | "onmousemove"
    | "onmouseout"
    | "onkeypress"
    | "onkeydown"
    | "onkeyup"
    | "version"
    | "charset"
    | "hreflang"
    (* | mime-type*) 
    | "datetime"
    | "for"
    | "name"
    (* | text-value *)
    | "value"
    | "label"
    | "axis"
    | "summary"
    | "datapagesize"
    | "alt" 
    | "usemap"
    | "codetype" 
    | "target"
    | "content"
(* http_equiv *)
    | "style"
    | "property" -> "\"" ^ value ^ "\""
    | "high"
    | "low"
    | "max"
    (* | "input_max" *)
    | "min"
    (* | "input_min" *)
    | "optimum"
    | "span"
    | "start"
    | "step"
    | "tabindex"
    | "cols"
    | "maxlength"
    | "rows"
    | "size"
(* | "int_value" *)
(* | "float_value" *)
    | "colspan"
    | "rowspan"
    | "height"
    | "width" -> value
    | _ -> failwith "Unkown attr."
  in string ("a_" ^ name ^ " " ^ "(" ^ ml_attr_value ^ ")")

let attrs_to_ml tag_name = function
  | [] -> string "~a:[]"
  | attrs ->
     let ml_attrs = List.map (attr_to_ml tag_name) attrs in
     string "~a:[" ^^ List.fold_left (fun a b -> a ^^ string ";" ^^ b)
				     (List.hd ml_attrs) (List.tl ml_attrs)
     ^^ string "]\n"

let rec xml_to_ml = function
  | `El (tag, childs) ->
    tag_to_ml tag childs
  | `Data s ->
    string ("(pcdata \"" ^ s ^ "\")")

and tag_to_ml ((_, name), attrs) childs =
  let _ = List.map (attr_to_ml name) attrs in
  let fun_to_ml =
    match name with
    | "html" -> html_to_ml
    | "head" -> head_to_ml
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
    | "svg" -> assert false
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

and childs_to_ml = function
  | [] -> string "[]"
  | childs ->  
     let ml_childs = List.map xml_to_ml childs in 
     string "[\n" ^^
       List.fold_left (fun a b -> a ^^ string ";\n" ^^ b)
		      (List.hd ml_childs) (List.tl ml_childs)
       ^^ string "]\n"
		      
and nullary_to_ml name attrs = function
  | [] ->
    string "(" ^^ string name ^^ string " " 
    ^^ attrs_to_ml name attrs ^^ string " " ^^ string " ())\n"
  | _ -> failwith "Must not have childs."

and unary_to_ml name attrs = function
  | [x] ->
    string "(" ^^ string name ^^ string " " 
    ^^ attrs_to_ml name attrs ^^ string " " ^^ string " "
    ^^ xml_to_ml x ^^ string ")\n"
  | _ -> failwith "Must have only one child."

and star_to_ml name attrs childs =
  string "(" ^^ string name ^^ string " "
  ^^ string " " ^^ attrs_to_ml name attrs ^^ string " " 
  ^^ childs_to_ml childs ^^ string ")\n"

and html_to_ml attrs childs =
  let head, childs = extract_el "head" childs in
  let body, childs = extract_el "body" childs in
  if List.length childs = 0 then
    string "(html " ^^ xml_to_ml head 
    ^^ string " " ^^ string " " ^^ attrs_to_ml "html" attrs 
    ^^ string " " ^^ xml_to_ml body ^^ string ")\n"
  else
    failwith "<html> must only have <head> and <body> as childs"

and head_to_ml attrs childs = 
  let title, childs = extract_el "title" childs in
  string "(head " ^^ xml_to_ml title ^^ string " "
  ^^ string " " ^^ attrs_to_ml "head" attrs ^^ string " " 
  ^^ childs_to_ml childs ^^ string ")\n"

let _ =
  let el tag childs = `El (tag, childs) in
  let data s = `Data s in
  match snd (Xmlm.input_doc_tree ~el ~data (Xmlm.make_input ~strip:true (`Channel stdin))) with
  | `El (tag, childs) as e ->
    let buff = Buffer.create 150 in
    ignore (xml_to_ml e
           |> PPrint.ToBuffer.pretty 1.0 0 buff);
    print_endline (Buffer.contents buff)
  | `Data s -> print_endline "Lol"
  | exception Xmlm.Error ((x, y), err) ->
    Printf.printf "Error at line %d column %d\n" x y;
    print_endline (Xmlm.error_message err)
