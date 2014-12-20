open Mkast

let fmt_variant s =
  Bytes.of_string s 
  |> Bytes.capitalize
  |> Bytes.map (fun c -> if c = '-' then '_' else c)
  |> Bytes.to_string

let extract_attrs name =
  List.partition
    (fun ((_, n), _) -> n = name)

let extract_attr name l =
  match extract_attrs name l with
  | [attr], l -> attr, l
  | _ -> failwith ("Must have only one " ^ name ^ "attr.")

let extract_opt_attr name l =
  match extract_attrs name l with
  | [attr], l -> Some attr, l
  | [], l -> None, l
  | _ -> failwith ("Must have only one " ^ name ^ "attr.")

let attr_to_ml tag_name ((_, name), value) =
  let autocomplete_value_to_ml = function
    | "on" -> "`On"
    | "off" -> "`Off"
    | _ -> failwith "Unknown autocomplete attr value"
  in
  let crossorigin_value_to_ml = function
    | "anonymous" -> "`Anonymous"
    | "use-credentials" -> "`Use_credentials"
    | _ -> failwith "Unknown crossorigin attr value"
  in
  let dir_value_to_ml = function
    | "lrt" -> "`Lrt"
    | "rtl" -> "`Rtl"
    | _ -> failwith "Unknown dir attr value"
  in
  let formmethod_value_to_ml value =
    match String.lowercase value with
    | "delete" -> "`Delete"
    | "get" -> "`Get"
    | "post" -> "`Post"
    | "put" -> "`Put"
    | _ -> failwith "Unknown formmethod or method attr value"
  in
  let method_value_to_ml = formmethod_value_to_ml in
  let preload_value_to_ml = function
    | "audio" -> "`Audio"
    | "metadata" -> "`Metadata"
    | "none" -> "`None"
    | _ -> failwith "Unknown preload attr value"
  in
  let wrap_value_to_ml = function
    | "hard" -> "`Hard"
    | "soft" -> "`Soft"
    | _ -> failwith "Unkown wrap attr value"
  in
  let xmlns_value_to_ml = function
    | "http://www.w3.org/1999/xhtml" -> "`W3_org_1999_xhtml"
    | _ -> failwith "xmls value should be http://www.w3.org/1999/xhtml"
  in
  let input_type_value_to_ml = function
    | "button" -> "`Button"
    | "checkbox" -> "`Checkbox"
    | "color" -> "`Color"
    | "date" -> "`Date"
    | "datetime" -> "`Datetime"
    | "datetime-local" -> "`Datetime_local"
    | "email" -> "`Email"
    | "file" -> "`File"
    | "hidden" -> "`Hidden"
    | "image" -> "`Image"
    | "month" -> "`Month"
    | "number" -> "`Number"
    | "password" -> "`Password"
    | "radio" -> "`Radio"
    | "range" -> "`Range"
    | "reset" -> "`Reset"
    | "search" -> "`Search"
    | "submit" -> "`Submit"
    | "tel" -> "`Tel"
    | "text" -> "`Text"
    | "time" -> "`Time"
    | "url" -> "`Url"
    | "week" -> "`Week"
    | _ -> failwith "Unknown input type value attr"
  in
  let button_type_value_to_ml = function
    | "button" -> "`Button"
    | "reset" -> "`Reset"
    | "submit" -> "`Submit"
    | _ -> failwith "Unknown button type value attr"
  in
  let command_type_value_to_ml = function
    | "checkbox" -> "`Checkbox"
    | "command" -> "`Command"
    | "radio" -> "`Radio"
    | _ -> failwith "Unknown command type value attr"
  in
  let menu_type_value_to_ml = function
    | "context" -> "`Context"
    | "toolbar" -> "`Toolbar"
    | _ -> failwith "Unknown menu type value attr"
  in
  let align_value_to_ml = function
    | "char" -> "`Char"
    | "justify" -> "`Justify"
    | "left" -> "`Left"
    | "right" -> "`Right"
    | _ -> failwith "Unknown align value attr"
  in
  let scope_value_to_ml = function
    | "col" -> "`Col"
    | "colgroup" -> "`Colgroup"
    | "row" -> "`Row"
    | "rowgroup" -> "`Rowgroup"
    | _ -> failwith "Unknown scope value attr"
  in
  let rules_value_to_ml = function
    | "all" -> "`All"
    | "cols" -> "`Cols"
    | "groups" -> "`Groups"
    | "none" -> "`None"
    | "rows" -> "`Rows"
    | _ -> failwith "Unknown rules value attr"
  in
  let shape_value_to_ml = function
    | "circle" -> "`Circle"
    | "default" -> "`Default"
    | "poly" -> "`Poly"
    | "rect" -> "`Rect"
    | _ -> failwith "Unknown shape value attr"
  in
  let frameborder_value_to_ml = function
    | "one" -> "`One"
    | "zero" -> "`Zero"
    | _ -> failwith "Unknown frameborder value attr"
  in
  let scrolling_value_to_ml = function
    | "auto" -> "`Auto"
    | "no" -> "`No"
    | "yes" -> "`Yes"
    | _ -> failwith "Unknown scrolling value attr"
  in
  let rel_value_to_ml = function
    | "alternate" -> "`Alternate"
    | "archives" -> "`Archives"
    | "author" -> "`Author"
    | "bookmark" -> "`Bookmark"
    | "external" -> "`External"
    | "first" -> "`First"
    | "help" -> "`Help"
    | "icon" -> "`Icon"
    | "index" -> "`Index"
    | "last" -> "`Last"
    | "license" -> "`License"
    | "next" -> "`Next"
    | "nofollow" -> "`Nofollow"
    | "noreferrer" -> "`Noreferrer"
    | "pingback" -> "`Pingback"
    | "prefetch" -> "`Prefetch"
    | "prev" -> "`Prev"
    | "search" -> "`Search"
    | "sidebar" -> "`Sidebar"
    | "stylesheet" -> "`Stylesheet"
    | "tag" -> "`Tag"
    | "up" -> "`Up"
    | other -> "(`Other \"" ^ other ^ "\")"
  in
  let ml_attr_value = 
    match name with
(*    | "autocomplete" -> autocomplete_value_to_ml value
    | "crossorigin" -> crossorigin_value_to_ml value
    | "dir" -> dir_value_to_ml value
    | "formmethod" -> formmethod_value_to_ml value
    | "method" -> method_value_to_ml value
    | "preload" -> preload_value_to_ml value
    | "wrap" -> wrap_value_to_ml value
    | "xmlns" -> xmlns_value_to_ml value
    | "type" when tag_name = "input" -> input_type_value_to_ml value
    | "type" when tag_name = "button" -> button_type_value_to_ml value
    | "type" when tag_name = "command" -> command_type_value_to_ml value
    | "type" when tag_name = "menu" -> menu_type_value_to_ml value
    | "align" -> align_value_to_ml value
    | "scope" -> scope_value_to_ml value
    | "rules" -> rules_value_to_ml value
    | "shape" -> shape_value_to_ml value
    | "frameborder" -> frameborder_value_to_ml value
    | "scrolling" -> scrolling_value_to_ml value
    | "rel" -> rel_value_to_ml value*)
    | "async" (* -> "`Async" *)
    | "autofocus" (* -> "`Autofocus" *)
    | "autoplay" (* -> "`Autoplay" *)
    | "muted" (* -> "`Muted" *)
    | "controls" (* -> "`Controls" *)
    (*| "novalidate" when tag_name = "form" -> "`Formnovalidate" (* formnovalidate and novalidate, issue here *)*)
    | "novalidate" (* -> "`Novalidate" *)
    | "hidden" (* -> "`Hidden" *)
    | "ismap" (* -> "`Ismap" *)
    | "loop" (* -> "`Loop" *)
    | "open" (* -> "`Open" *)
    | "pubdate" (* -> "`Pubdate" *)
    | "required" (* -> "`Required" *)
    | "reversed" (* -> "`Reversed" *)
    | "scoped" (* -> "`Scoped" *)
    | "seamless" (* -> "`Seamless" *)
    | "checked" (* -> "`Checked" *)
    | "multiple" (* -> "`Multiple" *)
    | "selected" (* -> "`Selected" *)
    | "disabled" (* -> "`Disabled" *)
    | "readonly" (* -> "`ReadOnly" *)
    | "defer" (* -> "`Defer" *) -> mkvariant (fmt_variant name)
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
    | "type"     (* WARNING !!! should be | mime-type*) 
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
    | "property" -> mkstring value(*"\"" ^ value ^ "\""*)
(*    | "high"
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
    | "width" -> value*)
    | "action" (* formaction too *)
    | "icon"
    | "poster"
    | "manifest"
    | "cite"
    | "href"
    | "src"
    | "data" -> mkapply "uri_of_string" [] [mkstring value] (*"(uri_of_string \"" ^ value ^ "\")"*)
    | _ -> failwith ("Unkown attr " ^ name ^ ".")
  in name, ml_attr_value

let a_attr_to_ml (name, ml_attr_value) = 
  (*string ("a_" ^ name ^ " " ^ ml_attr_value)*)
  mkapply ("a_" ^ name) [] [ml_attr_value]
(*
let param_attr_to_ml (name, ml_attr_value) =
  string ("~" ^ name ^ ":" ^ "(" ^ ml_attr_value ^ ")")

let opt_attr_to_ml name = function
  | Some a -> param_attr_to_ml (attr_to_ml name a) 
  | None -> string "" 
 *)
let attrs_to_ml tag_name attrs = (*function
  | [] -> string "~a:[]"
  | attrs ->
     let ml_attrs = List.map (attr_to_ml tag_name) attrs 
                    |> List.map a_attr_to_ml in
     string "~a:" ^^ OCaml.list (fun x -> x) ml_attrs*)
  (List.map (attr_to_ml tag_name) attrs)
  |> List.map a_attr_to_ml
  |> mklist 
