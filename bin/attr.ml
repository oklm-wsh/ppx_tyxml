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
  let autocomplete_value_to_ml v =
    match v with
    | "on"
    | "off" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown autocomplete attr value"
  in
  let crossorigin_value_to_ml v =
    match v with
    | "anonymous"
    | "use-credentials" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown crossorigin attr value"
  in
  let dir_value_to_ml v =
    match v with
    | "lrt"
    | "rtl" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown dir attr value"
  in
  let formmethod_value_to_ml value =
    match String.lowercase value with
    | "delete"
    | "get"
    | "post"
    | "put" -> mkvariant (fmt_variant (String.lowercase value)) None
    | _ -> failwith "Unknown formmethod or method attr value"
  in
  let method_value_to_ml = formmethod_value_to_ml in
  let preload_value_to_ml v =
    match v with
    | "audio"
    | "metadata"
    | "none" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown preload attr value"
  in
  let wrap_value_to_ml v =
    match v with
    | "hard"
    | "soft" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unkown wrap attr value"
  in
  let xmlns_value_to_ml = function
    | "http://www.w3.org/1999/xhtml" -> mkvariant "W3_org_1999_xhtml" None
    | _ -> failwith "xmls value should be http://www.w3.org/1999/xhtml"
  in
  let input_type_value_to_ml v =
    match v with
    | "button"
    | "checkbox"
    | "color"
    | "date"
    | "datetime"
    | "datetime-local"
    | "email"
    | "file"
    | "hidden"
    | "image"
    | "month"
    | "number"
    | "password"
    | "radio"
    | "range"
    | "reset"
    | "search"
    | "submit"
    | "tel"
    | "text"
    | "time"
    | "url"
    | "week" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown input type value attr"
  in
  let button_type_value_to_ml v =
    match v with
    | "button"
    | "reset"
    | "submit" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown button type value attr"
  in
  let command_type_value_to_ml v =
    match v with
    | "checkbox"
    | "command"
    | "radio" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown command type value attr"
  in
  let menu_type_value_to_ml v =
    match v with
    | "context"
    | "toolbar" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown menu type value attr"
  in
  let align_value_to_ml v =
    match v with
    | "char"
    | "justify"
    | "left"
    | "right" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown align value attr"
  in
  let scope_value_to_ml v =
    match v with
    | "col"
    | "colgroup"
    | "row"
    | "rowgroup" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown scope value attr"
  in
  let rules_value_to_ml v =
    match v with
    | "all"
    | "cols"
    | "groups"
    | "none"
    | "rows" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown rules value attr"
  in
  let shape_value_to_ml v =
    match v with
    | "circle"
    | "default"
    | "poly"
    | "rect" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown shape value attr"
  in
  let frameborder_value_to_ml v =
    match v with
    | "one"
    | "zero" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown frameborder value attr"
  in
  let scrolling_value_to_ml v =
    match v with
    | "auto"
    | "no"
    | "yes" -> mkvariant (fmt_variant v) None
    | _ -> failwith "Unknown scrolling value attr"
  in
  let rel_value_to_ml v =
    match v with
    | "alternate"
    | "archives"
    | "author"
    | "bookmark"
    | "external"
    | "first"
    | "help"
    | "icon"
    | "index"
    | "last"
    | "license"
    | "next"
    | "nofollow"
    | "noreferrer"
    | "pingback"
    | "prefetch"
    | "prev"
    | "search"
    | "sidebar"
    | "stylesheet"
    | "tag"
    | "up" -> mkvariant (fmt_variant v) None
    | other -> mkvariant "Other" (Some (mkstring other))
  in
  let ml_attr_value = 
    match name with
    | "autocomplete" -> autocomplete_value_to_ml value
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
    | "rel" -> rel_value_to_ml value
    | "async"
    | "autofocus"
    | "autoplay"
    | "muted"
    | "controls"
    (*| "novalidate" when tag_name = "form" -> "`Formnovalidate" (* formnovalidate and novalidate, issue here *)*)
    | "novalidate"
    | "hidden"
    | "ismap"
    | "loop"
    | "open"
    | "pubdate"
    | "required"
    | "reversed"
    | "scoped"
    | "seamless"
    | "checked"
    | "multiple"
    | "selected"
    | "disabled"
    | "readonly"
    | "defer" -> mkvariant (fmt_variant name) None
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
    | "property" -> mkstring value
    | "high"
    | "low"
    | "max"
    (* | "input_max" *)
    | "min"
    (* | "input_min" *)
    | "optimum"
    | "step"
    | "span" -> mkint (int_of_string value)
    | "start"
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
    | "width" -> mkfloat value
    | "action" (* formaction too *)
    | "icon"
    | "poster"
    | "manifest"
    | "cite"
    | "href"
    | "src"
    | "data" -> mkapply "uri_of_string" [] [mkstring value]
    | _ -> failwith ("Unkown attr " ^ name ^ ".")
  in name, ml_attr_value

let a_attr_to_ml (name, ml_attr_value) = 
  mkapply ("a_" ^ name) [] [ml_attr_value]
(*
let param_attr_to_ml (name, ml_attr_value) =
  string ("~" ^ name ^ ":" ^ "(" ^ ml_attr_value ^ ")")

let opt_attr_to_ml name = function
  | Some a -> param_attr_to_ml (attr_to_ml name a) 
  | None -> string "" 
 *)
let attrs_to_ml tag_name attrs =
  (List.map (attr_to_ml tag_name) attrs)
  |> List.map a_attr_to_ml
  |> mklist 
