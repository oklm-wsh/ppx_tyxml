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
  let ml_attr_value = 
    match name with
    | "type" when List.exists ((=) tag_name) ["input"; "button"; "command"; "menu"] ->
       mkvariant (fmt_variant value) None
    | "autocomplete" (*-> autocomplete_value_to_ml value*)
    | "crossorigin" (*-> crossorigin_value_to_ml value*)
    | "dir" (*-> dir_value_to_ml value*)
    | "formmethod" (*-> formmethod_value_to_ml value*)
    | "method" (*-> method_value_to_ml value*)
    | "preload" (*-> preload_value_to_ml value*)
    | "wrap" (*-> wrap_value_to_ml value*)
    | "xmlns" (*-> xmlns_value_to_ml value
    | "type" when tag_name = "input" -> input_type_value_to_ml value
    | "type" when tag_name = "button" -> button_type_value_to_ml value
    | "type" when tag_name = "command" -> command_type_value_to_ml value
    | "type" when tag_name = "menu" -> menu_type_value_to_ml value*)
    | "align" (*-> align_value_to_ml value*)
    | "scope" (*-> scope_value_to_ml value*)
    | "rules" (*-> rules_value_to_ml value*)
    | "shape" (*-> shape_value_to_ml value*)
    | "frameborder" (*-> frameborder_value_to_ml value*)
    | "scrolling" (*-> scrolling_value_to_ml value*)
    | "rel" (*-> rel_value_to_ml value*) -> mkvariant (fmt_variant value) None
    | "async"
    | "autofocus"
    | "autoplay"
    | "muted"
    | "controls"
    | "novalidate" when tag_name = "form" -> mkvariant "Formnovalidate" None (* formnovalidate and novalidate, issue here*)
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

let attrs_to_ml tag_name attrs =
  (List.map (attr_to_ml tag_name) attrs)
  |> List.map a_attr_to_ml
  |> mklist 
