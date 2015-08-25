open Mkast

let regexp_spaces = Str.regexp " +"

let regexp_x = Str.regexp "x"

let split_spaces =
  Str.split regexp_spaces

let split_x =
  Str.split regexp_x

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

let attr_to_ml pos tag_name ((_, name), value) =
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
    | "up" -> mkvariant pos (fmt_variant v) None
    | other -> mkvariant pos "Other" (Some (mkstring pos other))
  in
  let bool_to_ml = function
    | "true" -> true_ pos
    | "false" -> false_ pos
    | _ -> failwith "Must be true or false."
  in
  let sandbox_value_to_ml value =
    split_spaces value
    |> List.map (fun v -> mkvariant pos (fmt_variant v) None)
    |> mklist pos
  in
  let sizes_value_to_ml value =
    match value with
    | "any" -> mkvariant pos "Any" None
    | sizes ->
       let l =
	 split_spaces sizes
	 |> List.map split_x
	 |> List.map (List.map (fun x -> mkint pos x))
	 |> List.map (mktuple pos)
	 |> mklist pos
       in
       mkvariant pos "Sizes" (Some l)
  in
  let name =
    (* UUUUUUUGLY *)
    (match tag_name with
     | "input" -> "input_" ^ name
     | "button"
     | "command"
     | "menu" when name = "type" -> tag_name ^ "_type"
     | _ when name = "type" -> "mime_type"
     | _ -> name )
    |> Bytes.of_string
    |> Bytes.map (fun c -> if c = '-' then '_' else c)
    |> Bytes.to_string
  in
  let ml_attr_value = 
    match name with
    | "input_type"
    | "button_type"
    | "command_type"
    | "menu_type" ->
       mkvariant pos (fmt_variant value) None
    | "autocomplete"
    | "crossorigin"
    | "dir"
    | "formmethod"
    | "method"
    | "preload"
    | "wrap"
    | "xmlns"
    | "align"
    | "scope"
    | "rules"
    | "shape"
    | "frameborder"
    | "scrolling" -> mkvariant pos (fmt_variant value) None
    | "rel" -> rel_value_to_ml value
    | "async"
    | "autofocus"
    | "autoplay"
    | "muted"
    | "controls"
    | "formnovalidate" -> mkvariant pos "Formnovalidate" None
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
    | "defer" -> mkvariant pos (fmt_variant name) None
    | "mediagroup"
    | "challenge"
    | "form"
    | "formenctype"
    | "formtarget"
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
    | "xml_lang"
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
    | "mime_type"
    | "datetime"
    | "for"
    | "name"
    | "text_value"
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
    | "http_equiv"
    | "style"
    | "property" -> mkstring pos value
    | "high"
    | "low"
    | "max"
    | "input_max"
    | "min"
    | "input_min"
    | "optimum"
    | "step"
    | "span" -> mkint pos value
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
    | "width" -> mkfloat pos value
    | "formaction"
    | "action"
    | "icon"
    | "poster"
    | "manifest"
    | "cite"
    | "href"
    | "src"
    | "data" -> mkapply pos "uri_of_string" [] [mkstring pos value]
    | "draggable"
    | "contenteditable"
    | "spellcheck" -> bool_to_ml value
    | "sandbox" -> sandbox_value_to_ml value
    | "sizes" -> sizes_value_to_ml value
    | _ -> failwith ("Unkown attr " ^ name ^ ".")
  in name, ml_attr_value

let a_attr_to_ml pos (name, ml_attr_value) = 
  mkapply pos ("a_" ^ name) [] [ml_attr_value]

let attrs_to_ml pos tag_name attrs =
  (List.map (attr_to_ml pos tag_name) attrs)
  |> List.map (a_attr_to_ml pos)
  |> mklist pos
