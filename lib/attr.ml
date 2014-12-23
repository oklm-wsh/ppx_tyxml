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

let attr_to_ml tag_name ((_, name), value) =
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
  let bool_to_ml = function
    | "true" -> true_
    | "false" -> false_
    | _ -> failwith "Must be true or false."
  in
  let sandbox_value_to_ml value =
    split_spaces value
    |> List.map (fun v -> mkvariant (fmt_variant v) None)
    |> mklist
  in
  let sizes_value_to_ml value =
    match value with
    | "any" -> mkvariant "Any" None
    | sizes ->
       let l =
	 split_spaces sizes
	 |> List.map split_x
	 |> List.map (List.map (fun x -> mkint (int_of_string x)))
	 |> List.map mktuple
	 |> mklist
       in
       mkvariant "Sizes" (Some l)
  in
  let name =
    match tag_name with
    | "input" -> "input_" ^ name
    | "form" when name <> "form" -> "form" ^ name
    | _ -> name
  in
  let ml_attr_value = 
    match name with
    | "type" when List.exists ((=) tag_name) ["input"; "button"; "command"; "menu"] ->
       mkvariant (fmt_variant value) None
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
    | "scrolling" -> mkvariant (fmt_variant value) None
    | "rel" -> rel_value_to_ml value
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
    | "input_max"
    | "min"
    | "input_min"
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
    | "draggable"
    | "contenteditable"
    | "spellcheck" -> bool_to_ml value
    | "sandbox" -> sandbox_value_to_ml value
    | "sizes" -> sizes_value_to_ml value
    | _ -> failwith ("Unkown attr " ^ name ^ ".")
  in name, ml_attr_value

let a_attr_to_ml (name, ml_attr_value) = 
  mkapply ("a_" ^ name) [] [ml_attr_value]

let attrs_to_ml tag_name attrs =
  (List.map (attr_to_ml tag_name) attrs)
  |> List.map a_attr_to_ml
  |> mklist 
