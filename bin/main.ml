open PPrint

(* Known issues : <tag> xmlm throws error *)

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

let rec xml_to_ml = function
  | `El (tag, childs) ->
    tag_to_ml tag childs
  | `Data s ->
    string ("(pcdata \"" ^ s ^ "\"")

and tag_to_ml ((_, name), attrs) childs =
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

and childs_to_ml childs = 
  string "[\n" ^^
  (List.map xml_to_ml childs
   |> List.fold_left (fun a b -> a ^^ string ";\n" ^^ b) (string ""))
  ^^ string "]\n"

and nullary_to_ml name attrs = function
  | [] ->
    string "(" ^^ string name ^^ string " ())\n"
  | _ -> failwith "Must not have childs."

and unary_to_ml name attrs = function
  | [x] ->
    string "(" ^^ string name ^^ string " "
    ^^ xml_to_ml x ^^ string ")\n"
  | _ -> failwith "Must have only one child."

and star_to_ml name attrs childs =
  string "(" ^^ string name ^^ string " "
  ^^ childs_to_ml childs ^^ string ")\n"

and html_to_ml attrs childs =
  let head, childs = extract_el "head" childs in
  let body, childs = extract_el "body" childs in
  if List.length childs = 0 then
    string "(html " ^^ xml_to_ml head 
    ^^ string " " ^^ xml_to_ml body ^^ string ")\n"
  else
    failwith "<html> must only have <head> and <body> as childs"

and head_to_ml attrs childs = 
  let title, childs = extract_el "title" childs in
  string "(head " ^^ xml_to_ml title ^^ string " "
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
