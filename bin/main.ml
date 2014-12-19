(*open PPrint*)
open Tag
(* Known issues : <tag> xmlm throws error *)
(*                formnovalidate output a_novalidate *)
(*                enctyp output a_enctyp (should be a_formenctyp with form tag,
                  remind to check issue with form *)

let _ =
  let el tag childs = `El (tag, childs) in
  let data s = `Data s in
  match snd (Xmlm.input_doc_tree ~el ~data (Xmlm.make_input ~strip:true (`Channel stdin))) with
  | `El (tag, childs) as e ->
(*    let buff = Buffer.create 150 in
    ignore (xml_to_ml e
           |> PPrint.ToBuffer.pretty 0.9 0 buff);*)
     let exp = xml_to_ml e in
    print_endline (Pprintast.string_of_expression exp)
  | `Data s -> print_endline "Lol"
  | exception Xmlm.Error ((x, y), err) ->
    Printf.printf "Error at line %d column %d\n" x y;
    print_endline (Xmlm.error_message err)
