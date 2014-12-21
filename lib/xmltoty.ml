let xmltoty source =
  let el tag childs = `El (tag, childs) in
  let data s = `Data s in
  match snd (Xmlm.input_doc_tree ~el ~data (Xmlm.make_input ~strip:true source)) with
  | `El (tag, childs) as e -> Tag.xml_to_ml e
  | `Data s -> failwith "Must begin with a tag."
  | exception Xmlm.Error ((x, y), err) ->
    Printf.printf "Error at line %d column %d\n" x y;
    failwith ""

