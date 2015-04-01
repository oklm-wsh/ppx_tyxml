let xmltoty source =
  let el tag childs = `El (tag, childs) in
  let data s = `Data s in
  let input = Xmlm.make_input ~strip:true source in
  match snd (Xmlm.input_doc_tree ~el ~data input) with
  | exception Xmlm.Error ((x, y), err) ->
    Printf.printf "Error at line %d column %d\n" x y;
    failwith ""
  | e -> Tag.xml_to_ml ~input e
			
