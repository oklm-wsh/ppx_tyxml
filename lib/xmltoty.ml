let xmltoty source =
  let input = Xmlm.make_input ~strip:true source in
  let el tag childs = 
    let x, y = Xmlm.pos input in
    let () = Printf.printf "x %d y %d\n" x y in
    `El (Xmlm.pos input, tag, childs) in
  let data s = `Data (Xmlm.pos input, s) in
  match snd (Xmlm.input_doc_tree ~el ~data input) with
  | exception Xmlm.Error ((x, y), err) ->
    Printf.printf "Error at line %d column %d\n" x y;
    failwith ""
  | e -> Tag.xml_to_ml ~input e
			
