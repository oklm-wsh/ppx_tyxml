let build_start_loc start_loc (y, x) =
  let {Lexing.pos_lnum; pos_cnum} = start_loc in
  Printf.printf "start y %d start x %d\n" pos_lnum pos_cnum;
  Printf.printf "xmlm y %d xmlm x %d\n" y x;
  let pos_lnum, pos_cnum = pos_lnum + y - 1, x - 1 in
  Printf.printf "y %d x %d\n" pos_lnum pos_cnum;
  Lexing.{start_loc with pos_lnum; pos_cnum}

let xmltoty loc source =
  let input = Xmlm.make_input ~strip:true source in
  let el tag childs = 
    `El (build_start_loc loc (Xmlm.pos input), tag, childs)
  in
  let data s = `Data (build_start_loc loc (Xmlm.pos input), s) in
  try
    Xmlm.input_doc_tree ~el ~data input
    |> snd 
    |> Tag.xml_to_ml ~input
  with
  | Xmlm.Error ((x, y), _) ->
    Printf.eprintf "Error at line %d column %d\n" x y;
    assert false
  | Failure msg ->
    print_endline msg;
    assert false
      

