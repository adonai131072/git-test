(* File calc.ml *)
let _ =
  try
    while true do
      let _ = Printf.printf ">" in let s = read_line () in
        let result = Parser.main Lexer.token (Lexing.from_string s) in
          Printf.printf "%s\n" (Eval.print_val (Eval.eval result [] (fun v -> v)));
          print_newline();
          flush stdout
      done
  with End_of_file ->
    exit 0
