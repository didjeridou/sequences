

(* REQUIRES opam install cryptokit *)

open Core.Std

let open_dna_file file =
  In_channel.with_file file ~f:(fun ic ->
    let dna = input_line ic in
    print_endline dna;
  )

(* part 1 *)
let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

(* part 2 *)
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> open_dna_file filename)

(* part 3 *)
let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command



