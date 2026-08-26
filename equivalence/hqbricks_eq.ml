open Cmdliner
open Cmdliner.Term.Syntax

let pair_converter =
  let parse text =
    match Hps_equivalence.endpoint_pair_of_string text with
    | Ok pair -> Ok pair
    | Error message -> Error (`Msg message)
  in
  Arg.conv
    ( parse,
      fun formatter pair ->
        Format.pp_print_string formatter
          (Hps_equivalence.endpoint_pair_to_string pair) )

let left_file =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"LEFT.qasm")

let right_file =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"RIGHT.qasm")

let input_pairs =
  Arg.(
    value & opt_all pair_converter []
    & info [ "input-pair" ] ~docv:"TYPED-ENDPOINT=TYPED-ENDPOINT")

let output_pairs =
  Arg.(
    value & opt_all pair_converter []
    & info [ "output-pair" ] ~docv:"TYPED-ENDPOINT=TYPED-ENDPOINT")

let run left_file right_file input_pairs output_pairs =
  match
    Hps_equivalence.check_files ~input_pairs ~output_pairs left_file right_file
  with
  | Ok verdict -> print_endline (Hps_equivalence.verdict_to_string verdict)
  | Error message ->
      Printf.eprintf "error: %s\n" message;
      exit 2

let command =
  let doc = "check bounded OpenQASM 2 equivalence with HQbricks HPS" in
  Cmd.v (Cmd.info "hqbricks_eq" ~doc)
  @@
  let+ left_file = left_file
  and+ right_file = right_file
  and+ input_pairs = input_pairs
  and+ output_pairs = output_pairs in
  run left_file right_file input_pairs output_pairs

let () = if not !Sys.interactive then exit (Cmd.eval command)
