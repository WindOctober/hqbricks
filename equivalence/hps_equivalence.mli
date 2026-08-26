(** A conservative HPS equivalence checker for a bounded OpenQASM 2 subset. *)

type kind = Quantum | Classical
type endpoint = { kind : kind; register : string; index : int }
type endpoint_pair = { left : endpoint; right : endpoint }
type verdict = Eq | Neq | Unknown

val endpoint_pair_of_string : string -> (endpoint_pair, string) result
(** Parse [quantum:q[0]=quantum:r[0]] or the analogous [classical:] form. *)

val endpoint_pair_to_string : endpoint_pair -> string
val verdict_to_string : verdict -> string

val check_openqasm2 :
  input_pairs:endpoint_pair list ->
  output_pairs:endpoint_pair list ->
  string ->
  string ->
  (verdict, string) result
(** Check two OpenQASM 2 source strings. [Error] is reserved for invalid or
    ambiguous endpoint/declaration metadata; unsupported semantics and resource
    limits produce [Ok Unknown]. QbIRcks parser metadata is module-global, so a
    call to this function or [check_files] must not overlap any QbIRcks OpenQASM
    parse. *)

val check_files :
  input_pairs:endpoint_pair list ->
  output_pairs:endpoint_pair list ->
  string ->
  string ->
  (verdict, string) result
(** Read and check two OpenQASM 2 files. File errors are returned as [Error]. *)
