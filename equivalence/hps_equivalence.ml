open Hqbricks

type kind = Quantum | Classical
type endpoint = { kind : kind; register : string; index : int }
type endpoint_pair = { left : endpoint; right : endpoint }
type verdict = Eq | Neq | Unknown

exception Invalid_input of string
exception Unsupported

let max_input_pairs = 8
let max_output_pairs = 8
let max_declared_bits = 64
let max_ast_nodes = 10_000
let max_path_variables = 12
let max_work_units = 5_000_000
let max_source_bytes = 1_000_000
let kind_to_string = function Quantum -> "quantum" | Classical -> "classical"

let endpoint_to_string endpoint =
  Printf.sprintf "%s:%s[%d]"
    (kind_to_string endpoint.kind)
    endpoint.register endpoint.index

let endpoint_pair_to_string pair =
  endpoint_to_string pair.left ^ "=" ^ endpoint_to_string pair.right

let verdict_to_string = function
  | Eq -> "eq"
  | Neq -> "neq"
  | Unknown -> "unknown"

let parse_endpoint text =
  let kind, body =
    if String.starts_with ~prefix:"quantum:" text then
      (Quantum, String.sub text 8 (String.length text - 8))
    else if String.starts_with ~prefix:"classical:" text then
      (Classical, String.sub text 10 (String.length text - 10))
    else raise (Invalid_input "endpoint kind must be quantum or classical")
  in
  match String.rindex_opt body '[' with
  | None -> raise (Invalid_input "endpoint must end in [INDEX]")
  | Some bracket ->
      let body_len = String.length body in
      if bracket = 0 || body_len < bracket + 3 || body.[body_len - 1] <> ']'
      then raise (Invalid_input "endpoint must be REGISTER[INDEX]")
      else
        let register = String.sub body 0 bracket in
        let index_text =
          String.sub body (bracket + 1) (body_len - bracket - 2)
        in
        let index =
          try int_of_string index_text
          with Failure _ ->
            raise (Invalid_input "endpoint index is not an integer")
        in
        if index < 0 then
          raise (Invalid_input "endpoint index must be nonnegative")
        else { kind; register; index }

let endpoint_pair_of_string text =
  try
    if not (String.equal text (String.trim text)) then
      raise (Invalid_input "endpoint pair must not contain outer whitespace");
    match String.split_on_char '=' text with
    | [ left; right ] ->
        let left = parse_endpoint left in
        let right = parse_endpoint right in
        if left.kind <> right.kind then
          Error "paired endpoints must have the same kind"
        else Ok { left; right }
    | _ -> Error "endpoint pair must contain exactly one '='"
  with Invalid_input message -> Error message

let compare_kind left right =
  match (left, right) with
  | Quantum, Quantum | Classical, Classical -> 0
  | Quantum, Classical -> -1
  | Classical, Quantum -> 1

let compare_endpoint left right =
  match compare_kind left.kind right.kind with
  | 0 -> (
      match String.compare left.register right.register with
      | 0 -> Int.compare left.index right.index
      | result -> result)
  | result -> result

let equal_endpoint left right = compare_endpoint left right = 0

module Endpoint_set = Set.Make (struct
  type t = endpoint

  let compare = compare_endpoint
end)

type register = {
  register_kind : kind;
  register_name : string;
  register_length : int;
}

type side = Left | Right

type prepared = {
  bits : (endpoint * string) list;
  program : Prog.t;
  ast_nodes : int;
}

let int_of_z value = try Z.to_int value with Z.Overflow -> raise Unsupported

let register_of_declaration kind (name, length) =
  let length = int_of_z length in
  if length <= 0 then
    raise
      (Invalid_input (Printf.sprintf "register %s has nonpositive length" name));
  if length > max_declared_bits then raise Unsupported;
  { register_kind = kind; register_name = name; register_length = length }

let registers_of_parsed (parsed : Qbircks.Translation.parsed) =
  let registers =
    List.map (register_of_declaration Quantum) parsed.qregs
    @ List.map (register_of_declaration Classical) parsed.cregs
  in
  let rec reject_duplicates seen = function
    | [] -> ()
    | register :: rest ->
        if List.mem register.register_name seen then
          raise
            (Invalid_input
               (Printf.sprintf "duplicate or cross-kind register %s"
                  register.register_name));
        reject_duplicates (register.register_name :: seen) rest
  in
  reject_duplicates [] registers;
  ignore
    (List.fold_left
       (fun total register ->
         if total > max_declared_bits - register.register_length then
           raise Unsupported;
         total + register.register_length)
       0 registers);
  registers

let find_register registers endpoint =
  List.find_opt
    (fun register ->
      register.register_kind = endpoint.kind
      && String.equal register.register_name endpoint.register)
    registers

let validate_endpoint side_name registers endpoint =
  match find_register registers endpoint with
  | None ->
      raise
        (Invalid_input
           (Printf.sprintf "%s endpoint %s is not declared" side_name
              (endpoint_to_string endpoint)))
  | Some register
    when endpoint.index < 0 || endpoint.index >= register.register_length ->
      raise
        (Invalid_input
           (Printf.sprintf "%s endpoint %s is out of bounds" side_name
              (endpoint_to_string endpoint)))
  | Some _ -> ()

let validate_pairs label left_registers right_registers pairs =
  let rec loop left_seen right_seen = function
    | [] -> ()
    | pair :: rest ->
        if pair.left.kind <> pair.right.kind then
          raise (Invalid_input (label ^ " pair changes endpoint kind"));
        validate_endpoint "left" left_registers pair.left;
        validate_endpoint "right" right_registers pair.right;
        if Endpoint_set.mem pair.left left_seen then
          raise
            (Invalid_input
               (Printf.sprintf "duplicate left endpoint in %s pairs: %s" label
                  (endpoint_to_string pair.left)));
        if Endpoint_set.mem pair.right right_seen then
          raise
            (Invalid_input
               (Printf.sprintf "duplicate right endpoint in %s pairs: %s" label
                  (endpoint_to_string pair.right)));
        loop
          (Endpoint_set.add pair.left left_seen)
          (Endpoint_set.add pair.right right_seen)
          rest
  in
  loop Endpoint_set.empty Endpoint_set.empty pairs

let endpoints_of_registers registers =
  List.concat_map
    (fun register ->
      List.init register.register_length (fun index ->
          {
            kind = register.register_kind;
            register = register.register_name;
            index;
          }))
    registers

let endpoint_on_side side pair =
  match side with Left -> pair.left | Right -> pair.right

let canonical_output_name kind index =
  Printf.sprintf "__hqbricks_eq_observed_%s_%d"
    (match kind with Quantum -> "q" | Classical -> "c")
    index

let observed_name side output_pairs endpoint =
  let rec find index = function
    | [] -> None
    | pair :: rest ->
        if equal_endpoint endpoint (endpoint_on_side side pair) then
          Some (canonical_output_name endpoint.kind index)
        else find (index + 1) rest
  in
  find 0 output_pairs

let make_bit_names side output_pairs registers =
  List.mapi
    (fun ordinal endpoint ->
      let name =
        match observed_name side output_pairs endpoint with
        | Some name -> name
        | None ->
            Printf.sprintf "__hqbricks_eq_%s_unobserved_%d"
              (match side with Left -> "left" | Right -> "right")
              ordinal
      in
      (endpoint, name))
    (endpoints_of_registers registers)

let internal_name bits endpoint =
  match
    List.find_opt (fun (candidate, _) -> equal_endpoint endpoint candidate) bits
  with
  | Some (_, name) -> name
  | None -> raise Unsupported

let validate_embedded_register registers kind name embedded_length =
  let endpoint = { kind; register = name; index = 0 } in
  match find_register registers endpoint with
  | Some register
    when Z.equal embedded_length (Z.of_int register.register_length) ->
      register
  | _ -> raise Unsupported

let singleton_qreg name = Qbircks.Base.QIndex ((name, Z.one), Z.zero)
let singleton_creg name = Qbircks.Base.CIndex ((name, Z.one), Z.zero)

let expand_qreg registers bits = function
  | Qbircks.Base.QIndex ((name, length), index) ->
      let register = validate_embedded_register registers Quantum name length in
      let index = int_of_z index in
      if index < 0 || index >= register.register_length then raise Unsupported;
      let endpoint = { kind = Quantum; register = name; index } in
      [ singleton_qreg (internal_name bits endpoint) ]
  | Qbircks.Base.QCons (name, length) ->
      let register = validate_embedded_register registers Quantum name length in
      List.init register.register_length (fun index ->
          let endpoint = { kind = Quantum; register = name; index } in
          singleton_qreg (internal_name bits endpoint))

let expand_creg registers bits = function
  | Qbircks.Base.CIndex ((name, length), index) ->
      let register =
        validate_embedded_register registers Classical name length
      in
      let index = int_of_z index in
      if index < 0 || index >= register.register_length then raise Unsupported;
      let endpoint = { kind = Classical; register = name; index } in
      [ singleton_creg (internal_name bits endpoint) ]
  | Qbircks.Base.CCons (name, length) ->
      let register =
        validate_embedded_register registers Classical name length
      in
      List.init register.register_length (fun index ->
          let endpoint = { kind = Classical; register = name; index } in
          singleton_creg (internal_name bits endpoint))

let rec transform_bool registers bits = function
  | Qbircks.Base.False -> Qbircks.Base.False
  | Qbircks.Base.True -> Qbircks.Base.True
  | Qbircks.Base.CBitVal ((name, length), index) ->
      let register =
        validate_embedded_register registers Classical name length
      in
      let index_int = int_of_z index in
      if index_int < 0 || index_int >= register.register_length then
        raise Unsupported;
      let endpoint = { kind = Classical; register = name; index = index_int } in
      Qbircks.Base.CBitVal ((internal_name bits endpoint, Z.one), Z.zero)
  | Qbircks.Base.Not condition ->
      Qbircks.Base.Not (transform_bool registers bits condition)
  | Qbircks.Base.And (left, right) ->
      Qbircks.Base.And
        (transform_bool registers bits left, transform_bool registers bits right)

let seq_of_list = function
  | [] -> Qbircks.Ast.Skip
  | first :: rest ->
      List.fold_left
        (fun result item -> Qbircks.Ast.Seq (result, item))
        first rest

let supported_gate_arity name =
  match name with
  | "h" | "x" | "z" -> Some 1
  | "cx" | "CX" | "cz" -> Some 2
  | "ccx" -> Some 3
  | _ -> None

let qreg_name = function
  | Qbircks.Base.QCons (name, _) | Qbircks.Base.QIndex ((name, _), _) -> name

let distinct_qregs qregs =
  let names = List.map qreg_name qregs in
  List.length names = List.length (List.sort_uniq String.compare names)

let expand_gate registers bits (gate : Qbircks.Gate.t) =
  let arity =
    match supported_gate_arity gate.name with
    | Some arity -> arity
    | None -> raise Unsupported
  in
  if List.length gate.qreg_params <> arity || gate.params <> [] then
    raise Unsupported;
  let arguments = List.map (expand_qreg registers bits) gate.qreg_params in
  let width =
    match arguments with first :: _ -> List.length first | [] -> 0
  in
  if
    width = 0
    || not
         (List.for_all (fun argument -> List.length argument = width) arguments)
  then raise Unsupported;
  List.init width (fun index ->
      let qreg_params =
        List.map (fun argument -> List.nth argument index) arguments
      in
      if not (distinct_qregs qreg_params) then raise Unsupported;
      Qbircks.Ast.Gate Qbircks.Gate.{ gate with qreg_params })

let rec transform_ast_aux under_if registers bits = function
  | Qbircks.Ast.Skip -> Qbircks.Ast.Skip
  | Qbircks.Ast.InitQReg _ -> raise Unsupported
  | Qbircks.Ast.Seq (left, right) ->
      Qbircks.Ast.Seq
        ( transform_ast_aux under_if registers bits left,
          transform_ast_aux under_if registers bits right )
  | Qbircks.Ast.If (condition, body) ->
      Qbircks.Ast.If
        ( transform_bool registers bits condition,
          transform_ast_aux true registers bits body )
  | Qbircks.Ast.Meas (qreg, creg) ->
      let qregs = expand_qreg registers bits qreg in
      let cregs = expand_creg registers bits creg in
      if List.length qregs <> List.length cregs then raise Unsupported;
      List.map2 (fun qreg creg -> Qbircks.Ast.Meas (qreg, creg)) qregs cregs
      |> seq_of_list
  | Qbircks.Ast.Gate gate ->
      if under_if && String.equal gate.name "h" then raise Unsupported;
      expand_gate registers bits gate |> seq_of_list
  | Qbircks.Ast.SetCReg _ -> raise Unsupported

let transform_ast registers bits ast =
  transform_ast_aux false registers bits ast

let rec ast_size = function
  | Qbircks.Ast.Skip | InitQReg _ | Meas _ | Gate _ | SetCReg _ -> 1
  | Seq (left, right) -> 1 + ast_size left + ast_size right
  | If (_, body) -> 1 + ast_size body

let prepare side output_pairs registers ast =
  if ast_size ast > max_ast_nodes then raise Unsupported;
  let bits = make_bit_names side output_pairs registers in
  let ast = transform_ast registers bits ast |> Qbircks.Ast.remove_skip_seq in
  if ast_size ast > max_ast_nodes then raise Unsupported;
  let program =
    Qbircks.Ast.to_prog Gate_set_impl.Clifford_k.ir_gate_func_map ast
  in
  { bits; program; ast_nodes = ast_size ast }

let paired_input_index side input_pairs endpoint =
  let rec find bit = function
    | [] -> None
    | pair :: rest ->
        if equal_endpoint endpoint (endpoint_on_side side pair) then Some bit
        else find (bit + 1) rest
  in
  find 0 input_pairs

let input_hps side input_pairs value_of_index prepared =
  List.fold_left
    (fun hps (endpoint, name) ->
      let ket =
        match paired_input_index side input_pairs endpoint with
        | Some index -> value_of_index index
        | None -> Hps.Hket.zero
      in
      match endpoint.kind with
      | Quantum -> Hps.add_qmem (name, 0) ket hps
      | Classical -> Hps.add_cmem (name, 0) ket hps)
    Hps.one prepared.bits

let concrete_input_hps side input_pairs valuation prepared =
  input_hps side input_pairs
    (fun index ->
      if valuation land (1 lsl index) = 0 then Hps.Hket.zero else Hps.Hket.one)
    prepared

let symbolic_input_hps side input_pairs prepared =
  input_hps side input_pairs
    (fun index -> Hps.Hket.of_var (Hps.Var.X index))
    prepared

let names_in_mem memory =
  Hps.Mem.fold
    (fun (name, _) _ names -> Hps.Reg_name_set.add name names)
    memory Hps.Reg_name_set.empty

let past_names hps =
  match hps.Hps.output.cmem_stack with
  | [] | [ _ ] -> Hps.Reg_name_set.empty
  | _present :: past ->
      List.fold_left
        (fun names memory -> Hps.Reg_name_set.union names (names_in_mem memory))
        Hps.Reg_name_set.empty past

let discard_past hps =
  let names = past_names hps in
  if Hps.Reg_name_set.is_empty names then hps
  else
    match Rewrite.Discard.check_and_apply ~past_only:true names hps with
    | Ok (kept, _) -> kept
    | Error _ -> raise Unsupported

let observed_names output_pairs =
  List.mapi
    (fun index pair -> canonical_output_name pair.left.kind index)
    output_pairs
  |> List.fold_left
       (fun names name -> Hps.Reg_name_set.add name names)
       Hps.Reg_name_set.empty

let current_cmem hps =
  match hps.Hps.output.cmem_stack with
  | present :: _ -> present
  | [] -> Hps.Mem.empty

let validate_observed_bindings output_pairs hps =
  List.iteri
    (fun index pair ->
      let reg_id = (canonical_output_name pair.left.kind index, 0) in
      let present =
        match pair.left.kind with
        | Quantum -> Hps.Mem.contains_reg reg_id hps.Hps.output.qmem
        | Classical -> Hps.Mem.contains_reg reg_id (current_cmem hps)
      in
      if not present then raise Unsupported)
    output_pairs

let support_as_y_set support =
  Hps.Support.to_list support
  |> List.fold_left
       (fun result index -> Hps.Y_set.add index result)
       Hps.Y_set.empty

let validate_path_support hps =
  let used =
    Hps.Y_set.union
      (Hps.Phase.find_all_y hps.Hps.phase)
      (Hps.Y_set.union
         (Hps.Scalar.find_all_y hps.Hps.scalar)
         (Hps.Output.find_all_y hps.Hps.output))
  in
  if not (Hps.Y_set.subset used (support_as_y_set hps.Hps.support)) then
    raise Unsupported

let add_x_from_var_set variables result =
  let x_variables, _ = Hps.Var_set.partition_xset_yset variables in
  Hps.X_set.union x_variables result

let x_variables_in_mem memory result =
  Hps.Mem.fold
    (fun _ ket result -> add_x_from_var_set (Hps.Hket.find_all_xy ket) result)
    memory result

let x_variables_in_hps hps =
  let from_phase =
    Hps.Phase.fold
      (fun variables _ result -> add_x_from_var_set variables result)
      hps.Hps.phase Hps.X_set.empty
  in
  let from_scalar =
    add_x_from_var_set (Hps.Scalar.find_all_xy hps.Hps.scalar) from_phase
  in
  let from_qmem = x_variables_in_mem hps.Hps.output.qmem from_scalar in
  List.fold_left
    (fun result memory -> x_variables_in_mem memory result)
    from_qmem hps.Hps.output.cmem_stack

let discard_input_dependent_factor keep hps =
  match Rewrite.Fact_distr.Reg.check_and_apply keep hps with
  | Error _ -> raise Unsupported
  | Ok (kept, discarded) ->
      let kept_x = x_variables_in_hps kept in
      let discarded_x = x_variables_in_hps discarded in
      if not (Hps.X_set.disjoint kept_x discarded_x) then raise Unsupported;
      let norm =
        match Hps.norm2_opt discarded with
        | Some norm when not (Hps.Scalar.contains_any_var norm) -> norm
        | _ -> raise Unsupported
      in
      Hps.set_scalar Hps.Scalar.(simp (SMul (Sqrt norm, kept.scalar))) kept

let discard_current unobserved keep hps =
  match Rewrite.Discard.check_and_apply unobserved hps with
  | Ok (kept, _) -> kept
  | Error _ -> discard_input_dependent_factor keep hps

let observe output_pairs hps =
  let hps = Rewrite.reduce_hps hps in
  validate_path_support hps;
  let hps = discard_past hps in
  validate_path_support hps;
  let hps = Rewrite.reduce_hps hps in
  validate_path_support hps;
  validate_observed_bindings output_pairs hps;
  let keep = observed_names output_pairs in
  let all = Hps.Output.find_reg_names hps.output in
  let unobserved = Hps.Reg_name_set.diff all keep in
  let hps =
    if Hps.Reg_name_set.is_empty unobserved then hps
    else discard_current unobserved keep hps
  in
  validate_path_support hps;
  let hps = Rewrite.reduce_hps hps |> Hps.remove_cmem_stack_trailing_voids in
  validate_path_support hps;
  hps

let evaluate input output_pairs prepared =
  Evaluator.evaluate_prog ~rewrite_settings:Evaluator.all_auto
    ~print:Evaluator.Pr_none prepared.program input
  |> observe output_pairs

let evaluate_concrete side input_pairs output_pairs valuation prepared =
  evaluate
    (concrete_input_hps side input_pairs valuation prepared)
    output_pairs prepared

let evaluate_symbolic side input_pairs output_pairs prepared =
  evaluate (symbolic_input_hps side input_pairs prepared) output_pairs prepared

let vector_map hps =
  if Hps.Support.cardinal hps.Hps.support > max_path_variables then
    raise Unsupported;
  Concretization.Vector_map.of_hps hps

let output_spec output_pairs valuation =
  let rec build index qmem cmem = function
    | [] -> (qmem, cmem)
    | pair :: rest -> (
        let ket =
          if valuation land (1 lsl index) = 0 then Hps.Hket.zero
          else Hps.Hket.one
        in
        let reg_id = (canonical_output_name pair.left.kind index, 0) in
        match pair.left.kind with
        | Quantum -> build (index + 1) (Hps.Mem.add reg_id ket qmem) cmem rest
        | Classical -> build (index + 1) qmem (Hps.Mem.add reg_id ket cmem) rest
        )
  in
  let qmem, cmem = build 0 Hps.Mem.empty Hps.Mem.empty output_pairs in
  Hps.Output.make qmem [ cmem ]

let exact_probability spec hps =
  Concretization.hps_proba_output spec hps |> Hps.Scalar.to_q

let has_exact_witness output_pairs left right =
  let event_count = 1 lsl List.length output_pairs in
  let rec loop event =
    if event = event_count then false
    else
      let spec = output_spec output_pairs event in
      match (exact_probability spec left, exact_probability spec right) with
      | Some left_probability, Some right_probability
        when not (Q.equal left_probability right_probability) ->
          true
      | _ -> loop (event + 1)
  in
  loop 0

let symbolic_eq input_pairs output_pairs left right =
  let left_hps = evaluate_symbolic Left input_pairs output_pairs left in
  let right_hps = evaluate_symbolic Right input_pairs output_pairs right in
  Concretization.Vector_map.equal (vector_map left_hps) (vector_map right_hps)

let concrete_neq_witness input_pairs output_pairs left right =
  let valuation_count = 1 lsl List.length input_pairs in
  let rec loop valuation =
    if valuation = valuation_count then false
    else
      let witness =
        try
          let left_hps =
            evaluate_concrete Left input_pairs output_pairs valuation left
          in
          let right_hps =
            evaluate_concrete Right input_pairs output_pairs valuation right
          in
          if
            Hps.Support.cardinal left_hps.Hps.support > max_path_variables
            || Hps.Support.cardinal right_hps.Hps.support > max_path_variables
          then false
          else has_exact_witness output_pairs left_hps right_hps
        with _ -> false
      in
      witness || loop (valuation + 1)
  in
  loop 0

let check_prepared input_pairs output_pairs left right =
  let input_count = List.length input_pairs in
  let output_count = List.length output_pairs in
  let valuations = 1 lsl input_count in
  let events = 1 lsl output_count in
  let work =
    (left.ast_nodes + right.ast_nodes + 1)
    * valuations * (events + 1) * (1 lsl max_path_variables)
  in
  if work > max_work_units then Unknown
  else
    let is_eq =
      try symbolic_eq input_pairs output_pairs left right with _ -> false
    in
    if is_eq then Eq
    else if concrete_neq_witness input_pairs output_pairs left right then Neq
    else Unknown

let has_custom_gate_declaration source =
  source |> String.split_on_char '\n'
  |> List.map (fun line ->
         match String.index_opt line '/' with
         | Some index
           when index + 1 < String.length line && line.[index + 1] = '/' ->
             String.sub line 0 index
         | _ -> line)
  |> String.concat "\n"
  |> String.map (function ';' -> '\n' | char -> char)
  |> String.split_on_char '\n'
  |> List.exists (fun statement ->
         let statement = String.trim statement in
         String.starts_with ~prefix:"gate " statement
         || String.starts_with ~prefix:"opaque " statement)

let check_openqasm2 ~input_pairs ~output_pairs left_source right_source =
  if
    String.length left_source > max_source_bytes
    || String.length right_source > max_source_bytes
  then Ok Unknown
  else
    try
      if List.length input_pairs > max_input_pairs then raise Unsupported;
      if List.length output_pairs > max_output_pairs then raise Unsupported;
      if
        has_custom_gate_declaration left_source
        || has_custom_gate_declaration right_source
      then raise Unsupported;
      let left_parsed =
        Qbircks.Translation.of_openqasm2_with_declarations left_source
      in
      let right_parsed =
        Qbircks.Translation.of_openqasm2_with_declarations right_source
      in
      let left_registers = registers_of_parsed left_parsed in
      let right_registers = registers_of_parsed right_parsed in
      validate_pairs "input" left_registers right_registers input_pairs;
      validate_pairs "output" left_registers right_registers output_pairs;
      let left = prepare Left output_pairs left_registers left_parsed.ast in
      let right = prepare Right output_pairs right_registers right_parsed.ast in
      Ok (check_prepared input_pairs output_pairs left right)
    with
    | Invalid_input message -> Error message
    | Unsupported -> Ok Unknown
    | _ -> Ok Unknown

let read_channel channel =
  let chunk = Bytes.create 4096 in
  let buffer = Buffer.create 4096 in
  let rec loop total =
    let count = In_channel.input channel chunk 0 (Bytes.length chunk) in
    if count = 0 then `Source (Buffer.contents buffer)
    else if total > max_source_bytes - count then `Too_large
    else (
      Buffer.add_subbytes buffer chunk 0 count;
      loop (total + count))
  in
  loop 0

let read_file path =
  try Ok (In_channel.with_open_bin path read_channel)
  with Sys_error message -> Error message

let check_files ~input_pairs ~output_pairs left_path right_path =
  match (read_file left_path, read_file right_path) with
  | Ok (`Source left_source), Ok (`Source right_source) ->
      check_openqasm2 ~input_pairs ~output_pairs left_source right_source
  | Ok `Too_large, _ | _, Ok `Too_large -> Ok Unknown
  | Error message, _ | _, Error message -> Error message
