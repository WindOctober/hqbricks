open Hps_equivalence
open Hqbricks

let qasm declarations body =
  "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n" ^ declarations ^ body

let pair text =
  match endpoint_pair_of_string text with
  | Ok pair -> pair
  | Error message -> Alcotest.fail message

let check_verdict expected ~inputs ~outputs left right =
  match
    check_openqasm2 ~input_pairs:(List.map pair inputs)
      ~output_pairs:(List.map pair outputs) left right
  with
  | Ok actual ->
      Alcotest.(check string)
        "verdict"
        (verdict_to_string expected)
        (verdict_to_string actual)
  | Error message -> Alcotest.fail message

let pure_eq () =
  let left = qasm "qreg q[1];\n" "x q[0];\nx q[0];\n" in
  let right = qasm "qreg r[1];\n" "h r[0];\nh r[0];\n" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let pure_neq () =
  let left = qasm "qreg q[1];\n" "x q[0];\nx q[0];\n" in
  let right = qasm "qreg r[1];\n" "x r[0];\n" in
  check_verdict Neq
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let renamed_idle_register () =
  let left = qasm "qreg q[1];\n" "" in
  let right = qasm "qreg renamed[1];\n" "" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:renamed[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:renamed[0]" ]
    left right

let unobserved_separable_output () =
  let left = qasm "qreg q[2];\n" "x q[0];\nx q[0];\nx q[1];\n" in
  let right = qasm "qreg r[2];\n" "h r[0];\nh r[0];\n" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let hybrid_eq () =
  let left =
    qasm "qreg q[2];\ncreg c[1];\n"
      "h q[0];\nmeasure q[0] -> c[0];\nif(c==1) x q[1];\n"
  in
  let right =
    qasm "qreg r[2];\ncreg d[1];\n"
      "h r[0];\nmeasure r[0] -> d[0];\nif(d==1) x r[1];\n"
  in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[1]=quantum:r[1]"; "classical:c[0]=classical:d[0]" ]
    left right

let hybrid_neq () =
  let left =
    qasm "qreg q[2];\ncreg c[1];\n"
      "h q[0];\nmeasure q[0] -> c[0];\nif(c==1) x q[1];\n"
  in
  let right =
    qasm "qreg r[2];\ncreg d[1];\n" "h r[0];\nmeasure r[0] -> d[0];\n"
  in
  check_verdict Neq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[1]=quantum:r[1]"; "classical:c[0]=classical:d[0]" ]
    left right

let classical_input_control () =
  let left = qasm "qreg q[1];\ncreg c[1];\n" "if(c==1) x q[0];\n" in
  let right = qasm "qreg r[1];\ncreg d[1];\n" "if(d==1) x r[0];\n" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "classical:c[0]=classical:d[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]"; "classical:c[0]=classical:d[0]" ]
    left right

let unsupported_gate () =
  let left = qasm "qreg q[1];\n" "y q[0];\n" in
  let right = qasm "qreg r[1];\n" "y r[0];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let entangled_discard_is_unknown () =
  let left = qasm "qreg q[2];\n" "h q[0];\ncx q[0],q[1];\n" in
  let right = qasm "qreg r[2];\n" "h r[0];\ncx r[0],r[1];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let global_phase_is_conservative () =
  let left = qasm "qreg q[1];\n" "x q[0];\nz q[0];\nx q[0];\nz q[0];\n" in
  let right = qasm "qreg r[1];\n" "x r[0];\nx r[0];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let path_alpha_order () =
  let left = qasm "qreg q[2];\n" "h q[0];\nh q[1];\n" in
  let right = qasm "qreg r[2];\n" "h r[1];\nh r[0];\n" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    left right

let custom_gate_declaration () =
  let left = qasm "gate foo a { x a; }\nqreg q[1];\n" "x q[0];\n" in
  let right = qasm "qreg r[1];\n" "x r[0];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let ignored_foreign_include () =
  let left =
    "OPENQASM 2.0;\n\
     include \"qelib1.inc\";\n\
     include \"other.inc\";\n\
     qreg q[1];\n\
     x q[0];\n"
  in
  let right = qasm "qreg r[1];\n" "x r[0];\n" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let wrong_version () =
  let left = "OPENQASM 3.0;\ninclude \"qelib1.inc\";\nqreg q[1];\nx q[0];\n" in
  let right = qasm "qreg r[1];\n" "x r[0];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let out_of_range_condition () =
  let left = qasm "qreg q[1];\ncreg c[2];\n" "if(c==4) x q[0];\n" in
  let right = qasm "qreg r[1];\ncreg d[2];\n" "" in
  check_verdict Unknown
    ~inputs:
      [
        "quantum:q[0]=quantum:r[0]";
        "classical:c[0]=classical:d[0]";
        "classical:c[1]=classical:d[1]";
      ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let reset_is_unknown () =
  let left = qasm "qreg q[2];\n" "h q[0];\ncx q[0],q[1];\nreset q[1];\n" in
  let right = qasm "qreg r[2];\n" "h r[0];\nreset r[1];\n" in
  check_verdict Unknown ~inputs:[]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let ignored_include_and_barrier () =
  let right = qasm "qreg r[1];\n" "x r[0];\n" in
  let cases =
    [
      "OPENQASM 2.0;\nqreg q[1];\nx q[0];\n";
      "OPENQASM 2.0;\nqreg q[1];\ninclude \"qelib1.inc\";\nx q[0];\n";
      "OPENQASM 2.0;\ninclude \"qelib1.inc\" garbage;\nqreg q[1];\nx q[0];\n";
      "OPENQASM 2.0;\n\
       include \"qelib1.inc\";\n\
       qreg q[1];\n\
       barrier missing;\n\
       x q[0];\n";
    ]
  in
  List.iter
    (fun left ->
      check_verdict Eq
        ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
        ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
        left right)
    cases

let measured_quantum_output () =
  let left =
    qasm "qreg q[1];\ncreg c[1];\n" "h q[0];\nmeasure q[0] -> c[0];\n"
  in
  let right =
    qasm "qreg r[1];\ncreg d[1];\n" "h r[0];\nmeasure r[0] -> d[0];\n"
  in
  check_verdict Unknown ~inputs:[]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let rotation_is_unknown () =
  let left = qasm "qreg q[1];\n" "rz(pi/2) q[0];\n" in
  let right = qasm "qreg r[1];\n" "rz(pi/2) r[0];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let controlled_h_is_unknown () =
  let left = qasm "qreg q[2];\n" "ch q[0],q[1];\n" in
  let right = qasm "qreg r[2];\n" "ch r[0],r[1];\n" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    left right

let conditional_h_is_unknown () =
  let left = qasm "qreg q[1];\ncreg c[1];\n" "if(c==1) h q[0];\n" in
  let right = qasm "qreg r[1];\ncreg d[1];\n" "if(d==1) h r[0];\n" in
  check_verdict Unknown
    ~inputs:[ "classical:c[0]=classical:d[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let duplicate_gate_operand () =
  let left = qasm "qreg q[1];\n" "cx q[0],q[0];\n" in
  let right = qasm "qreg r[1];\n" "" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let whole_register_expansion () =
  let left = qasm "qreg q[2];\n" "h q;\nh q;\n" in
  let right = qasm "qreg r[2];\n" "x r;\nx r;\n" in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    left right

let distinct_input_and_output_permutations () =
  let left = qasm "qreg q[2];\n" "" in
  let right =
    qasm "qreg r[2];\n" "cx r[0],r[1];\ncx r[1],r[0];\ncx r[0],r[1];\n"
  in
  check_verdict Eq
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[1]"; "quantum:q[1]=quantum:r[0]" ]
    left right

let duplicate_pair_is_invalid () =
  let source = qasm "qreg q[1];\n" "" in
  let other = qasm "qreg r[1];\n" "" in
  let duplicate = pair "quantum:q[0]=quantum:r[0]" in
  match
    check_openqasm2 ~input_pairs:[ duplicate; duplicate ]
      ~output_pairs:[ duplicate ] source other
  with
  | Error _ -> ()
  | Ok verdict ->
      Alcotest.failf "expected invalid mapping, got %s"
        (verdict_to_string verdict)

let negative_endpoint_is_invalid () =
  let source = qasm "qreg q[1];\n" "" in
  let other = qasm "qreg r[1];\n" "" in
  let negative =
    {
      left = { kind = Quantum; register = "q"; index = -1 };
      right = { kind = Quantum; register = "r"; index = -1 };
    }
  in
  match
    check_openqasm2 ~input_pairs:[ negative ] ~output_pairs:[] source other
  with
  | Error _ -> ()
  | Ok verdict ->
      Alcotest.failf "expected invalid negative endpoint, got %s"
        (verdict_to_string verdict)

let input_pair_cap () =
  let left = qasm "qreg q[9];\n" "" in
  let right = qasm "qreg r[9];\n" "" in
  let inputs =
    List.init 9 (fun index ->
        Printf.sprintf "quantum:q[%d]=quantum:r[%d]" index index)
  in
  check_verdict Unknown ~inputs
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let classical_history_is_unobserved () =
  let left =
    qasm "qreg q[2];\ncreg c[1];\n"
      "x q[0];\nmeasure q[0] -> c[0];\nmeasure q[1] -> c[0];\n"
  in
  let right = qasm "qreg r[2];\ncreg d[1];\n" "measure r[1] -> d[0];\n" in
  check_verdict Eq ~inputs:[]
    ~outputs:[ "classical:c[0]=classical:d[0]" ]
    left right

let orphaned_history_path_is_unknown () =
  let left =
    qasm "qreg q[2];\ncreg c[2];\n"
      "h q[0];\nh q[1];\nmeasure q[0] -> c[0];\nmeasure q[1] -> c[1];\n"
  in
  let right =
    qasm "qreg r[2];\ncreg d[2];\n"
      "x r[0];\nmeasure r[0] -> d[0];\nh r[1];\nmeasure r[1] -> d[1];\n"
  in
  check_verdict Unknown ~inputs:[]
    ~outputs:[ "classical:c[0]=classical:d[0]" ]
    left right

let inverse_circuit_concretization () =
  let left =
    qasm "qreg q[2];\n"
      ("h q[0];\ncx q[0],q[1];\nh q[1];\ncx q[1],q[0];\nh q[0];\n"
     ^ "h q[0];\ncx q[1],q[0];\nh q[1];\ncx q[0],q[1];\nh q[0];\n")
  in
  let right = qasm "qreg r[2];\n" "" in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]"; "quantum:q[1]=quantum:r[1]" ]
    left right

let mixed_phase_concretization_sign () =
  let half = Hps.Dyadic1.make Z.one 1 in
  let actual =
    Hps.(
      one |> add_phase [ Var.Y 1 ] half
      |> add_phase [ Var.Y 0; Var.Y 1 ] half
      |> add_qmem ("q", 0) (Hket.of_var (Var.Y 1))
      |> add_support [ 0; 1 ])
    |> Concretization.Vector_map.of_hps
  in
  let expected =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.Mem.(empty |> add ("q", 0) Hps.Hket.zero)
           (Hps.Phase.zero, Hps.Scalar.make_int (Z.of_int 2)))
  in
  Alcotest.(check bool)
    "opposite-phase paths cancel with the correct sign" true
    (Concretization.Vector_map.equal expected actual)

let preflight_work_cap () =
  let repeated_x register =
    List.init 80 (fun _ -> "x " ^ register ^ "[0];\n") |> String.concat ""
  in
  let left = qasm "qreg q[1];\n" (repeated_x "q") in
  let right = qasm "qreg r[1];\n" (repeated_x "r") in
  check_verdict Unknown
    ~inputs:[ "quantum:q[0]=quantum:r[0]" ]
    ~outputs:[ "quantum:q[0]=quantum:r[0]" ]
    left right

let () =
  Alcotest.run "HPS equivalence"
    [
      ( "verdicts",
        [
          Alcotest.test_case "pure eq and register rename" `Quick pure_eq;
          Alcotest.test_case "exact neq witness" `Quick pure_neq;
          Alcotest.test_case "unused declared register" `Quick
            renamed_idle_register;
          Alcotest.test_case "unobserved separable output" `Quick
            unobserved_separable_output;
          Alcotest.test_case "hybrid measurement and control eq" `Quick
            hybrid_eq;
          Alcotest.test_case "hybrid exact neq witness" `Quick hybrid_neq;
          Alcotest.test_case "classical paired input" `Quick
            classical_input_control;
          Alcotest.test_case "unsupported gate" `Quick unsupported_gate;
          Alcotest.test_case "entangled discard" `Quick
            entangled_discard_is_unknown;
          Alcotest.test_case "global phase" `Quick global_phase_is_conservative;
          Alcotest.test_case "path alpha order" `Quick path_alpha_order;
          Alcotest.test_case "custom gate declaration" `Quick
            custom_gate_declaration;
          Alcotest.test_case "ignored foreign include" `Quick
            ignored_foreign_include;
          Alcotest.test_case "wrong OpenQASM version" `Quick wrong_version;
          Alcotest.test_case "out-of-range condition" `Quick
            out_of_range_condition;
          Alcotest.test_case "reset" `Quick reset_is_unknown;
          Alcotest.test_case "ignored include and barrier" `Quick
            ignored_include_and_barrier;
          Alcotest.test_case "measured quantum output" `Quick
            measured_quantum_output;
          Alcotest.test_case "rotation" `Quick rotation_is_unknown;
          Alcotest.test_case "controlled H" `Quick controlled_h_is_unknown;
          Alcotest.test_case "conditional H" `Quick conditional_h_is_unknown;
          Alcotest.test_case "duplicate gate operand" `Quick
            duplicate_gate_operand;
          Alcotest.test_case "whole-register expansion" `Quick
            whole_register_expansion;
          Alcotest.test_case "distinct input/output permutations" `Quick
            distinct_input_and_output_permutations;
          Alcotest.test_case "duplicate endpoint pair" `Quick
            duplicate_pair_is_invalid;
          Alcotest.test_case "negative API endpoint" `Quick
            negative_endpoint_is_invalid;
          Alcotest.test_case "input-pair resource cap" `Quick input_pair_cap;
          Alcotest.test_case "classical history" `Quick
            classical_history_is_unobserved;
          Alcotest.test_case "path-support closure after history discard" `Quick
            orphaned_history_path_is_unknown;
          Alcotest.test_case "inverse circuit concretization" `Quick
            inverse_circuit_concretization;
          Alcotest.test_case "mixed phase concretization sign" `Quick
            mixed_phase_concretization_sign;
          Alcotest.test_case "preflight work cap" `Quick preflight_work_cap;
        ] );
    ]
