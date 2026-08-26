open Hqbricks

let mixed_phase_addition () =
  let open Hps in
  let memory = Mem.empty in
  let history = Mem_stack.empty in
  let zero = Phase.zero in
  let half = Phase.one_half in
  let scalar n = Scalar.make_int (Z.of_int n) in
  let expected =
    Concretization.Vector_map.(empty |> add history memory (zero, scalar 2))
  in
  let zero_then_half =
    Concretization.Vector_map.(
      empty
      |> add history memory (zero, scalar 2)
      |> add history memory (half, scalar 1)
      |> add history memory (zero, scalar 1))
  in
  let half_then_zero =
    Concretization.Vector_map.(
      empty
      |> add history memory (half, scalar 1)
      |> add history memory (zero, scalar 2)
      |> add history memory (zero, scalar 1))
  in
  Alcotest.(check bool)
    "zero then half" true
    (Concretization.Vector_map.equal expected zero_then_half);
  Alcotest.(check bool)
    "half then zero" true
    (Concretization.Vector_map.equal expected half_then_zero)

let () =
  Alcotest.run "Concretization"
    [
      ( "Vector map",
        [
          Alcotest.test_case "mixed phase addition preserves sign" `Quick
            mixed_phase_addition;
        ] );
    ]
