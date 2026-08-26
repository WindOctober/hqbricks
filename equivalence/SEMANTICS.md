# HQbricks HPS equivalence semantics

## Program interface

The checker compares two OpenQASM 2 programs using explicit input and output
pairs.

- A quantum input pair shares one HPS input variable.
- A classical input pair shares one initial Boolean variable.
- Declared bits without an input pair start at zero.
- Only listed output pairs are compared.
- Quantum and classical endpoints cannot be paired with each other.
- Output pairs may describe a permutation or register renaming.

Input and output pairings are independent: input pairs describe the initial
correspondence, while output pairs describe the observations to compare.

## Equivalence procedure

1. Parse both programs with QbIRcks and retain their register declarations.
2. Expand supported whole-register operations into bit operations and validate
   register widths, gate arities, and pair metadata.
3. Rename paired outputs to shared internal names and keep all other bits
   distinct between the two programs.
4. Lower both programs through the existing QbIRcks-to-HQbricks Clifford-k
   translation and evaluate them as HPS expressions.
5. Remove classical history and unobserved outputs using HPS discard and
   factorization rules.
6. Concretize the remaining bounded path variables and compare the resulting
   HPS vector maps exactly.

Includes and barriers follow the existing QbIRcks behavior and are ignored.
Custom gate declarations, opaque gates, and reset are not supported.

## Results

- `eq`: the observed, concretized HPS vector maps are exactly equal.
- `neq`: a computational-basis input produces different exact observed output
  probabilities.
- `unknown`: the programs are outside the supported subset or neither test
  establishes a result.

Structural HPS inequality alone does not produce `neq`; it must be confirmed by
an exact probability witness.

## Observation details

- Unlisted outputs are discarded when the HPS discard or factorization rules
  can remove them.
- Measurement removes the measured quantum bit and records its value in
  classical memory. Such a quantum bit cannot subsequently be selected as an
  output.
- Only the current classical-memory snapshot is observable.
- Path-variable names are eliminated by concretization, so alpha-renaming does
  not affect comparison.
- No independent global-phase quotient is applied.

## Supported subset

The supported gates are parameter-free `h`, `x`, `z`, `cx`/`CX`, `cz`, and
`ccx`, with exact arity. Measurement and supported classical conditions are
handled by the existing HPS evaluator. Conditional `h`, `ch`, rotations, and
other parameterized gates are currently unsupported by the equivalence
checker.

The current limits are:

- 8 input pairs and 8 output pairs;
- 64 declared bits per program;
- 10,000 AST nodes before and after expansion;
- 1,000,000 source bytes;
- 12 HPS path variables;
- 5,000,000 estimated symbolic/witness work units.

Exceeding a limit returns `unknown`.
