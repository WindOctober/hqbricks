(**************************************************************************)
(*  This file is part of HQbricks.                                        *)
(*                                                                        *)
(*  Copyright (C) 2026                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    Université Paris-Saclay                                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.0.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3.0                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

(** QbIRcks-OpenQASM2 translations. *)

type parsed = {
  ast : Ast.t;
  qregs : Base.reg_id list;
  cregs : Base.reg_id list;
}
(** A parsed AST and its declarations. Declaration lists preserve source order
    and include unused registers. *)

(** Parsing uses module-global declaration tables, so calls to OpenQASM parsing
    functions in this module must not overlap. *)

val of_openqasm2_with_declarations : string -> parsed
val of_openqasm2 : string -> Ast.t
val of_openqasm2_file_with_declarations : string -> parsed
val of_openqasm2_file : string -> Ast.t
val to_openqasm2 : Ast.t -> string
