open Ast

val optimize : stmt -> stmt
val typecheck : stmt -> bool
val infer : stmt -> stmt
