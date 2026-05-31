type t

(** Build a dependency graph from a parsed AST. *)
val build : Pinc_Types.Ast.t -> t

(** Direct dependencies of a declaration. *)
val dependencies_of : t -> string -> StringSet.t

(** All declarations needed to compile a given declaration. *)
val transitive_dependencies_of : t -> string -> StringSet.t
