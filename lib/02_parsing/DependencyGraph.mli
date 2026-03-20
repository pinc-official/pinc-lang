(* DependencyGraph.mli
   Syntactic dependency graph over Pinc declarations. *)

(** An opaque graph mapping declaration names to their syntactic dependencies. *)
type t

type group =
  | Single of string
      (** A declaration with no mutual recursion. May still be self-recursive. *)
  | Recursive of string list
      (** A group of mutually recursive declarations. Must be inferred together. *)

(** Build a dependency graph from a parsed AST. *)
val build : Ast.t -> t

(** Direct syntactic dependencies of a named declaration. *)
val dependencies_of : t -> string -> StringSet.t

(** All declarations needed to compile a given declaration, recursively. Useful for subset
    loading — load only what a component actually needs. *)
val transitive_dependencies_of : t -> string -> StringSet.t

(** Returns declaration groups in dependency order — a group only appears after all groups
    it depends on. Mutually recursive declarations are bundled into a single Recursive
    group. Used by the type checker to determine inference order. *)
val topo_sorted_groups : t -> group list
