type primitive_value =
  [ `String of string
  | `Int of int
  | `Float of float
  | `Boolean of bool
  ]

type public_tag_typ =
  [ `String
  | `Int
  | `Float
  | `Boolean
  | `Array
  | `Record
  | `Slot
  | `Store
  | `Custom of string
  ]

type public_tag = {
  public_tag_key : string;
  public_tag_typ : public_tag_typ;
  public_tag_children : public_tag list option;
  public_tag_initial_value : primitive_value option;
}

val transform : Parsetree.t -> Ast.t
val all_tags : string -> Parsetree.t -> public_tag list
