type public_tag = {
  public_tag_key : string;
  public_tag_typ : Ast.tag_typ;
  public_tag_children : public_tag list option;
}

val transform : Parsetree.t -> Ast.t
val all_tags : string -> Parsetree.t -> public_tag list
