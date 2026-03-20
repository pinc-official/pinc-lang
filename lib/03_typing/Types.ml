module Diagnostics = Pinc_Diagnostics

module rec Type : sig
  type t =
    | TVar of string
    | TString
    | TChar
    | TInt
    | TFloat
    | TBool
    | TFunction of t list * t
    | TRecord of t StringMap.t
    | TLibrary of t StringMap.t
    | TComponent of t StringMap.t
    | TStore of t StringMap.t
    | TPage of t StringMap.t

  val free_type_variables : t -> StringSet.t
  val apply : t StringMap.t -> t -> t
  val show : t -> string
  val fresh_variable : unit -> t
end = struct
  type t =
    | TVar of string
    | TString
    | TChar
    | TInt
    | TFloat
    | TBool
    | TFunction of t list * t
    | TRecord of t StringMap.t
    | TLibrary of t StringMap.t
    | TComponent of t StringMap.t
    | TStore of t StringMap.t
    | TPage of t StringMap.t

  let rec free_type_variables t =
    match t with
    | TVar n -> StringSet.singleton n
    | TFunction (params, return) ->
        let params =
          List.fold_left
            (fun acc param -> StringSet.union acc (free_type_variables param))
            StringSet.empty
            params
        in
        StringSet.union params (free_type_variables return)
    | TString | TChar | TInt | TFloat | TBool -> StringSet.empty
    | TRecord fields | TLibrary fields | TComponent fields | TStore fields | TPage fields
      ->
        StringMap.fold
          (fun _key t acc -> StringSet.union acc (free_type_variables t))
          fields
          StringSet.empty
  ;;

  let rec apply substitution t =
    match t with
    | TVar n -> (
        match StringMap.find_opt n substitution with
        | None -> TVar n
        | Some t -> t)
    | TFunction (params, return) ->
        let params = List.map (apply substitution) params in
        let return = apply substitution return in
        TFunction (params, return)
    | (TString | TChar | TInt | TFloat | TBool) as t -> t
    | TRecord fields -> TRecord (StringMap.map (apply substitution) fields)
    | TLibrary fields -> TLibrary (StringMap.map (apply substitution) fields)
    | TComponent fields -> TComponent (StringMap.map (apply substitution) fields)
    | TStore fields -> TStore (StringMap.map (apply substitution) fields)
    | TPage fields -> TPage (StringMap.map (apply substitution) fields)
  ;;

  let rec show = function
    | TVar s -> s
    | TString -> "string"
    | TChar -> "char"
    | TInt -> "int"
    | TFloat -> "float"
    | TBool -> "bool"
    | TFunction (params, return) ->
        let params = List.map show params |> String.concat ", " in
        let return = show return in
        Printf.sprintf "fun (%s) -> %s" params return
    | TRecord fields ->
        let fields =
          StringMap.fold
            (fun key t acc -> Printf.sprintf "%s: %s" key (show t) :: acc)
            fields
            []
          |> List.rev
          |> String.concat ", "
        in
        Printf.sprintf "{ %s }" fields
    | TLibrary _ -> "library"
    | TComponent _ -> "component"
    | TStore _ -> "store"
    | TPage _ -> "page"
  ;;

  (* Several operations, for example type scheme instantiation, require fresh names for newly
    introduced type variables. *)
  let fresh_variable =
    let id = ref 0 in
    fun () ->
      incr id;
      TVar ("t" ^ string_of_int !id)
  ;;
end

and Scheme : sig
  type t = Scheme of string list * Type.t

  val free_type_variables : t -> StringSet.t
  val apply : Type.t StringMap.t -> t -> t
end = struct
  type t = Scheme of string list * Type.t

  let free_type_variables (Scheme (vars, t)) =
    StringSet.diff (Type.free_type_variables t) (StringSet.of_list vars)
  ;;

  let apply substitution (Scheme (vars, t)) =
    (* Remove the bound variables from the substitution before applying —
     bound variables are not free, so they must not be substituted *)
    let substitution' = List.fold_right StringMap.remove vars substitution in
    let t' = Type.apply substitution' t in
    Scheme (vars, t')
  ;;
end
