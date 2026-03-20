open Types

(* The function generalize abstracts a type over all type variables which are free in the type but
not free in the given type environment. *)
let generalize env t =
  let free_t = Type.free_type_variables t in
  let free_env = Environment.free_type_variables env in
  let vars = StringSet.to_list @@ StringSet.diff free_t free_env in
  Scheme.Scheme (vars, t)
;;

(* The instantiation function replaces all bound type variables in a type scheme with fresh type
variables. *)
let instantiate (Scheme.Scheme (vars, t)) =
  let vars' = List.map (fun _ -> Type.fresh_variable ()) vars in
  let s = StringMap.of_list @@ List.combine vars vars' in
  Type.apply s t
;;
