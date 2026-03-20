(* 
  This is the unification function for types. For two types t1 and t2, most_general_unifier(t1, t2) returns the most
  general unifier. A unifier is a substitution S such that S(t1) == S(t2).
*)
let rec most_general_unifier t1 t2 =
  match (t1, t2) with
  | Types.Type.TVar v, t -> var_bind v t
  | t, Types.Type.TVar v -> var_bind v t
  | Types.Type.TString, Types.Type.TString -> Substitution.empty
  | Types.Type.TChar, Types.Type.TChar -> Substitution.empty
  | Types.Type.TInt, Types.Type.TInt -> Substitution.empty
  | Types.Type.TFloat, Types.Type.TFloat -> Substitution.empty
  | Types.Type.TBool, Types.Type.TBool -> Substitution.empty
  | Types.Type.TFunction (params1, ret1), Types.Type.TFunction (params2, ret2) ->
      if List.length params1 <> List.length params2 then
        failwith
          (Printf.sprintf
             "Types do not match. Expected `%s` got `%s`."
             (Types.Type.show t1)
             (Types.Type.show t2));
      let subst =
        List.fold_left2
          (fun subst p1 p2 ->
            let s =
              most_general_unifier (Types.Type.apply subst p1) (Types.Type.apply subst p2)
            in
            Substitution.compose s subst)
          Substitution.empty
          params1
          params2
      in
      let s =
        most_general_unifier (Types.Type.apply subst ret1) (Types.Type.apply subst ret2)
      in
      Substitution.compose s subst
  | Types.Type.TRecord f1, Types.Type.TRecord f2 -> unify_fields f1 f2
  | Types.Type.TLibrary f1, Types.Type.TLibrary f2 -> unify_fields f1 f2
  | Types.Type.TComponent f1, Types.Type.TComponent f2 -> unify_fields f1 f2
  | Types.Type.TStore f1, Types.Type.TStore f2 -> unify_fields f1 f2
  | Types.Type.TPage f1, Types.Type.TPage f2 -> unify_fields f1 f2
  | _ ->
      failwith
        (Printf.sprintf
           "Types do not match. Expected `%s` got `%s`."
           (Types.Type.show t1)
           (Types.Type.show t2))

(* Unify two field maps pairwise, composing substitutions *)
and unify_fields f1 f2 =
  StringMap.fold
    (fun key t1 subst ->
      match StringMap.find_opt key f2 with
      | None -> failwith (Printf.sprintf "record/declaration has no field: %s" key)
      | Some t2 ->
          let s =
            most_general_unifier (Types.Type.apply subst t1) (Types.Type.apply subst t2)
          in
          Substitution.compose s subst)
    f1
    Substitution.empty

(* The function var_bind attempts to bind a type variable to a type and return that binding as a subsitution, 
   but avoids binding a variable to itself and performs the occurs check, which is responsible for circularity
   type errors. *)
and var_bind var t =
  match t with
  | Types.Type.TVar v when var = v -> Substitution.empty
  | _ when StringSet.mem var (Types.Type.free_type_variables t) ->
      failwith (Printf.sprintf "occurs check fails: %s vs %s" var (Types.Type.show t))
  | _ -> StringMap.singleton var t
;;
