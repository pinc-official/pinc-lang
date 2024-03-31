module Position = struct
  type t = {
    source : Pinc_Source.t;
    line : int;
    column : int;
  }

  let make ~source ~line ~column = { source; line; column }
  let get_line pos = pos.line
  let get_column pos = pos.column
  let get_source pos = pos.source
end

type t = {
  loc_start : Position.t;
  loc_end : Position.t;
}

let merge ~s ~e () = { loc_start = s.loc_start; loc_end = e.loc_end }

let make ?e ~s () =
  match e with
  | None -> { loc_start = s; loc_end = s }
  | Some e -> { loc_start = s; loc_end = e }
;;

let none =
  {
    loc_start = Position.make ~source:Pinc_Source.empty ~line:0 ~column:0;
    loc_end = Position.make ~source:Pinc_Source.empty ~line:0 ~column:0;
  }
;;

let get_source loc = loc.loc_start.source
let get_start loc = loc.loc_start
let get_end loc = loc.loc_end

let pp ppf loc =
  match (Pinc_Source.name loc.loc_start.source, loc = none) with
  | None, _ | _, true -> ()
  | Some filename, _ ->
      let loc_string =
        if loc.loc_start.line = loc.loc_end.line then
          if loc.loc_start.column = loc.loc_end.column then
            Format.sprintf "%i:%i" loc.loc_start.line loc.loc_start.column
          else
            Format.sprintf
              "%i:%i-%i"
              loc.loc_start.line
              loc.loc_start.column
              loc.loc_end.column
        else
          Format.sprintf
            "%i:%i-%i:%i"
            loc.loc_start.line
            loc.loc_start.column
            loc.loc_end.line
            loc.loc_end.column
      in
      Fmt.pf
        ppf
        "%a"
        Fmt.(styled `Faint string)
        (Printf.sprintf "in file %s:%s" filename loc_string)
;;
