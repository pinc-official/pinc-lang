module Position = Pinc_Position

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
    loc_start = Position.make ~source:Pinc_Core.Source.empty ~line:0 ~column:0;
    loc_end = Position.make ~source:Pinc_Core.Source.empty ~line:0 ~column:0;
  }
;;
