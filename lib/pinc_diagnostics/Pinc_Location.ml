module Position = Pinc_Position

type t = {
  loc_start : Position.t;
  loc_end : Position.t;
}

let make ?e ~s () =
  match e with
  | None -> { loc_start = s; loc_end = s }
  | Some e -> { loc_start = s; loc_end = e }
;;
