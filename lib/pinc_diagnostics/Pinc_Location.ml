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

type 'a with_location = 'a * t

let add : t -> 'a -> 'a with_location = fun loc a -> (a, loc)
let get : 'a with_location -> t = snd
let get_data : 'a with_location -> 'a = fst
