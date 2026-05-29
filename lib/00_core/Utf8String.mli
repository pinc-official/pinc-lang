type t

val n_chars : t -> int
val to_string : t -> string
val to_list : ?idx:int -> t -> Uchar.t list
val of_list : Uchar.t list -> t
val of_string_exn : string -> t
val of_string : string -> t option
