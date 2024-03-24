type t = {
  source : Pinc_Core.Source.t;
  line : int;
  beginning_of_line : int;
  column : int;
}

let make ~source ~line ~column = { source; line; beginning_of_line = 0; column }
