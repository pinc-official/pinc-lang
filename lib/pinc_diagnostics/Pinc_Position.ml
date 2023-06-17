type t = {
  filename : string;
  line : int;
  beginning_of_line : int;
  column : int;
}

let make ~filename ~line ~column = { filename; line; beginning_of_line = 0; column }
