type t = {
  filename: string;
  line: int;
  column: int;
}

let make ~filename ~line ~column = { filename; line; column }
