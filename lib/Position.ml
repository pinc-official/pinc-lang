type t = {
  filename: string;
  line: int;
  column: int;
}
[@@deriving show { with_path = false }]

let make ~filename ~line ~column = {
  filename;
  line;
  column;
}
