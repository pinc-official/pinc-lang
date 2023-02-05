module Location = Pinc_Location
module Position = Pinc_Position

let error ~start_pos ~end_pos message =
  let location = Location.make start_pos end_pos in
  let ppf = Format.err_formatter in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (Pinc_Printer.print ~kind:`error) location message;
  exit 1
;;

let warn ~start_pos ~end_pos message =
  let location = Location.make start_pos end_pos in
  let ppf = Format.err_formatter in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (Pinc_Printer.print ~kind:`error) location message
;;
