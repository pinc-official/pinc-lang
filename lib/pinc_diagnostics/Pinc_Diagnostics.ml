module Location = Pinc_Location

let error location message =
  let ppf = Format.err_formatter in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (Pinc_Printer.print ~kind:`error) location message;
  exit 1
;;

let warn location message =
  let ppf = Format.err_formatter in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (Pinc_Printer.print ~kind:`warning) location message
;;
