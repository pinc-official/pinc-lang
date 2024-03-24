module Location = Pinc_Location

let set_renderer ppf =
  match Sys.getenv_opt "NO_COLOR" with
  | None | Some "" -> Fmt.set_style_renderer ppf `Ansi_tty
  | Some _ -> Fmt.set_style_renderer ppf `None
;;

let error location message =
  let ppf = Format.err_formatter in
  set_renderer ppf;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (Pinc_Printer.print ~kind:`error) location message;
  exit 1
;;

let warn location message =
  let ppf = Format.err_formatter in
  set_renderer ppf;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (Pinc_Printer.print ~kind:`warning) location message
;;
