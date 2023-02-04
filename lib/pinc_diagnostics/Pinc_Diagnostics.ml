module Location = Pinc_Location
module Position = Pinc_Position

let report ~src ~start_pos ~end_pos message =
  let location = Location.make start_pos end_pos in
  Format.fprintf Format.err_formatter "@[<v>";
  Pinc_Printer.Super_location.super_error_reporter
    Format.err_formatter
    src
    location
    message;
  Format.fprintf Format.err_formatter "@]@.";
  exit 1
;;
