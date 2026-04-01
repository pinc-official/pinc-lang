let format sources =
  sources |> Pinc_Parser.parse ~include_stdlib:false |> Formatter.format
;;
