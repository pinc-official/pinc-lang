open Js_of_ocaml

let () =
  Js.export "pinc_format" (fun source ->
      let source = Pinc_lang.Source.of_string source in
      Pinc_lang.Formatter.format [ source ])
;;
