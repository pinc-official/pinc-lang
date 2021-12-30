open Fennek_lib
open Printf

let rec print_ast (ast : Ast.t) =
  print_location ast.loc;
  ast.declarations |> List.iter print_declaration

and print_declaration decl =
  match decl with
  | Ast.Declaration.Site s -> print_site s
  | Ast.Declaration.Store s -> print_store s
  | Ast.Declaration.Page p -> print_page p
  | Ast.Declaration.Component c -> print_component c

and print_site site =
  printf "site %s\n" site.ident;
  site.symbols |> List.iter (print_symbol ~ind:2)

and print_store store =
  printf "store %s\n" store.ident;
  store.symbols |> List.iter (print_symbol ~ind:2)

and print_page page =
  printf "page %s\n" page.ident;
  page.symbols |> List.iter (print_symbol ~ind:2)

and print_component component =
  printf "component %s\n" component.ident;
  component.symbols |> List.iter (print_symbol ~ind:2)

and print_symbol ~ind symbol =
  let indent = String.make ind ' ' in
  printf "%ssymbol %s (\n" indent symbol.Ast.Symbol.ident;
  symbol.attrs |> List.iter (print_attr ~ind:(ind + 2));
  printf "%s)\n" indent;

and print_attr ~ind attr =
  let indent = String.make ind ' ' in
  printf "%sattr %s\n" indent attr.Ast.Attr.name;
  attr.value |> print_expr ~ind:(ind + 2)

and print_expr ~ind expr =
  let indent = String.make ind ' ' in
  match expr with
  | Ast.Value.String s -> printf "%sstring %s\n" indent s
  | Ast.Value.Float f -> printf "%sfloat %f\n" indent f
  | Ast.Value.Int i -> printf "%sint %i\n" indent i
  | Ast.Value.Bool b -> printf "%sbool %b\n" indent b
  | Ast.Value.Symbol s -> print_symbol ~ind:(ind + 2) s;
  | Ast.Value.Array items ->
    printf "%sarray [\n" indent;
    items |> List.iter (print_expr ~ind:(ind + 2));
    printf "%s]\n" indent;

and print_location pos =
  let fname = pos.Position.filename in
  let lnum = pos.line in
  let col = pos.column in
  Printf.printf "%s:%d:%d\n" fname lnum col

let parse_and_print p = print_ast (Parser.scan p)

let file_contents chan = really_input_string chan (in_channel_length chan)

let main =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let src = file_contents chan in
  let parser = Parser.make ~filename src in
  let () = parse_and_print parser in
  close_in chan

let () = main