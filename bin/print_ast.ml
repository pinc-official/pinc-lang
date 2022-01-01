open Fennek_lib
open Printf

let rec print_ast (ast : Ast.File.t) =
  print_location ast.location;
  ast.declarations |> List.iter print_declaration

and print_declaration decl =
  match decl with
  | Ast.Declaration.Site s -> print_site s
  | Ast.Declaration.Store s -> print_store s
  | Ast.Declaration.Page p -> print_page p
  | Ast.Declaration.Component c -> print_component c

and print_site site =
  printf "site %s\n" site.identifier;
  site.attributes |> List.iter (print_symbol ~ind:2)

and print_store store =
  printf "store %s\n" store.identifier;
  store.attributes |> List.iter (print_symbol ~ind:2)

and print_page page =
  printf "page %s\n" page.identifier;
  page.attributes |> List.iter (print_symbol ~ind:2)

and print_component component =
  printf "component %s\n" component.identifier;
  component.attributes |> List.iter (print_symbol ~ind:2)

and print_symbol ~ind symbol =
  let indent = String.make ind ' ' in
  printf "%ssymbol %s (\n" indent symbol.Ast.Symbol.identifier;
  symbol.attributes |> List.iter (print_attr ~ind:(ind + 2));
  printf "%s)\n" indent;

and print_attr ~ind attr =
  let indent = String.make ind ' ' in
  printf "%sattr %s\n" indent attr.Ast.Attribute.key;
  attr.value |> print_expr ~ind:(ind + 2)

and print_expr ~ind expr =
  let indent = String.make ind ' ' in
  match expr with
  | Ast.Expression.Literal l -> print_literal ~ind l;
  | Ast.Expression.Symbols s -> s |> List.iter (print_symbol ~ind);
  | Ast.Expression.Array items ->
    printf "%sarray [\n" indent;
    items |> List.iter (print_expr ~ind:(ind + 2));
    printf "%s]\n" indent;
  | _ -> () (* TODO: *)

and print_literal ~ind lit =
  let indent = String.make ind ' ' in
  match lit with
  | Ast.Literal.String s -> printf "%sstring %s\n" indent s
  | Ast.Literal.Float f -> printf "%sfloat %f\n" indent f
  | Ast.Literal.Int i -> printf "%sint %i\n" indent i
  | Ast.Literal.Bool b -> printf "%sbool %b\n" indent b

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