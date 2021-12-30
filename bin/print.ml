open Fennek_lib
open Printf

let rec print_ast (ast : Ast.t) =
  print_location ast.loc;
  ast.declarations |> List.iter print_declaration

and print_declaration decl =
  match decl with
  | Ast_Declaration.Site s -> print_site s
  | Ast_Declaration.Store s -> print_store s
  | Ast_Declaration.Page p -> print_page p
  | Ast_Declaration.Component c -> print_component c

and print_site site =
  printf "site %s\n" site.ident;
  site.symbols |> List.iter print_symbol

and print_store store =
  printf "store %s\n" store.ident;
  store.symbols |> List.iter print_symbol

and print_page page =
  printf "page %s\n" page.ident;
  page.symbols |> List.iter print_symbol

and print_component component =
  printf "component %s\n" component.ident;
  component.symbols |> List.iter print_symbol

and print_symbol symbol =
  printf "symbol %s\n" symbol.ident;
  symbol.attrs |> List.iter print_attr

and print_attr attr =
  printf "attr %s\n" attr.name;
  print_expr attr.value

and print_expr expr =
  match expr with
  | Ast_Value.String s -> printf "string %s\n" s
  | Ast_Value.Float f -> printf "float %f\n" f
  | Ast_Value.Int i -> printf "int %i\n" i
  | Ast_Value.Bool b -> printf "bool %b\n" b
  | Ast_Value.Array items ->
    printf "array [\n";
    List.iter (fun v -> (
      printf "  "; 
      print_expr v
    )) items;
    printf "]\n";

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