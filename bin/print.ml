open Fennek_lang
open Lexing
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
  printf "site %S\n" site.ident;
  site.decorators |> List.iter print_decorator

and print_store store =
  printf "store %S\n" store.ident;
  store.decorators |> List.iter print_decorator

and print_page page =
  printf "page %S\n" page.ident;
  page.decorators |> List.iter print_decorator

and print_component component =
  printf "component %S\n" component.ident;
  component.decorators |> List.iter print_decorator

and print_decorator decorator =
  printf "decorator %S\n" decorator.ident;
  decorator.attrs |> List.iter print_attr

and print_attr attr =
  printf "attr %S\n" attr.name;
  print_expr attr.value

and print_expr expr =
  match expr with
  | Ast_Value.String s -> printf "string %S\n" s
  | Ast_Value.Int i -> printf "string %i\n" i
  | Ast_Value.Bool b -> printf "string %b\n" b
  | Ast_Value.Array items -> items |> List.iter print_expr

and print_location pos =
  let fname = pos.pos_fname in
  let lnum = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.printf "%s:%d:%d\n" fname lnum col

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      print_location lexbuf.lex_curr_p;
      eprintf "Syntax Error: %s" msg;
      None
  | Parser.Error ->
      print_location lexbuf.lex_curr_p;
      eprintf "Parser Error";
      exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      print_ast value;
      parse_and_print lexbuf
  | None -> ()

let main =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in chan

let () = main