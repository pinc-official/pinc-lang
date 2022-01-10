open Fennek_lib

let main = begin
  let filename = Sys.argv.(1) in
  let start_time = Unix.gettimeofday() in

  let buffer = Buffer.create 1024 in
  let output = Buffer.add_string buffer in
  let () = Interpreter.from_file ~output filename in

  let result = Buffer.contents buffer in
  
  let end_time = Unix.gettimeofday() in
  let _seconds = end_time -. start_time in
  print_endline result;
  (* Printf.printf "%fns" (seconds *. 1000. *. 1000.); *)
end

let () = main