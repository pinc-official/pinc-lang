[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-constructor"]

let iterations = 1

module Time : sig
  type t

  val now : unit -> t
  val toUint64 : t -> int64 [@@live]

  (* let of_uint64_ns ns = ns *)

  val nanosecond : t [@@live]
  val microsecond : t [@@live]
  val millisecond : t [@@live]
  val second : t [@@live]
  val minute : t [@@live]
  val hour : t [@@live]
  val zero : t
  val diff : t -> t -> t
  val add : t -> t -> t
  val print : t -> float
end = struct
  (* nanoseconds *)
  type t = int64

  let zero = 0L
  let toUint64 s = s
  let nanosecond = 1L
  let microsecond = Int64.mul 1000L nanosecond
  let millisecond = Int64.mul 1000L microsecond
  let second = Int64.mul 1000L millisecond
  let minute = Int64.mul 60L second
  let hour = Int64.mul 60L minute

  (* TODO: we could do this inside caml_absolute_time *)
  external init : unit -> unit = "caml_mach_initialize"

  let () = init ()

  external now : unit -> t = "caml_mach_absolute_time"

  let diff t1 t2 = Int64.sub t2 t1
  let add t1 t2 = Int64.add t1 t2
  let print t = Int64.to_float t *. 1e-6
end

module Benchmark : sig
  type t

  val make : name:string -> f:(t -> unit) -> unit -> t
  val launch : t -> unit
  val report : t -> unit
end = struct
  type t = {
    name : string;
    mutable start : Time.t;
    mutable n : int; (* current iterations count *)
    mutable duration : Time.t;
    benchFunc : t -> unit;
    mutable timerOn : bool;
    (* The initial states *)
    mutable startAllocs : float;
    mutable startBytes : float;
    (* The net total of this test after being run. *)
    mutable netAllocs : float;
    mutable netBytes : float;
  }

  let format_bytes bytes =
    let b = float_of_int bytes in
    let gb = 1024. *. 1024. *. 1024. in
    let mb = 1024. *. 1024. in
    let kb = 1024. in
    if b >= gb then
      Format.sprintf "%.4f GB" (b /. gb)
    else if b >= mb then
      Format.sprintf "%.4f MB" (b /. mb)
    else if b >= kb then
      Format.sprintf "%.4f KB" (b /. kb)
    else
      Format.sprintf "%d Bytes" bytes
  ;;

  let format_allocations n =
    let s = string_of_int n in
    let len = String.length s in
    let rec loop i acc count =
      if i < 0 then
        acc
      else if count > 0 && count mod 3 = 0 then
        loop (i - 1) (String.make 1 s.[i] ^ "," ^ acc) (count + 1)
      else
        loop (i - 1) (String.make 1 s.[i] ^ acc) (count + 1)
    in
    let formatted_base = loop (len - 1) "" 0 in

    (* Append shorthand notation based on the size of the number *)
    if n >= 1_000_000_000 then (
      let mio = float_of_int n /. 1_000_000_000. in
      Format.sprintf "%s (%.1f Bn)" formatted_base mio)
    else if n >= 1_000_000 then (
      let mio = float_of_int n /. 1_000_000. in
      Format.sprintf "%s (%.1f Mio)" formatted_base mio)
    else if n >= 1_000 then (
      let k = float_of_int n /. 1_000. in
      Format.sprintf "%s (%.1f K)" formatted_base k)
    else
      formatted_base
  ;;

  let report b =
    print_endline b.name;
    print_endline (Format.sprintf "Number of iterations: .............. %d" b.n);
    let () =
      if b.n > 1 then
        print_endline
          (Format.sprintf
             "Avg time per iteration: ............ %f ms"
             (Time.print b.duration /. float_of_int b.n))
    in
    let allocs_per_iteration = int_of_float (b.netAllocs /. float_of_int b.n) in
    print_endline
      (Format.sprintf
         "Allocs per iteration: .............. %s"
         (format_allocations allocs_per_iteration));

    let bytes_per_iteration = int_of_float (b.netBytes /. float_of_int b.n) in
    print_endline
      (Format.sprintf
         "Bytes per iteration: ............... %s"
         (format_bytes bytes_per_iteration));
    print_endline
      (Format.sprintf
         "Time to complete all iterations: ... %f ms"
         (Time.print b.duration));
    print_newline ();
    ()
  ;;

  let make ~name ~f () =
    {
      name;
      start = Time.zero;
      n = 0;
      benchFunc = f;
      duration = Time.zero;
      timerOn = false;
      startAllocs = 0.;
      startBytes = 0.;
      netAllocs = 0.;
      netBytes = 0.;
    }
  ;;

  (* total amount of memory allocated by the program since it started in words *)
  let mallocs () =
    (* Force a minor GC to flush current nursery allocations to the counters *)
    Gc.minor ();
    let stats = Gc.quick_stat () in
    stats.minor_words +. stats.major_words -. stats.promoted_words
  ;;

  let startTimer b =
    if not b.timerOn then (
      let allocatedWords = mallocs () in
      b.startAllocs <- allocatedWords;
      b.startBytes <- allocatedWords *. 8.;
      b.start <- Time.now ();
      b.timerOn <- true)
  ;;

  let stopTimer b =
    if b.timerOn then (
      let diff = Time.diff b.start (Time.now ()) in
      let allocatedWords = mallocs () in
      b.duration <- Time.add b.duration diff;
      b.netAllocs <- b.netAllocs +. (allocatedWords -. b.startAllocs);
      b.netBytes <- b.netBytes +. ((allocatedWords *. 8.) -. b.startBytes);
      b.timerOn <- false)
  ;;

  let resetTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs () in
      b.startAllocs <- allocatedWords;
      b.netAllocs <- allocatedWords *. 8.;
      b.start <- Time.now ());
    b.netAllocs <- 0.;
    b.netBytes <- 0.
  ;;

  let runIteration b n =
    Gc.full_major ();
    b.n <- n;
    resetTimer b;
    startTimer b;
    b.benchFunc b;
    stopTimer b
  ;;

  let launch b =
    for n = 1 to iterations do
      runIteration b n
    done
  ;;
end

module Benchmarks : sig
  val run : unit -> unit
end = struct
  type action =
    | Parse
    | Compile
    | Deserialize
    | Vm of string
    | Interp of string

  let string_of_action = function
    | Parse -> "[PARSER]"
    | Compile -> "[COMPILER]"
    | Deserialize -> "[DESERIALIZATION]"
    | Vm _ -> "[VM]"
    | Interp _ -> "[INTERPRETER]"
  ;;

  let benchmark filename action =
    let src = Pinc_lang.Source.of_file filename in
    let ast = Pinc_lang.Parser.get_ast ~include_stdlib:false [ src ] in
    let bytecode = Pinc_lang.Compiler.compile ast in
    let benchmarkFn =
      match action with
      | Parse -> fun _ -> ignore @@ Sys.opaque_identity (Pinc_lang.Parser.get_ast [ src ])
      | Interp root ->
          fun _ ->
            ignore
            @@ Sys.opaque_identity
                 (Pinc_lang.Interpreter.eval_declarations
                    ~tag_data_provider:Pinc_lang.Helpers.noop_data_provider
                    ~root
                    ast)
      | Compile -> fun _ -> ignore @@ Sys.opaque_identity (Pinc_lang.Compiler.compile ast)
      | Deserialize ->
          fun _ -> ignore @@ Sys.opaque_identity (Pinc_lang.Bytecode.deserialize bytecode)
      | Vm _root -> fun _ -> ignore @@ Sys.opaque_identity (Pinc_lang.Vm.eval bytecode)
    in
    let name = string_of_action action in
    let b = Benchmark.make ~name ~f:benchmarkFn () in
    Benchmark.launch b;
    Benchmark.report b
  ;;

  let run () =
    benchmark "./benchmark/data/Benchmark.pi" Parse;
    benchmark "./benchmark/data/Benchmark.pi" Compile;
    benchmark "./benchmark/data/Benchmark.pi" Deserialize;
    benchmark "./benchmark/data/Benchmark.pi" (Vm "Benchmark");
    benchmark "./benchmark/data/Benchmark.pi" (Interp "Benchmark")
  ;;
end

let () = Benchmarks.run ()
