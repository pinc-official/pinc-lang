[@@@warning "-unused-value-declaration"]

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

  let report b =
    print_endline (Format.sprintf "Benchmark: %s" b.name);
    print_endline
      (Format.sprintf
         "Avg time/iteration: %fms"
         (Time.print b.duration /. float_of_int b.n));
    print_endline
      (Format.sprintf
         "Allocs/iteration: %d"
         (int_of_float (b.netAllocs /. float_of_int b.n)));
    print_endline
      (Format.sprintf
         "Bytes/iteration: %d"
         (int_of_float (b.netBytes /. float_of_int b.n)));
    print_endline (Format.sprintf "Number of iterations: %d" b.n);
    print_endline
      (Format.sprintf "Time to complete all iterations: %fms" (Time.print b.duration));
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
      let allocatedWords = mallocs () in
      let diff = Time.diff b.start (Time.now ()) in
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
    for n = 1 to 200 do
      runIteration b n
    done
  ;;
end

module Benchmarks : sig
  val run : unit -> unit
end = struct
  type action =
    | Parse
    | Interp of string

  let string_of_action = function
    | Parse -> "parser"
    | Interp s -> "interpret: " ^ s
  ;;

  let benchmark filename action =
    let src = Pinc.Source.of_file filename in
    let benchmarkFn =
      match action with
      | Parse ->
          fun _ ->
            let _ = Sys.opaque_identity (Pinc.Parser.parse src) in
            ()
      | Interp root ->
          fun _ ->
            let _ =
              Sys.opaque_identity
                (Pinc.Interpreter.eval
                   ~tag_data_provider:Pinc.Interpreter.noop_data_provider
                   ~root
                   [ src ])
            in
            ()
    in
    let name = filename ^ " " ^ string_of_action action in
    let b = Benchmark.make ~name ~f:benchmarkFn () in
    Benchmark.launch b;
    Benchmark.report b
  ;;

  let run () =
    benchmark "./benchmark/data/Section.pi" Parse;
    benchmark "./benchmark/data/Section.pi" (Interp "Section")
  ;;
end

let () = Benchmarks.run ()
