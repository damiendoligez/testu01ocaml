open Printf

external smallcrush : unit -> unit = "smallcrush"
external crush : unit -> unit = "crush"
external bigcrush : unit -> unit = "bigcrush"

(* Note: if you use the 30-bit generator, some tests will always fail
   because they assume 31 random bits per word.
   In SmallCrush, these are the RandomWalk1 tests. *)

let usage =
  "usage: testrng [options] [seed]\n\
  \  one of -30, -32, -32mul must be specified"

let rev = ref false
type algorithm = Null | R30 | R32 | R32mul
let algo_to_string a =
  match a with
  | Null -> "(null)"
  | R30 -> "30"
  | R32 -> "32"
  | R32mul -> "32mul"
let algo = ref Null
type test = Small | Crush | Big
let test = ref Crush
let seed = Random.self_init (); ref (Random.bits ())

let set r x = Arg.Unit (fun () -> r := x)

let argspec = [
  "-r", Arg.Set rev, " reverse the bits from the PRNG";
  "-30", set algo R30, " test the stdlib 30-bit PRNG";
  "-32", set algo R32, " test the 32-bit PRNG";
  "-32mul", set algo R32mul, " test the 32-bit PRNG with output multiplier";
  "-small", set test Small, " use the SmallCrush test suite";
  "-crush", set test Crush, " use the Crush test suite (default)";
  "-big", set test Big, " use the BigCrush test suite";
]

let rev32 n0 =
  let n1 = ((n0 land 0xaaaaaaaa) lsr 1) lor ((n0 land 0x55555555) lsl 1) in
  let n2 = ((n1 land 0xcccccccc) lsr 2) lor ((n1 land 0x33333333) lsl 2) in
  let n3 = ((n2 land 0xf0f0f0f0) lsr 4) lor ((n2 land 0x0f0f0f0f) lsl 4) in
  let n4 = ((n3 land 0xff00ff00) lsr 8) lor ((n3 land 0x00ff00ff) lsl 8) in
  (n4 lsr 16) lor (n4 lsl 16)

let rev30 n = (rev32 n) lsr 2

;;
  Arg.parse argspec (fun n -> seed := int_of_string n) usage;
  let float, bits =
    match !algo, !rev with
    | Null, _ -> Arg.usage argspec usage; exit 2
    | R30, false ->
        Random.init !seed;
        Random.float, Random.bits
    | R30, true ->
        Random.init !seed;
        Random.float, fun () -> rev30 (Random.bits ())
    | R32, false ->
        Random32.init !seed;
        Random32.float, Random32.bits
    | R32, true ->
        Random32.init !seed;
        Random32.float, fun () -> rev32 (Random32.bits ())
    | R32mul, false ->
        Random32mul.init !seed;
        Random32mul.float, Random32mul.bits
    | R32mul, true ->
        Random32mul.init !seed;
        Random32.float, fun () -> rev32 (Random32mul.bits ())
  in
  let get_random_floats n = Array.init n (fun _ -> float 1.0) in
  let get_random_bits n = Array.init n (fun _ -> bits ()) in
  Callback.register "get_random_floats" get_random_floats;
  Callback.register "get_random_bits" get_random_bits;

  begin match !test with
  | Small -> smallcrush ();
  | Crush -> crush ();
  | Big -> bigcrush ();
  end;

  Printf.printf "generator = %s; seed = %d\n%!" (algo_to_string !algo) !seed;
