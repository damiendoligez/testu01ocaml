
(* Copy of the OCaml stdlib PRNG, modified to return 32 bits of data
   when calling the [bits] function.

   Of course, this will only work on 64-bit machines.
 *)

external random_seed: unit -> int array = "caml_sys_random_seed"

module State = struct

  type t = { st : int array; mutable idx : int }

  let new_state () = { st = Array.make 55 0; idx = 0 }
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx


  let full_init s seed =
    let combine accu x = Digest.string (accu ^ string_of_int x) in
    let extract d =
      Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16)
      + (Char.code d.[3] lsl 24)
    in
    let seed = if Array.length seed = 0 then [| 0 |] else seed in
    let l = Array.length seed in
    for i = 0 to 54 do
      s.st.(i) <- i;
    done;
    let accu = ref "x" in
    for i = 0 to 54 + max 55 l do
      let j = i mod 55 in
      let k = i mod l in
      accu := combine !accu seed.(k);
      s.st.(j) <- (s.st.(j) lxor extract !accu) land 0xFFFFFFFF;
    done;
    s.idx <- 0


  let make seed =
    let result = new_state () in
    full_init result seed;
    result


  let make_self_init () = make (random_seed ())

  let copy s =
    let result = new_state () in
    assign result s;
    result


  (* Returns 32 random bits as an integer 0 <= x < 2^32 *)
  let bits32 s =
    s.idx <- (s.idx + 1) mod 55;
    let curval = s.st.(s.idx) in
    let newval = s.st.((s.idx + 24) mod 55)
                 + (curval lxor ((curval lsr 25) land 0x1F)) in
    let newval30 = newval land 0xFFFFFFFF in
    s.st.(s.idx) <- newval30;
    (newval30 * 1618033989) land 0xFFFFFFFF

  (* Version of the [bits] function that returns 30 bits as usual. *)
  let bits s = bits32 s land 0x3FFFFFFF

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound


  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound


  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound


  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))


  (* Returns a float 0 <= x <= 1 with at most 60 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0  (* 2^30 *)
    and r1 = Pervasives.float (bits s)
    and r2 = Pervasives.float (bits s)
    in (r1 /. scale +. r2) /. scale


  let float s bound = rawfloat s *. bound

  let bool s = (bits s land 1 = 0)

  (* for the exported version of [bits], use the 32-bit version *)
  let bits s = bits32 s

end

(* This is the state you get with [init 27182818] and then applying
   the "land 0x3FFFFFFF" filter to them.  See #5575, #5793, #5977. *)
let default = {
  State.st = [|
      0x3ae2522b; 0x1d8d4634; 0x15b4fad0; 0x18b14ace; 0x12f8a3c4; 0x3b086c47;
      0x16d467d6; 0x101d91c7; 0x321df177; 0x0176c193; 0x1ff72bf1; 0x1e889109;
      0x0b464b18; 0x2b86b97c; 0x0891da48; 0x03137463; 0x085ac5a1; 0x15d61f2f;
      0x3bced359; 0x29c1c132; 0x3a86766e; 0x366d8c86; 0x1f5b6222; 0x3ce1b59f;
      0x2ebf78e1; 0x27cd1b86; 0x258f3dc3; 0x389a8194; 0x02e4c44c; 0x18c43f7d;
      0x0f6e534f; 0x1e7df359; 0x055d0b7e; 0x10e84e7e; 0x126198e4; 0x0e7722cb;
      0x1cbede28; 0x3391b964; 0x3d40e92a; 0x0c59933d; 0x0b8cd0b7; 0x24efff1c;
      0x2803fdaa; 0x08ebc72e; 0x0f522e32; 0x05398edc; 0x2144a04c; 0x0aef3cbd;
      0x01ad4719; 0x35b93cd6; 0x2a559d4f; 0x1e6fd768; 0x26e27f36; 0x186f18c3;
      0x2fbf967a;
    |];
  State.idx = 0;
}

(* re-do the initialization, but with 32-bit width *)
;; State.full_init default [| 27182818 |]

let bits () = State.bits default
let int bound = State.int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default

let full_init seed = State.full_init default seed
let init seed = State.full_init default [| seed |]
let self_init () = full_init (random_seed())

(* Manipulating the current state. *)

let get_state () = State.copy default
let set_state s = State.assign default s
