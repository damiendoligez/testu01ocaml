open Printf

external smallcrush : unit -> unit = "smallcrush"
external crush : unit -> unit = "crush"
external bigcrush : unit -> unit = "bigcrush"

module R = Random32
(* use [Random] to test the stdlib implementation, but then some tests will
   fail because they assume 31 random bits per word. *)

let get_random_floats n = Array.init n (fun _ -> R.float 1.0)
let get_random_bits n = Array.init n (fun _ -> R.bits ())

let usage () =
  eprintf "usage: testrng {small|crush|big} [seed]\n";
  exit 2;

;;
  Callback.register "get_random_floats" get_random_floats;
  Callback.register "get_random_bits" get_random_bits;

  let seed =
    if Array.length Sys.argv < 3 then begin
      R.self_init ();
      R.bits ()
    end else begin
      int_of_string Sys.argv.(2)
    end
  in

  if Array.length Sys.argv < 2 then usage ();
  begin match Sys.argv.(1) with
  | "small" -> smallcrush ();
  | "crush" -> crush ();
  | "big" -> bigcrush ();
  | _ -> usage ();
  end;

  Printf.printf "seed = %d\n%!" seed;
