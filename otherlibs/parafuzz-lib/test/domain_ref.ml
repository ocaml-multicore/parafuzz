module Crowbar = Parafuzz_lib.Crowbar

let r = ref 0

let incr () =  r := !r + 1

let test () = 
  let d = Domain.spawn(fun () -> incr ()) in
  Domain.join d;
  incr ();
  let t = Domain.spawn(fun () -> incr ()) in
  Crowbar.check (!r = 3);
  Domain.join t

let ()  = 
  Crowbar.(add_test ~name:"Multiple Domain spawns check" [Crowbar.const 1] (fun _ ->
    Parafuzz_lib.run test
  ))
