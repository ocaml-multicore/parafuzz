effect Context_switch : unit

let handle_context_switch k = 
(*     Add k to the continuation queue *)
(*     select a continuation & resume it by continue  *)
    ()

let context_switch () = 
    match perform Context_switch with
    | () -> ( Thread_afl_instrumentation.context_switch_instrument (); () )
    | effect Context_switch k ->
            handle_context_switch k

