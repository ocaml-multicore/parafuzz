let ()  = 
    Parafuzz_lib.run (fun () -> 
        Printf.printf "Range API call for 1 - 100 returns %d\n" @@ Scheduler.range ~min:1 100)
