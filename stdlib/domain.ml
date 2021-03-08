module Raw = struct
  (* Low-level primitives provided by the runtime *)
  type t = private int
  external spawn : (unit -> unit) -> t
    = "caml_domain_spawn"
  external self : unit -> t
    = "caml_ml_domain_id"
  external cpu_relax : unit -> unit
    = "caml_ml_domain_cpu_relax"
end

type nanoseconds = int64
external timer_ticks : unit -> (int64 [@unboxed]) =
  "caml_ml_domain_ticks" "caml_ml_domain_ticks_unboxed" [@@noalloc]

module Sync = struct
  exception Retry
  let critical_section _ =
    failwith "critical_section not implemented"
   
  let notify _ = 
      failwith "notify not implemented"

  let wait () =  
      failwith "wait not implemented"

  type timeout_or_notified =
      Timeout | Notified

  let wait_until _ = 
      failwith "wait_until not implemented"

  let wait_for _ = 
      failwith "wait_for not implemented"

  let cpu_relax () = Raw.cpu_relax ()
  external poll : unit -> unit = "%poll"
end

module Dllist = struct
  (* This module is part of Lwt, released under the MIT license. 
    Visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md for more details *)

  exception Invalid_index

  type 'a node = {
    mutable prev : 'a node;
    mutable next : 'a node;
    mutable data : 'a option;
    mutable active : bool;
  }

  type 'a t = {
    mutable head : 'a node;
    mutable rear : 'a node;
    mutable len : int;
    map  : (int, 'a node) Hashtbl.t;
  }

  let remove seq node =
    if node.active then begin
      node.active <- false;
      node.prev.next <- node.next;
      node.next.prev <- node.prev;
      Printf.printf "%d\n" seq.len;
      seq.len <- seq.len - 1
    end

  let remove_at_index seq n = 
      match Hashtbl.find_opt seq.map n with
      | None -> raise Invalid_index
      | Some node ->
          if seq.len > 1 then (
              (* Find last node *)
              let node_at_last = Hashtbl.find seq.map (seq.len - 1) in
              (* Replace binding at [n] with last node *)
              Hashtbl.replace seq.map n node_at_last;
              (* Remove binding at last index *)
              Hashtbl.remove seq.map (seq.len - 1)
          ) else (
              Hashtbl.remove seq.map n
          );
          remove seq node;
          Option.get node.data

  let create () =
      let rec dummy_head = {prev = dummy_head; next = dummy_head; data = None; active = true} in
      let rec dummy_rear = {prev = dummy_rear; next = dummy_rear; data = None; active = true} in
      dummy_head.next <- dummy_rear;
      dummy_rear.prev <- dummy_head;
      let seq = { head = dummy_head; rear = dummy_rear; len = 0; map = Hashtbl.create 10 } in
      seq


  let length seq = seq.len

  let is_empty seq = (length seq) = 0

  let add_binding seq node n = Hashtbl.add seq.map n node

  let add data seq =
    let node = { prev = seq.rear.prev; next = seq.rear; data = Some data; active = true } in
    seq.rear.prev.next <- node;
    seq.rear.prev <- node;
    add_binding seq node seq.len;
    seq.len <- seq.len + 1;
    ()

  let iter f seq = 
    let rec func f curnode = 
      if curnode != curnode.next then 
        (if curnode.active then f curnode.data else (); func f curnode.next)
    in func f seq.head

  let iter_node f seq = 
    let rec func f curnode = 
      if curnode != curnode.next then 
        (if curnode.active then f curnode else (); func f curnode.next)
    in func f seq.head
    
  let clear seq = iter_node (remove seq) seq

end

module Mvar = struct

  type 'a state = 
    | Filled of 'a * (((unit Scheduler.cont) * 'a * int) Dllist.t)
      (* The first element in the pair represents the value stored.
        The second element contains a queue of continuations that have `put`, that are waiting to be run *)

    | Empty of ('a Scheduler.cont * int) Dllist.t
        (* This represents a queue of continuations that have `get`, waiting to be run. These will be run
        once the MVar has some value. *)

  type 'a t = 'a state ref

  let make_empty () = ref (Empty (Dllist.create ()))

  (* Reads the value of an MVar ['a t], if empty, is put in a queue for waiting *)
  let get mvar = match (!mvar) with
    | Empty (wait_q) -> Scheduler.suspend (fun (cont,id) -> Dllist.add (cont,id) wait_q)
    | Filled (value, wait_q) -> 
        if Dllist.is_empty wait_q then (
            mvar := Empty(Dllist.create ()); value)
        else (
            let (waiting_f, nvalue, id) = Dllist.remove_at_index wait_q (Scheduler.range (Dllist.length wait_q)) in
            mvar := Filled(nvalue, wait_q); 
            Scheduler.resume (waiting_f, (), id);
            value)


  (* Writes ['as] to an MVar ['a t], if already full, is put in a queue for waiting *)
  let put v mvar = match (!mvar) with
    | Empty (wait_q) -> 
        if Dllist.is_empty wait_q then 
            mvar := Filled(v, Dllist.create ()) 
        else ( 
            let (waiting_f,id) = Dllist.remove_at_index wait_q (Scheduler.range (Dllist.length wait_q)) in 
            Scheduler.resume (waiting_f, v, id))
    | Filled (_, wait_q) -> Scheduler.suspend (fun (cont,id) -> Dllist.add (cont, v, id) wait_q)


  (* Writes the value ['a] to an MVar ['a t], and wakes all the waiting threads up if the MVar is already full, 
   * is put in a queue for waiting *)
  let put_all v mvar = match (!mvar) with
    | Empty (wait_q) -> 
        if Dllist.is_empty wait_q then 
            mvar := Filled(v, Dllist.create ()) 
        else (
            Dllist.iter (fun (thread) -> match thread with 
                        | Some(cont, id) -> Scheduler.resume (cont, v, id)
                        | None -> ()) wait_q; 
            Dllist.clear wait_q; 
            mvar := Empty(Dllist.create ()))
    | Filled (_, wait_q) -> Scheduler.suspend (fun (cont,id) -> Dllist.add (cont, v, id) wait_q)

  (* Tries to write a value ['a] to an MVar ['a t].
   * Returns true if successful, if the MVar is not empty, returns false *)
  let try_put v mvar = match !mvar with
    | Empty (_) -> (put v mvar); true
    | _ -> false
  
  (* Checks the value in the MVar. Returns true if equal,
   * else false *)
  let assert_val v mvar = match !mvar with
    | Empty(_) -> false
    | Filled(value, _) -> value = v

end

module Mutex = struct

  type t = int Mvar.t

  exception LockNotHeld

  let create = Mvar.make_empty

  let lock mut = Mvar.put (Scheduler.get_current_domain_id ()) mut 

  let unlock mut = let id = (Scheduler.get_current_domain_id ()) in
    if (Mvar.assert_val id mut) then ignore(Mvar.get mut) 
    else raise LockNotHeld

  let try_lock mut = Mvar.try_put (Scheduler.get_current_domain_id ()) mut

end

module Condition = struct

  type t = (bool Mvar.t) * Mutex.t

  let create mutex = (Mvar.make_empty (), mutex)

  let wait (cond, mutex) = 
    Mutex.unlock mutex;
    ignore (Mvar.get cond);
    Mutex.lock mutex

  let signal (cond, _) = Mvar.put true cond

  let broadcast (cond, _) = Mvar.put_all true cond

end

type id = Raw.t

type 'a state =
| Running
| Joining of ('a, exn) result option ref * Mutex.t * Condition.t
| Finished of ('a, exn) result
| Joined

type 'a t =
    { domain : Raw.t; state : 'a state Atomic.t; mutex : Mutex.t; cond : Condition.t }

exception Retry
let rec spin f =
  try f () with 
  Retry -> spin f

let cas r vold vnew =
  if not (Atomic.compare_and_set r vold vnew) then raise Retry

let spawn f =
  let state = Atomic.make Running in
  let body () =
    let result = match f () with
      | x -> Ok x
      | exception ex -> Error ex in
    (* Begin a critical section that is ended by domain
       termination *)
(*     Raw.critical_adjust (+1); *)
    spin (fun () ->
      match Atomic.get state with
      | Running ->
         cas state Running (Finished result)
      | Joining (r, mutex, cond) as old ->
         cas state old Joined;
         Mutex.lock mutex;
         r := Some result;
         Condition.signal cond;
         Mutex.unlock mutex
(*          Raw.interrupt d *)
      | Joined | Finished _ ->
         failwith "internal error: I'm already finished?") in
  let mutex = Mutex.create () in
  { domain = Raw.spawn body; state; mutex; cond = Condition.create mutex }

let join { state; mutex; cond } =
  let res = spin (fun () ->
    match Atomic.get state with
    | Running ->
       let res = ref None in
       cas state Running (Joining (res, mutex, cond));
       Mutex.lock mutex;
       let rec check_res () = match !res with
           | None -> Condition.wait cond; check_res ()
           | Some r -> r
       in
       let r = check_res () in
       Mutex.unlock mutex;
       r
    | Finished res as old ->
       cas state old Joined;
       res
    | Joining _ | Joined ->
       raise (Invalid_argument "This domain has already been joined")) in
  (* Wait until the domain has terminated.
     The domain is in a critical section which will be
     ended by the runtime when it terminates *)
(*   Sync.notify domain; *)
  match res with
  | Ok x -> x
  | Error ex -> raise ex


let get_id { domain; _ } = domain

let self () = Raw.self ()

module DLS = struct

  type 'a key = int ref

  type entry = {key: int ref; slot: Obj.t ref}

  external get_dls_list : unit -> entry list
    = "caml_domain_dls_get" [@@noalloc]

  external set_dls_list : entry list  -> unit
    = "caml_domain_dls_set" [@@noalloc]

  let new_key () = ref 0

  let set k x =
    let cs = Obj.repr x in
    let old = get_dls_list () in
    let rec add_or_update_entry k v l =
      match l with
      | [] -> Some {key = k; slot = ref v}
      | hd::tl -> if (hd.key == k) then begin
        hd.slot := v;
        None
      end else add_or_update_entry k v tl
    in
    match add_or_update_entry k cs old with
    | None -> ()
    | Some e -> set_dls_list (e::old)

  let get k =
    let vals = get_dls_list () in
    let rec search k l =
      match l with
      | [] -> None
      | hd::tl -> if hd.key == k then Some !(hd.slot) else search k tl
    in
    Obj.magic @@ search k vals

end
