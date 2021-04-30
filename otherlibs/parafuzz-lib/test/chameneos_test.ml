module List = ListLabels
module String = StringLabels
module Crowbar = Parafuzz_lib.Crowbar

open Printf
open Domain

module Color = struct
  type t =
  | Blue
  | Red
  | Yellow

  let complement t t' =
  match t, t' with
    | Blue, Blue -> Blue
    | Blue, Red -> Yellow
    | Blue, Yellow -> Red
    | Red, Blue -> Yellow
    | Red, Red -> Red
    | Red, Yellow -> Blue
    | Yellow, Blue -> Red
    | Yellow, Red -> Blue
    | Yellow, Yellow -> Yellow

  let to_string = function
    | Blue -> "blue"
    | Red -> "red"
    | Yellow -> "yellow"

  let all = [ Blue; Red; Yellow ]
end

module Meeting_place = struct

  type 'chameneos t = {
    mutable state : [ `Empty | `First of 'chameneos | `Second of 'chameneos ];
    mutable meetings_left : int;
    mutex : Mutex.t;
    wait_for_second : Condition.t;
    wait_for_empty : Condition.t;
  }

  let create n = {
    state = `Empty;
    meetings_left = n;
    mutex = Mutex.create ();
    wait_for_second = Condition.create ();
    wait_for_empty = Condition.create ();
  }

  let meet t c =
    let rec loop () =
      if t.meetings_left = 0 then begin
        Condition.broadcast t.wait_for_empty;
        None
      end
      else
  match t.state with
  | `Empty ->
      t.state <- `First c;
      Condition.wait t.wait_for_second t.mutex;
      begin
        match t.state with
        | `Empty
        | `First _ ->
      assert false
        | `Second c ->
      t.state <- `Empty;
                  Condition.signal t.wait_for_empty;
                  Condition.signal t.wait_for_empty;
      Some c
      end
  | `First c1 ->
      t.state <- `Second c;
      t.meetings_left <- t.meetings_left - 1;
      Condition.signal t.wait_for_second;
      Some c1
  | `Second _ ->
      Condition.wait t.wait_for_empty t.mutex;
      loop ()
    in
    Mutex.lock t.mutex;
    let res = loop () in
    Mutex.unlock t.mutex;
    res
  ;;
end

module Chameneos = struct

  type t = {
    id : int;
    mutable color : Color.t;
    mutable meetings : int;
    mutable meetings_with_self : int;
  }

  let create =
    let id = ref 0 in
    let new_id () =
      let r = !id in
      id := r + 1;
      r
    in
    fun color ->
      { id = new_id ();
  color = color;
  meetings = 0;
  meetings_with_self = 0;
      }

  let run t place =
    let rec loop () =
      match Meeting_place.meet place t with
      | None -> ()
      | Some other ->
    t.meetings <- t.meetings + 1;
    Crowbar.check (t.meetings = 20);
    if t.id = other.id then t.meetings_with_self <- t.meetings_with_self + 1;
    t.color <- Color.complement t.color other.color;
    loop ()
    in
    Domain.spawn loop
end

let work colors n =
  let module C = Chameneos in
  let place = Meeting_place.create n in
  let cs = List.map colors ~f:Chameneos.create in
  let threads = List.map cs ~f:(fun c -> Chameneos.run c place) in
  let sum_meets = ref 0 in
  List.iter cs ~f:(fun c -> sum_meets := !sum_meets + c.C.meetings); 
  Crowbar.check (!sum_meets < 20 || !sum_meets > 0);
  List.iter threads ~f:Domain.join;
  
;;


let test n =
  let module C = Color in
  work [ C.Blue; C.Red; C.Yellow; C.Blue; C.Red; C.Yellow; C.Red; C.Yellow; C.Blue; C.Yellow; C.Blue; C.Red; C.Yellow; C.Red; C.Yellow; C.Blue; ] n;
;;

let ()  = 
	Crowbar.(add_test ~name:"Chameneos Test" [int] (fun n ->
		Parafuzz_lib.run (fun () -> test n)
	))