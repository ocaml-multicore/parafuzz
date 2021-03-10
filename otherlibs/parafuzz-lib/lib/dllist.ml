(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



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

(* +-----------------------------------------------------------------+
   | Operations on nodes                                             |
   +-----------------------------------------------------------------+ *)

let get node =
  Option.get node.data

let set node data =
  node.data <- Some data

let remove_binding seq n = Hashtbl.remove seq.map n

let remove seq node =
  if node.active then begin
    node.active <- false;
    node.prev.next <- node.next;
    node.next.prev <- node.prev;
    seq.len <- seq.len - 1
  end


let remove_at_index seq n =
    if n < 0 || n >= (seq.len) then raise Invalid_index;

    match Hashtbl.find_opt seq.map n with
    | None -> raise Invalid_index
    | Some node ->
        (* If not the last node in the list*)
        if n <> (seq.len - 1)  then (
            (* Find last node *)
            let node_at_last = Hashtbl.find seq.map (seq.len - 1) in
            (* Replace binding at [n] with last node *)
            Hashtbl.replace seq.map n node_at_last
        );

        (* remove binding for last node in the list *)
        remove_binding seq (seq.len-1);
        remove seq node;
        Option.get node.data

(* +-----------------------------------------------------------------+
   | Operations on sequences                                         |
   +-----------------------------------------------------------------+ *)

let create () =
    (* Dummy nodes are not active *)
    let rec dummy_head = {prev = dummy_head; next = dummy_head; data = None; active = false} in
    let rec dummy_rear = {prev = dummy_rear; next = dummy_rear; data = None; active = false} in
    dummy_head.next <- dummy_rear;
    dummy_rear.prev <- dummy_head;
    let seq = { head = dummy_head; rear = dummy_rear; len = 0; map = Hashtbl.create 10 } in
    seq


let length seq = seq.len

let is_empty seq = ((length seq) = 0)

(** Add binding for [node] with [n]*)
let add_binding seq node n = Hashtbl.add seq.map n node

let add data seq =
  let node = { prev = seq.rear.prev; next = seq.rear; data = Some data; active = true } in
  seq.rear.prev.next <- node;
  seq.rear.prev <- node;
  (* Add binding for [node] with last index*)
  add_binding seq node seq.len;
  seq.len <- seq.len + 1;
  node

