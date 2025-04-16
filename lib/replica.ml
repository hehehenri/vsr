open Types

type normal = |
type view_change = |
type recovering = |

type _ state =
  | Normal : normal state
  | ViewChange : view_change state
  | Recovering : recovering state

type config = ReplicaId.t list

module LogMap = Map.Make (struct
  type t = OpNumber.t

  let compare = OpNumber.compare
end)

module ClientIdMap = Map.Make (struct
  type t = ClientId.t

  let compare = ClientId.compare
end)

type client_entry = {
  client_id : ClientId.t;
  request_number : RequestNumber.t;
  response : string option;
}

type client_map = client_entry ClientIdMap.t

module Make (N : Network.S) = struct
  type ('s, 'op) replica = {
    id : ReplicaId.t;
    state : 's state;
    view : ViewNumber.t;
    config : config;
    mutable op_number : OpNumber.t;
    commit_number : OpNumber.t;
    mutable request_log : 'op Message.request_message LogMap.t;
    mutable client_map : client_map;
    network : 'op N.t;
  }

  let init_replica id config =
    let config = List.map (fun id -> ReplicaId.of_int id) config in
    let request_log = LogMap.empty in
    let client_map = ClientIdMap.empty in
    {
      id = ReplicaId.of_int id;
      state = Normal;
      view = ViewNumber.init ();
      config;
      op_number = OpNumber.init ();
      commit_number = OpNumber.init ();
      request_log;
      client_map;
      network = N.create config;
    }

  let get_primary_id replica =
    let view_number = ViewNumber.to_int replica.view in
    let length = List.length replica.config in
    assert (length <> 0);
    let primary_index = view_number mod length in
    List.nth replica.config primary_index

  let is_primary replica =
    let primary_id = get_primary_id replica in
    primary_id = replica.id

  let handle_request (replica:('s, 'op) replica) (request:'op Message.request_message) =
    assert (is_primary replica);

    let continue () =
        replica.op_number <- OpNumber.succ replica.op_number;
        replica.request_log <- LogMap.add replica.op_number request replica.request_log;
        replica.client_map <- ClientIdMap.update request.client_id (fun _ -> Some ({ client_id = request.client_id; request_number = request.request_number; response = None; })) replica.client_map;

        let prepare = Message.Prepare {
                view_number = replica.view;
                op = request.op;
                op_number = replica.op_number;
                commit_number = replica.commit_number;
        } in
        N.broadcast replica.network prepare
    in

    let latest_request =
      ClientIdMap.find_opt request.client_id replica.client_map
    in
    match latest_request with
    | Some latest_request -> (
        if request.request_number < latest_request.request_number then
          (* NOTE: drop the request *)
          ()
        else if request.request_number = latest_request.request_number then
          (* TODO: reply back to the user with the existing response*)
          (* latest_request.response *)
          ()
        else continue ())
    | None -> continue ()

  let handle_message (replica:('s, 'op) replica) (message : 'op Message.message) =
    match message with
    | Request req -> handle_request replica req
    | _ -> assert false


  let get_primary_id replica =
    let view_number = ViewNumber.to_int replica.view in
    let length = List.length replica.config in
    assert (length <> 0);
    let primary_index = view_number mod length in
    List.nth replica.config primary_index

  let is_primary replica =
    let primary_id = get_primary_id replica in
    primary_id = replica.id

end