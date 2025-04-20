open Types

type normal = |
type view_change = |
type recovering = |

type _ state =
  | Normal : normal state
  | ViewChange : view_change state
  | Recovering : recovering state

module type OperationExecutor = sig
  type operation
  type result

  val execute : operation -> result
end

module Make (N : Network.S) (Op : OperationExecutor) = struct
  type 's replica = {
    id : ReplicaId.t;
    state : 's state;
    view_number : ViewNumber.t;
    config : ReplicaId.t list;
    mutable op_number : OpNumber.t;
    mutable commit_number : OpNumber.t;
    mutable request_log : Op.operation OpMap.t;
    mutable client_map : Types.client_map;
    mutable prepare_ok_acks : ReplicaSet.t OpMap.t;
    mutable start_view_change_acks : ReplicaSet.t ViewMap.t;
    network : Op.operation N.t;
  }

  let init_replica id config =
    let config = List.map (fun id -> ReplicaId.of_int id) config in
    let request_log = OpMap.empty in
    let client_map = ClientMap.empty in
    {
      id = ReplicaId.of_int id;
      state = Normal;
      view_number = ViewNumber.init ();
      config;
      op_number = OpNumber.init ();
      commit_number = OpNumber.init ();
      request_log;
      client_map;
      network = N.create config;
      prepare_ok_acks = OpMap.empty;
      start_view_change_acks = ViewMap.empty;
    }

  let get_primary_id replica =
    let view_number = ViewNumber.to_int replica.view_number in
    let length = List.length replica.config in
    assert (length <> 0);
    let primary_index = view_number mod length in
    List.nth replica.config primary_index

  let is_primary replica =
    let primary_id = get_primary_id replica in
    primary_id = replica.id

  let is_backup replica = not @@ is_primary replica

  let quorum (replica : 's replica) =
    let n = List.length replica.config in
    (n / 2) + 1

  let commit (replica : normal replica) (op : OpNumber.t) =
    match OpMap.find_opt op replica.request_log with
    | Some op -> Op.execute op
    | None -> failwith "op not found in log"

  let handle_request (replica : 's replica)
      (request : Op.operation Message.request) =
    assert (is_primary replica);

    let continue () =
      replica.op_number <- OpNumber.succ replica.op_number;
      replica.request_log <-
        OpMap.add replica.op_number request.op replica.request_log;
      replica.client_map <-
        ClientMap.update request.client_id
          (fun _ ->
            Some
              {
                client_id = request.client_id;
                request_number = request.request_number;
                response = None;
              })
          replica.client_map;

      let prepare =
        Message.Prepare
          {
            view_number = replica.view_number;
            op_number = replica.op_number;
            commit_number = replica.commit_number;
            request;
          }
      in
      N.broadcast replica.network prepare
    in

    let latest_request =
      ClientMap.find_opt request.client_id replica.client_map
    in
    match latest_request with
    | Some latest_request ->
        if request.request_number < latest_request.request_number then
          (* NOTE: drop the request *)
          ()
        else if request.request_number = latest_request.request_number then
          (* TODO: reply back to the user with the existing response*)
          (* latest_request.response *)
          ()
        else continue ()
    | None -> continue ()

  let handle_prepare (replica : normal replica)
      (prep : Op.operation Message.prepare) =
    assert (is_backup replica);
    let is_up_to_date = prep.op_number < OpNumber.succ replica.op_number in

    if not is_up_to_date then (* TODO: state transfer *)
      assert false
    else assert (OpNumber.succ replica.op_number = prep.op_number);
    replica.op_number <- prep.op_number;
    replica.request_log <-
      OpMap.update prep.op_number
        (fun _ -> Some prep.request.op)
        replica.request_log;
    replica.client_map <-
      ClientMap.add prep.request.client_id
        {
          client_id = prep.request.client_id;
          request_number = prep.request.request_number;
          response = None;
        }
        replica.client_map;

    let prepare_ok =
      Message.PrepareOk
        {
          view_number = replica.view_number;
          op_number = replica.op_number;
          replica_id = replica.id;
        }
    in
    let primary_id = get_primary_id replica in
    N.send replica.network (Single primary_id) prepare_ok

  let handle_prepare_ok (replica : normal replica)
      (prep_ok : Message.prepare_ok) =
    assert (is_primary replica);
    let acks =
      match OpMap.find_opt prep_ok.op_number replica.prepare_ok_acks with
      | Some x -> x
      | None -> ReplicaSet.empty
    in

    let acks = ReplicaSet.add prep_ok.replica_id acks in
    replica.prepare_ok_acks <-
      OpMap.add replica.op_number acks replica.prepare_ok_acks;

    let total_acks = ReplicaSet.cardinal acks in
    if total_acks = quorum replica then
      replica.commit_number <- prep_ok.op_number;
    let result = commit replica prep_ok.op_number in

    (* TODO: reply back to the user *)
    let commit =
      Message.Commit
        { view_number = replica.view_number; commit_number = prep_ok.op_number }
    in
    N.broadcast replica.network commit;
    ()

  let handle_commit (replica : normal replica) (message : Message.commit) =
    assert (is_backup replica);
    OpMap.find_opt message.commit_number replica.request_log
    |> Option.iter (fun op -> ignore @@ Op.execute op)

  let handle_start_view_change (replica : normal replica)
      (msg : Message.start_view_change) =
    if msg.view_number > replica.view_number then (
      let acks =
        match
          ViewMap.find_opt msg.view_number replica.start_view_change_acks
        with
        | Some acks -> acks
        | None -> ReplicaSet.empty
      in

      let acks = ReplicaSet.add msg.replica_id acks in
      replica.start_view_change_acks <-
        ViewMap.add msg.view_number acks replica.start_view_change_acks;

      let total_acks = ReplicaSet.cardinal acks in
      if total_acks = quorum replica then assert false)

  let handle_do_view_change (replica : normal replica)
      (message : Op.operation Message.do_view_change) =
    assert false

  let handle_normal_message (replica : normal replica)
      (message : Op.operation Message.message) =
    match message with
    | Request req -> handle_request replica req
    | Prepare prep -> handle_prepare replica prep
    | PrepareOk prep_ok -> handle_prepare_ok replica prep_ok
    | Commit commit -> handle_commit replica commit
    | StartViewChange svc -> handle_start_view_change replica svc
    | DoViewChange dvc -> handle_do_view_change replica dvc

  let handle_view_change_message (replica : view_change replica)
      (message : Op.operation Message.message) =
    match message with _ -> assert false

  let handle_recovery_message (replica : recovering replica)
      (message : Op.operation Message.message) =
    assert false

  let handle_message : type s. s replica -> Op.operation Message.message -> unit
      =
   fun replica message ->
    match replica.state with
    | Normal -> handle_normal_message replica message
    | ViewChange -> handle_view_change_message replica message
    | Recovering -> handle_recovery_message replica message
end
