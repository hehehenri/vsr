open Types

type state =
  | Normal
  | ViewChange 
  | Recovering

module type OperationExecutor = sig
  type operation
  type result

  val execute : operation -> result
end

module Make (N : Network.S) (Op : OperationExecutor) = struct
  type replica = {
    id : ReplicaId.t;
    config : ReplicaId.t list;
    mutable state : state;
    mutable view_number : ViewNumber.t;
    mutable op_number : OpNumber.t;
    mutable commit_number : OpNumber.t;
    mutable request_log : Op.operation OpMap.t;
    mutable client_map : Types.client_map;
    mutable prepare_ok_acks : ReplicaSet.t OpMap.t;
    mutable start_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable do_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable recovery_nonce : int option;
    mutable recovery_responses : ReplicaSet.t ViewMap.t;
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
      do_view_change_acks = ViewMap.empty;
      recovery_nonce = None;
      recovery_responses = ViewMap.empty;
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

  let next_primary replica =
    let current_primary = get_primary_id replica in
    ReplicaId.succ current_primary

  let is_backup replica = not @@ is_primary replica
  let is_normal replica = replica.state = Normal
  let is_view_changing replica = replica.state = ViewChange
  let is_recovering replica = replica.state = Recovering

  let quorum replica =
    let n = List.length replica.config in
    (n / 2) + 1

  let commit replica (op : OpNumber.t) =
    match OpMap.find_opt op replica.request_log with
    | Some op -> Op.execute op
    | None -> failwith "op not found in log"

  let handle_request replica
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

  let handle_prepare replica
      (prep : Op.operation Message.prepare) =
    assert (is_backup replica);
    assert (is_normal replica);
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

  let handle_prepare_ok replica
      (prep_ok : Message.prepare_ok) =
    assert (is_primary replica);
    assert (is_normal replica);
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

  let handle_commit replica (message : Message.commit) =
    assert (is_backup replica);
    assert (is_normal replica);
    OpMap.find_opt message.commit_number replica.request_log
    |> Option.iter (fun op -> ignore @@ Op.execute op)

  let handle_start_view_change replica
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
      if total_acks = quorum replica then
        let next_primary = next_primary replica in
        let do_view_change =
          Message.DoViewChange
            {
              view_number = ViewNumber.succ replica.view_number;
              log = replica.request_log;
              last_view_number = replica.view_number;
              op_number = replica.op_number;
              commit_number = replica.commit_number;
            }
        in
        N.send replica.network (Single next_primary) do_view_change)

  let handle_do_view_change (replica : replica)
      (message : Op.operation Message.do_view_change) =
      assert (is_normal replica);
    assert (is_primary replica);
    assert false

  let handle_recovery (replica : replica) (msg : Message.recovery) =
    assert (is_normal replica);
    let response = 
      Message.RecoveryResponse {
        replica_id = replica.id;
        view_number = replica.view_number;
        nonce = msg.nonce;
        log = if is_primary replica then Some replica.request_log else None;
        op_number = if is_primary replica then Some replica.op_number else None;
        commit_number = if is_primary replica then Some replica.commit_number else None;
      }
    in
    N.send replica.network (Single msg.replica_id) response

  let start_recovery (replica : replica) =
    assert (is_recovering replica);
    let nonce = int_of_float (Unix.time()) in
    let recovery_msg = Message.Recovery {
      replica_id = replica.id;
      nonce = nonce;
    } in
    replica.recovery_nonce <- Some nonce;
    replica.recovery_responses <- ViewMap.empty;
    N.broadcast replica.network recovery_msg

  let handle_recovery_response (replica : replica) (msg : Op.operation Message.recovery_response) =
    assert (is_recovering replica);
    match replica.recovery_nonce with
    | None -> ()
    | Some nonce when nonce <> msg.nonce -> ()
    | Some _ ->
        let responses = match ViewMap.find_opt msg.view_number replica.recovery_responses with
          | None -> ReplicaSet.empty
          | Some s -> s
        in
        let responses = ReplicaSet.add msg.replica_id responses in
        replica.recovery_responses <- ViewMap.add msg.view_number responses replica.recovery_responses;
        
        let latest_view = ViewMap.max_binding_opt replica.recovery_responses
          |> Option.map fst
          |> Option.value ~default:replica.view_number
        in
        let responses_for_view = ViewMap.find_opt latest_view replica.recovery_responses
          |> Option.value ~default:ReplicaSet.empty
        in
        if ReplicaSet.cardinal responses_for_view >= quorum replica then begin
          let primary_id = get_primary_id { replica with view_number = latest_view } in
          match msg.log, msg.op_number, msg.commit_number with
          | Some log, Some op_number, Some commit_number when msg.replica_id = primary_id ->
            replica.view_number <- msg.view_number;
            replica.op_number <- op_number;
            replica.commit_number <- commit_number;
            replica.request_log <- log;
            replica.state <- Normal;
            replica.recovery_nonce <- None;
            replica.recovery_responses <- ViewMap.empty;
            ()
          | _ -> ()
        end

  let handle_message replica (message : Op.operation Message.message) =
    match replica.state, message with
    (* Normal state handling *)
    | Normal, Request req -> handle_request replica req
    | Normal, Prepare prep -> handle_prepare replica prep
    | Normal, PrepareOk prep_ok -> handle_prepare_ok replica prep_ok
    | Normal, Commit commit -> handle_commit replica commit
    | Normal, StartViewChange svc -> handle_start_view_change replica svc
    | Normal, DoViewChange dvc -> handle_do_view_change replica dvc
    | Normal, Recovery recovery -> handle_recovery replica recovery
    | Normal, RecoveryResponse _ -> () (* Ignore recovery responses in normal state *)
    
    (* View Change state handling *)
    | ViewChange, StartViewChange svc -> handle_start_view_change replica svc
    | ViewChange, DoViewChange dvc -> handle_do_view_change replica dvc
    | ViewChange, Recovery recovery -> handle_recovery replica recovery
    | ViewChange, RecoveryResponse _ -> () (* Ignore recovery responses in view change *)
    | ViewChange, (Request _ | Prepare _ | PrepareOk _ | Commit _) -> 
        () (* Ignore normal operation messages during view change *)
    
    (* Recovery state handling *)
    | Recovering, Recovery recovery -> handle_recovery replica recovery
    | Recovering, RecoveryResponse resp -> handle_recovery_response replica resp
    | Recovering, (Request _ | Prepare _ | PrepareOk _ | Commit _ | StartViewChange _ | DoViewChange _) ->
        () (* Ignore all other messages during recovery *)

end
